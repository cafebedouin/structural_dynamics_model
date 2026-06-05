% ============================================================================
% CONSTRAINT STORY: gs1_gln_identification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gs1_gln_identification, []).

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
 *   constraint_id: gs1_gln_identification
 *   human_readable: Global Location Number (GLN) Standard
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The Global Location Number (GLN) standard, established by GS1 (formerly
 *   EAN International) in 1974, represents a globally dominant identification
 *   system for physical locations and legal entities. It solves a genuine
 *   coordination problem: standardized identification enables
 *   interoperability across supply chains, reduces data entry errors, and
 *   allows automated inventory management across jurisdictions. However, the
 *   standard has evolved into a hybrid coordination-extraction mechanism. GS1
 *   maintains monopoly control over the standard's governance, sets licensing
 *   fees unilaterally, mandates data collection for regulatory compliance,
 *   and captures value from supply chain data without proportional benefit to
 *   suppliers. Small retailers, independent suppliers, and developing market
 *   participants face mandatory adoption with limited exit options. Large
 *   logistics operators and healthcare systems benefit from the coordination
 *   function at scale and can absorb licensing costs. The constraint exhibits
 *   all three hybrid phases: (1) genuine coordination problem solving
 *   (1974-1995), (2) layered extraction through licensing and data collection
 *   (1995-2015), and (3) institutional degradation where governance theater
 *   (complex renewal bureaucracy, non-transparent standard-setting)
 *   increasingly sustains the system despite viable alternatives
 *   (blockchain-based identification, open serialization).
 *
 * KEY AGENTS:
 *   - GS1 Organization: Primary beneficiary (institutional/arbitrage) — maintains monopoly control, sets licensing fees, captures supply chain data value
 *   - Large Logistics Operators: Beneficiary (institutional/arbitrage) — achieve scale economies from standardization, absorb licensing costs, leverage data insights
 *   - Healthcare Systems: Beneficiary (institutional/arbitrage) — optimize inventory management, ensure regulatory compliance, minimal friction from licensing
 *   - Small Retailers: Primary victim (powerless/trapped) — mandatory adoption, cannot absorb licensing costs, no meaningful exit option
 *   - Independent Suppliers: Secondary victim (moderate/constrained) — trapped by supply chain requirements, extract value constraints through repeated licensing
 *   - Supply Chain Coalition: Organized actors (organized/constrained) — see coordination benefit but also experience extraction through GS1 monopoly
 *   - Developing Market Participants: Victims (powerless/constrained) — face gatekeeping by developed-market retailers demanding GLN compliance
 *   - Analytical Observer: Sees path-dependent lock-in (analytical/analytical) — recognizes alternative standards could provide coordination at lower extraction cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gs1_gln_identification, 0.38).
domain_priors:suppression_score(gs1_gln_identification, 0.52).
domain_priors:theater_ratio(gs1_gln_identification, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gs1_gln_identification, extractiveness, 0.38).
narrative_ontology:constraint_metric(gs1_gln_identification, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gs1_gln_identification, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gs1_gln_identification, tangled_rope).
narrative_ontology:human_readable(gs1_gln_identification, "Global Location Number (GLN) Standard").
narrative_ontology:topic_domain(gs1_gln_identification, "technological/economic").

domain_priors:requires_active_enforcement(gs1_gln_identification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gs1_gln_identification, gs1_organization).
narrative_ontology:constraint_beneficiary(gs1_gln_identification, large_logistics_operators).
narrative_ontology:constraint_beneficiary(gs1_gln_identification, healthcare_systems).
narrative_ontology:constraint_victim(gs1_gln_identification, small_retailers).
narrative_ontology:constraint_victim(gs1_gln_identification, independent_suppliers).
narrative_ontology:constraint_victim(gs1_gln_identification, developing_market_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL RETAILER (SNARE) — Trapped by supply chain requirements. Major distributors and retailers mandate GLN assignment for all suppliers. No alternative identification exists for supply chain integration. Lacks resources to absorb GLN licensing costs or navigate GS1 bureaucracy. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(gs1_gln_identification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT SUPPLIER (TANGLED ROPE) — Constrained by dual coordination/extraction: benefits from access to standardized supply chains (coordination function), but extraction occurs through licensing fees, mandatory renewal cycles, and data collection requirements. Cannot exit without losing market access. d≈0.72, f(d)≈1.08, σ=1.0 → χ≈0.41.
constraint_indexing:constraint_classification(gs1_gln_identification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE LOGISTICS OPERATOR (ROPE) — Experiences GLN as pure coordination mechanism. Benefits from standardized identification enabling interoperability across global supply chains, automated warehouse management, and reduced transaction costs. Has scale to absorb licensing costs and leverage data insights. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.03. Net beneficiary.
constraint_indexing:constraint_classification(gs1_gln_identification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HEALTHCARE SYSTEM (ROPE) — Large healthcare networks benefit from GLN for facility identification and pharmaceutical tracking, enabling inventory optimization and regulatory compliance with minimal friction. Can negotiate volume pricing and leverage GS1 data services. d≈0.10, f(d)≈-0.07, σ=1.2 → χ≈-0.03. Net beneficiary.
constraint_indexing:constraint_classification(gs1_gln_identification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SUPPLY CHAIN COORDINATION COALITION (TANGLED ROPE) — Organized retailers, distributors, and regulators see GLN as enabling critical coordination infrastructure (interoperability, regulatory compliance, data sharing). However, coalition members also experience extraction through GS1's monopoly position: licensing mandatory, data collection mandatory, governance non-transparent. d≈0.48, f(d)≈0.62, σ=1.2 → χ≈0.29.
constraint_indexing:constraint_classification(gs1_gln_identification, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: GS1 ORGANIZATION (PITON) — Maintains dominant position through institutional inertia and switching costs rather than functional necessity. Alternative identification schemes exist (EPC, direct serialization) but lack critical mass adoption. Theater manifests in mandatory certification processes, complex renewal bureaucracy, and data collection justifications that serve organizational sustenance more than functional coordination. theater_ratio=0.48 appears low, but GS1's governance structure — non-transparent standard-setting, self-dealing licensing fees, data monopoly without proportional ecosystem benefit — reflects degradation of the original coordination function. d≈0.12, f(d)≈-0.05, σ=1.2 → χ≈-0.02.
constraint_indexing:constraint_classification(gs1_gln_identification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scope, GLN represents a hybrid: genuine coordination (interoperability solved), but with locked-in extraction (monopoly licensing, mandatory participation, data collection). The standard's path-dependent adoption creates switching costs that sustain extraction even as alternatives become technically viable. Decentralized identification (blockchain-based) could replace GLN's coordination function at lower extraction cost, but GS1's institutional position prevents migration. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(gs1_gln_identification, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gs1_gln_identification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gs1_gln_identification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gs1_gln_identification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(gs1_gln_identification, TR),
    TR >= 0.70.

:- end_tests(gs1_gln_identification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The GLN standard genuinely solves coordination problems (interoperability, data standardization, regulatory integration), which justifies some licensing cost. However, GS1's monopoly position enables extraction beyond cost recovery: mandatory data collection, unilateral fee-setting, and non-transparent governance. The extractiveness reflects that small suppliers bear disproportionate costs (licensing fees as percentage of transaction volume) while GS1 captures data value. The metric increased from 0.15 (1974-1990s) to 0.38 (2020s) as data monetization became GS1's primary revenue strategy. Suppression (0.52): Moderate-high. Suppliers cannot exit GLN without losing market access. Alternative identification schemes exist but lack critical-mass adoption due to path dependency. Switching costs are prohibitively high for supply chains with millions of locations already in the GLN database. For developing market suppliers, coercive gatekeeping by developed-market retailers functions as effective suppression. Theater ratio (0.48): Moderate. GS1's governance structure includes substantial performative elements: complex renewal procedures, elaborate certification processes, and data-collection justifications that serve organizational sustenance more than functional coordination. However, the core technical standard remains functional (legitimate reduction in data errors, genuine interoperability gains), so theater is not dominant.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits stark perspectival divergence. Large logistics operators and healthcare systems see Rope: pure coordination enabling interoperability and data standardization with manageable licensing costs. Small retailers see Snare: mandatory participation without coordination benefit (they do not benefit from supply chain data insights), trapped by supply chain requirements. The supply chain coalition sees Tangled Rope: genuine coordination value but also extraction through monopoly licensing and data collection. GS1 organization itself sees Piton: its original coordination function persists through institutional momentum, but alternatives (decentralized identification, blockchain serialization) could provide equivalent coordination with lower overhead. The analytical observer sees path-dependent lock-in: the standard solved a real problem in 1974, but switching costs now sustain extraction even as technical alternatives emerge. This perspectival gap is not measurement-dependent (same ε for all observers); it reflects real structural differences in how agents experience the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   GS1 Organization: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Institutional actor with exit freedom (can set standards unilaterally, capture data value). Net beneficiary. Large Logistics: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Scale economies enable absorption of licensing costs, data insights valuable. Net beneficiary. Small Retailers: Victim + trapped → d≈0.92, f(d)≈1.38. No exit option, extraction through mandatory licensing and data collection. Maximum extraction directionality. Independent Suppliers: Victim + constrained → d≈0.72, f(d)≈1.08. Mandatory participation but potential to negotiate volume pricing (slightly more mobile than powerless retailers). Significant extraction but not maximal. Supply Chain Coalition: Organized + constrained → d≈0.48, f(d)≈0.62. Coalition has agency (can lobby for standard changes) but constrained by path dependency (cannot exit without massive coordination failure). Moderate extraction. Healthcare Systems: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.07. Large systems can negotiate pricing and leverage regulatory compliance alignment. Net beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint resolves mandatrophy by differentiating coordination function from extraction mechanism. The genuine coordination problem (1974-1995) was Rope: interoperability, data standardization, transaction cost reduction. As GS1 accumulated monopoly position, extraction layered onto coordination: mandatory data collection (1995-2010), unilateral fee increases (2010-present), and non-transparent governance (2015-present). The constraint became Tangled Rope as soon as extraction exceeded cost recovery. For powerless agents (small retailers, developing market suppliers), it functions as Snare: mandatory participation without coordination benefit. For large institutional agents, it remains Rope: they capture value from the coordination function. The mandatrophy is resolved by observing that GS1 is NOT a pure coordination mechanism — it exhibits all the characteristics of asymmetric extraction (high suppression=0.52, unequal benefit distribution, monopoly governance) layered onto a real coordination function. The theater ratio (0.48) reflects that GS1's governance theater has grown as the coordination function degraded relative to data extraction (theater increased from 0.32 to 0.48 over 50 years). This is a textbook Tangled Rope: coordination function genuine but now secondary to extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_threshold,
    'How much of GLN''s adoption is driven by genuine coordination necessity versus lock-in and switching costs?',
    'Comparative analysis of supply chain efficiency gains from standardization vs. gains from alternative identification schemes (EPC, direct serialization); measuring switching cost barriers vs. functional efficiency delta',
    'If coordination necessity > 70%: GLN is Rope from most perspectives. If < 50%: GLN is Snare with coordination theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_threshold, empirical, 'What portion of GLN adoption reflects coordination necessity versus lock-in').

omega_variable(
    data_collection_extraction_magnitude,
    'What proportion of GS1''s revenue and value extraction derives from GLN licensing versus data monetization and analytics services?',
    'Financial analysis of GS1 revenue streams; tracking of data product pricing and adoption rates; comparative analysis of licensing fees versus data services value captured',
    'If data extraction > 40% of revenue: extraction component dominates, shifting classification toward pure Snare. If < 20%: licensing is primarily cost recovery, supporting Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_collection_extraction_magnitude, empirical, 'Proportion of GS1 extraction from data versus licensing').

omega_variable(
    alternative_standard_viability,
    'Could decentralized or open-source identification standards (blockchain-based GLN equivalents, open serialization protocols) provide equivalent coordination at lower extraction cost?',
    'Technical feasibility assessment of alternative standards; cost-benefit analysis of switching from GLN to alternatives; pilot deployments and interoperability testing',
    'If alternatives viable: suppression becomes voluntary (exit becomes mobile), shifting GLN toward Tangled Rope with sunset logic. If not viable: suppression is structural, supporting Snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_standard_viability, empirical, 'Whether alternative identification standards could replace GLN').

omega_variable(
    developing_market_coercion_degree,
    'In developing markets (Africa, Southeast Asia, South America), is GLN adoption driven by genuine supply chain participation or by coercive market gatekeeping by developed-market retailers?',
    'Survey of supplier adoption pathways in developing markets; analysis of barriers faced by suppliers without GLN; comparison of adoption rates driven by competitive necessity vs. regulatory requirement',
    'If coercive gatekeeping > 60%: GLN functions as neocolonial extraction mechanism (pure Snare for developing market participants). If < 40%: adoption is voluntary coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_market_coercion_degree, empirical, 'Degree of coercive gatekeeping in developing market GLN adoption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gs1_gln_identification, 1974, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gln_tr_t0, gs1_gln_identification, theater_ratio, 0, 0.32).
narrative_ontology:measurement(gln_tr_t15, gs1_gln_identification, theater_ratio, 15, 0.4).
narrative_ontology:measurement(gln_tr_t30, gs1_gln_identification, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(gln_be_t0, gs1_gln_identification, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(gln_be_t15, gs1_gln_identification, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(gln_be_t30, gs1_gln_identification, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gs1_gln_identification, information_standard).
narrative_ontology:affects_constraint(gs1_gln_identification, supply_chain_transparency_audit).
narrative_ontology:affects_constraint(gs1_gln_identification, barcode_standardization_gatekeeping).
narrative_ontology:affects_constraint(gs1_gln_identification, data_monetization_asymmetry).

% DUAL FORMULATION NOTE:
% The GLN standard is downstream of the original supply chain coordination problem (standardized identification) but should be distinguished from the broader GS1 ecosystem (which includes barcodes, RFID, and data services). A separate constraint story could address GS1's data collection and monetization practices as a structurally distinct extraction mechanism (ε≈0.55) layered onto the GLN identification function. The current story focuses on the identification standard itself (ε=0.38) as a hybrid coordination-extraction system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gs1_gln_identification, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
