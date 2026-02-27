% ============================================================================
% CONSTRAINT STORY: eu_deforestation_regulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_deforestation_regulation, []).

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
 *   constraint_id: eu_deforestation_regulation
 *   human_readable: EU Deforestation Regulation (EUDR)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The EU Deforestation Regulation (EUDR) prohibits the import of
 *   commodities linked to deforestation, creating a transnational constraint
 *   that operates simultaneously as a conservation mechanism, a trade
 *   protection device, and an extraction system targeting smallholder farmers
 *   and weak-state exporters. The regulation exhibits the full tension of
 *   tangled rope: genuine coordination function (protecting global forest
 *   commons and incentivizing supply-chain transparency), simultaneous
 *   extraction of compliance costs onto those least able to bear them, and
 *   active enforcement through satellite monitoring and liability chains. The
 *   constraint's theater ratio (0.58) reflects that satellite verification
 *   creates the appearance of enforcement without guaranteeing on-ground
 *   conservation outcomes — small-scale clearings evade detection,
 *   reforestation is classified as forest recovery, and informal land
 *   transactions occur off the books. The core mandatrophy is whether EUDR
 *   reduces global deforestation (genuine coordination) or merely displaces
 *   it to non-EU markets (pure extraction with theater). The perspectival gap
 *   between the EU institutional beneficiary and the global smallholder
 *   victim is maximum: same constraint, diametrically opposed
 *   classifications.
 *
 * KEY AGENTS:
 *   - Smallholder Farmers (Developing Nations): Primary victims (powerless/trapped) — lack capital and land documentation to implement compliance systems; excluded from EU market unless costs absorbed by middlemen
 *   - Commodity Exporters (Weak Governance): Secondary victim (moderate/constrained) — state capacity insufficient to verify supply chains; must outsource verification to commodity traders or forgo market access
 *   - EU Agricultural Sector: Primary beneficiary (institutional/arbitrage) — protected from non-compliant commodity imports; captures supply-chain gatekeeping rents
 *   - Commodity Traders and Processors: Organized beneficiary (organized/constrained) — extract margin from verification services; bear compliance and liability costs but pass through to upstream suppliers
 *   - Brazilian/Indonesian Agribusiness: Powerful but constrained (powerful/mobile) — can navigate compliance through scale and capital but experience suppression through market access restrictions
 *   - Certification Bodies (FSC, etc.): Organized beneficiary (organized/constrained) — profit from increased demand for certified products; constrained by reputational liability
 *   - Conservation NGOs and Governance Coalition: Organized agent seeking sunset (organized/mobile) — view EUDR as temporary measure pending forest governance capacity building
 *   - EU Regulatory Apparatus: Institutional actor maintaining ritual (institutional/arbitrage) — implements regulation as expression of commitment to climate and conservation; benefits from legitimacy even if verification gaps persist
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent policy choice (import restrictions) as inevitable response to deforestation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_deforestation_regulation, 0.52).
domain_priors:suppression_score(eu_deforestation_regulation, 0.65).
domain_priors:theater_ratio(eu_deforestation_regulation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_deforestation_regulation, extractiveness, 0.52).
narrative_ontology:constraint_metric(eu_deforestation_regulation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(eu_deforestation_regulation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_deforestation_regulation, tangled_rope).
narrative_ontology:human_readable(eu_deforestation_regulation, "EU Deforestation Regulation (EUDR)").
narrative_ontology:topic_domain(eu_deforestation_regulation, "economic/political").

domain_priors:requires_active_enforcement(eu_deforestation_regulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_deforestation_regulation, tropical_forest_ecosystems).
narrative_ontology:constraint_beneficiary(eu_deforestation_regulation, global_carbon_cycle_stability).
narrative_ontology:constraint_beneficiary(eu_deforestation_regulation, eu_agricultural_competitiveness).
narrative_ontology:constraint_victim(eu_deforestation_regulation, smallholder_farmers_developing_nations).
narrative_ontology:constraint_victim(eu_deforestation_regulation, commodity_exporters_weak_governance).
narrative_ontology:constraint_victim(eu_deforestation_regulation, supply_chain_compliance_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLHOLDER FARMERS (SNARE) — Cannot exit the constraint; lack capital to implement traceability systems or land documentation. Excluded from export markets unless compliance costs are absorbed. Maximum experienced extraction — trapped in informal land tenure systems that make regulatory compliance prohibitively expensive.
constraint_indexing:constraint_classification(eu_deforestation_regulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMODITY EXPORTERS (WEAK GOVERNANCE) (TANGLED ROPE) — Constrained by limited state capacity to verify supply chains and implement enforcement. Benefits nominally from forest protection norms and market access incentives, but bears significant compliance costs. Extraction is asymmetric: EU firms outsource verification burden onto exporting nations; those nations also benefit from access to world's largest commodity market. Mixed extraction and coordination.
constraint_indexing:constraint_classification(eu_deforestation_regulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EU AGRICULTURAL EXPORTERS/IMPORTERS (ROPE) — Experience EUDR as coordination mechanism. Regulation protects EU farmers from competition with non-compliant commodity imports while preserving market access for EU-compliant producers. Net beneficiary; extraction runs toward EU agricultural sector through tariff protection and supply-chain gatekeeping without major cost to them — verification is outsourced.
constraint_indexing:constraint_classification(eu_deforestation_regulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMMODITY TRADERS/PROCESSORS (TANGLED ROPE) — Organized but constrained by verification requirements and liability exposure. Benefit from supply-chain gatekeeping (fewer smaller competitors; higher margins). Bear compliance costs (geolocation tracking, satellite monitoring, due diligence systems). Active enforcement required — regulation creates legal liability if deforestation links materialize. Asymmetric: traders capture margin; upstream farmers and exporters bear verification burden.
constraint_indexing:constraint_classification(eu_deforestation_regulation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: EU REGULATORY APPARATUS (PITON) — EUDR is partly performative: satellite monitoring, geolocation verification, and supply-chain documentation create the appearance of enforcement without guaranteeing on-ground deforestation reduction. Verification relies on satellite imagery and corporate self-reporting, both of which have blind spots (small-scale clearings, planted forests classified as regeneration, under-the-table transactions). Theater ratio high because regulatory compliance does not map cleanly to conservation outcomes. Institution maintains regulation through reputational commitment despite imperfect verification.
constraint_indexing:constraint_classification(eu_deforestation_regulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: BRAZILIAN/INDONESIAN AGRIBUSINESS (TANGLED ROPE) — Powerful multinational producers with capacity to navigate compliance; mobile through ability to certify land retroactively or shift supply chains. Experience extraction (verification costs, reduced export volume) but also coordination benefits (market access, competitive advantage over smaller producers). Suppression high (regulatory pressure, threatened market access) but not total — major exporters can restructure operations.
constraint_indexing:constraint_classification(eu_deforestation_regulation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: CONSERVATION NGOS/GOVERNANCE COALITION (SCAFFOLD) — See EUDR as temporary enforcement mechanism with sunset logic. The constraint exists because forest governance in tropical nations is weak; as governance capacity builds (REDD+, forest monitoring systems, land registries), verification burden shifts from commodity importers to governments. Suppression declines as alternative institutions mature. Theater is moderate because the rule creates incentives for governance investment. Exit path visible: institutionalized forest governance makes import restrictions unnecessary.
constraint_indexing:constraint_classification(eu_deforestation_regulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: CERTIFICATION BODIES (FSC, etc.) (TANGLED ROPE) — Benefit from EUDR enforcement (market demand for certified products rises). Constrained by need to maintain certification integrity while expanding supply. Experience mixed coordination (standard-setting function) and extraction (liability for certified products that later show deforestation links). Suppression moderately high due to reputational risk and legal exposure.
constraint_indexing:constraint_classification(eu_deforestation_regulation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER — UNIVERSAL VIEW (MOUNTAIN-RISK) — From a civilizational perspective, the supply-chain transparency requirement appears as an immutable natural law: any commodity market with weak governance zones will generate deforestation incentives; verification is inherent to supply chains, not contingent. However, the structural data (high theater, significant suppression, asymmetric extraction) suggests this is a false summit — the 'natural' framing naturalizes the choice to enforce through import restrictions rather than through direct governance investment.
constraint_indexing:constraint_classification(eu_deforestation_regulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_deforestation_regulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_deforestation_regulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_deforestation_regulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_deforestation_regulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_deforestation_regulation, TR),
    TR >= 0.70.

:- end_tests(eu_deforestation_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. EUDR imposes compliance costs (geolocation documentation, satellite monitoring, auditing, traceability systems) that are disproportionately borne by smallholders and exporters in weak-governance zones. These costs function as an extraction mechanism — they transfer wealth from commodity producers to compliance vendors and ultimately subsidize EU agricultural competitiveness. However, extractiveness is not maximal (0.52 not 0.70+) because the regulation also provides genuine benefits: market access incentives encourage sustainable practices, and the coordination function (reducing deforestation) has real public-good value. Suppression (0.65): High. Alternatives to EUDR-compliant sourcing are severely constrained — export to non-EU markets exists but at significantly lower prices. Land documentation and certification infrastructure are often inaccessible to smallholders. Informal trade exists but at high risk of reputational and legal liability. Theater ratio (0.58): Moderate-high and rising. Satellite monitoring creates the appearance of enforcement without capturing small-scale deforestation, planted-forest misclassification, or informal land transactions. The theater has increased over time as compliance infrastructure has grown — firms invest heavily in certification and auditing that may not correlate with on-ground forest protection. The rise in theater_ratio from 0.35 to 0.58 reflects institutional drift toward symbolic compliance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint presents a maximum perspectival gap. The EU institutional beneficiary and commodity traders see a coordination mechanism with acceptable compliance overhead (Rope). Smallholder farmers and weak-state exporters see pure extraction with impossible compliance barriers (Snare or high-χ Tangled Rope). Conservation NGOs see a temporary measure addressing a governance gap with a visible sunset (Scaffold). The EU regulatory apparatus sees a maintained ritual of environmental commitment despite imperfect verification (Piton). The analytical observer risks naturalizing the policy choice (import restrictions) as inevitable response to deforestation, when alternative mechanisms (direct governance investment, carbon pricing, forest tenure security) could achieve the coordination function with lower extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position: beneficiaries with arbitrage options (EU agricultural sector, commodity traders) experience low d → low χ. Trapped victims (smallholders, weak-state exporters) experience high d → high χ. Organized agents with some exit capacity experience moderate d → moderate χ. The piton classification arises from high theater despite institutional enforcement — the regulation persists through EU commitment and reputational value, not because verification mechanisms reliably achieve forest protection. The multiple tangled-rope perspectives reflect that different agents perceive both coordination (supply-chain transparency enables market access) and extraction (compliance costs are asymmetrically distributed). The scaffold perspective depends on the assumption that forest governance capacity will mature, making import restrictions unnecessary — this is a genuine structural possibility, but only if governance investment is prioritized over compliance theater.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for EUDR is whether the regulation achieves net global deforestation reduction or merely displaces cutting to non-EU markets. If displacement occurs (omega_traded_deforestation_displacement), the 'coordination' framing is illusory — the regulation is pure extraction with theater, and the classification should shift toward Snare + Piton. If net global reduction occurs and is substantial, the tangled-rope classification holds: genuine coordination function plus asymmetric extraction costs. The resolution mechanism is long-term comparative deforestation analysis across EU-supplying vs. non-EU-supplying regions. Until that data is available, the tangled-rope classification captures the structural ambiguity: the constraint has both coordination and extraction components, and their relative magnitude is empirically uncertain. The scaffold perspective (sunset through governance capacity) is plausible but depends on political commitment to governance investment that is not guaranteed by EUDR itself — the regulation could persist indefinitely as a compliance mechanism without ever triggering its own obsolescence through governance reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    satellite_verification_accuracy,
    'Can satellite monitoring reliably distinguish deforestation from agricultural intensification, reforestation, or natural forest recovery within the 10-hectare resolution threshold?',
    'Ground-truth validation studies comparing satellite classifications with on-the-ground surveys; analysis of false positive and false negative rates by ecosystem type and land-use transition category',
    'If accuracy < 75%: many non-compliant commodities pass through; regulation becomes symbolic. If accuracy > 90%: verification bottleneck shifts from farms to traceability systems; extraction focus changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(satellite_verification_accuracy, empirical, 'Accuracy of satellite-based deforestation detection').

omega_variable(
    smallholder_inclusion_threshold,
    'At what compliance cost do smallholder farmer income gains from deforestation become economically irrational compared to sustainable intensification?',
    'Cost-benefit analysis for smallholders: compliance infrastructure investment vs. lost deforestation revenue; comparison with agricultural intensification subsidies offered by exporting nations',
    'If threshold < $500/hectare: inclusion programs can work; constraint becomes Scaffold. If threshold > $5000/hectare: smallholders remain trapped; constraint remains Snare for this agent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(smallholder_inclusion_threshold, empirical, 'Compliance cost threshold for smallholder participation').

omega_variable(
    forest_governance_capacity_timeline,
    'What is the realistic timeline for tropical nations to build land registries, monitoring systems, and enforcement capacity sufficient to make EUDR-level verification redundant?',
    'Longitudinal assessment of governance capacity indices (land registry completion, environmental agency staffing, satellite monitoring infrastructure) in major commodity-exporting nations; comparison with historical timelines for governance institution-building',
    'If timeline < 10 years: Scaffold sunset is real and plausible. If timeline > 30 years: regulation becomes permanent extraction mechanism; classification should shift toward Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(forest_governance_capacity_timeline, empirical, 'Timeline for governance capacity to replace import verification').

omega_variable(
    traded_deforestation_displacement,
    'Does EUDR reduce tropical forest loss globally, or does it displace deforestation to non-EU markets (China, India, Middle East) with no net forest benefit?',
    'Comparative analysis of deforestation trends pre/post-EUDR in EU-supplying vs. non-EU-supplying regions; attribution analysis for commodity-specific flows (palm oil, beef, soy, timber)',
    'If net reduction > 20%: constraint has genuine coordination function (forest protection). If net reduction < 5%: constraint is mostly Snare and Piton (extraction with theater); forest benefit is illusory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(traded_deforestation_displacement, empirical, 'Whether EUDR achieves net global deforestation reduction').

omega_variable(
    compliance_infrastructure_capture,
    'Will certification and traceability infrastructure (digital tools, auditing firms, remote sensing) become concentrated in a few firms, creating a new compliance monopoly?',
    'Market concentration analysis of certification bodies, blockchain/traceability vendors, and auditing firms 5-10 years post-EUDR; comparison with certification market concentration indices (ISO, FSC) from analogous periods',
    'If captured: extraction mechanism shifts from commodity importers to compliance vendors; theater increases; net victim burden on smallholders worsens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_infrastructure_capture, conceptual, 'Risk of compliance infrastructure monopolization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_deforestation_regulation, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eudr_tr_t0, eu_deforestation_regulation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eudr_tr_t2, eu_deforestation_regulation, theater_ratio, 2, 0.48).
narrative_ontology:measurement(eudr_tr_t5, eu_deforestation_regulation, theater_ratio, 5, 0.58).

% Extraction over time
narrative_ontology:measurement(eudr_be_t0, eu_deforestation_regulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eudr_be_t2, eu_deforestation_regulation, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(eudr_be_t5, eu_deforestation_regulation, base_extractiveness, 5, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_deforestation_regulation, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_deforestation_regulation, tropical_forest_tenure_insecurity).
narrative_ontology:affects_constraint(eu_deforestation_regulation, global_commodity_supply_chain_asymmetry).
narrative_ontology:affects_constraint(eu_deforestation_regulation, carbon_offset_credit_integrity).

% DUAL FORMULATION NOTE:
% EUDR is the downstream enforcement mechanism for two upstream structural constraints: forest tenure insecurity (weak property rights make deforestation rational for marginal agents) and commodity supply-chain asymmetry (price differentials incentivize extraction). Addressing upstream constraints through governance investment and tenure formalization could reduce the need for import restrictions. EUDR's effectiveness depends on whether these upstream constraints are addressed in parallel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_deforestation_regulation, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
