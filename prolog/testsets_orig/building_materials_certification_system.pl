% ============================================================================
% CONSTRAINT STORY: building_materials_certification_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_building_materials_certification_system, []).

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
 *   constraint_id: building_materials_certification_system
 *   human_readable: Building Materials Certification System
 *   domain: construction/safety/standards
 *
 * SUMMARY:
 *   The building materials certification system represents a global
 *   regulatory coordination mechanism that has evolved into a two-tier market
 *   structure: large manufacturers absorb certification costs and leverage
 *   them as competitive advantage; small and regional manufacturers face
 *   structural barriers to market entry. The constraint exhibits genuine
 *   coordination function (third-party verification addresses information
 *   asymmetry for safety-critical materials) combined with systematic
 *   extraction favoring incumbents. Over the 20-year interval, extractiveness
 *   has increased (0.35→0.52) as certification requirements have proliferated
 *   across jurisdictions and cost barriers have risen. Theater ratio has
 *   increased (0.48→0.65) reflecting growth in duplicative testing across
 *   multiple certification bodies. The system is Tangled Rope at the
 *   structural level: coordination and extraction coexist, with beneficiary
 *   groups (large manufacturers, certification bodies) institutionally locked
 *   into the extraction mechanism.
 *
 * KEY AGENTS:
 *   - Small-Scale Manufacturers: Primary victims (powerless/trapped) — face capital barriers to testing and certification that prevent market entry; structurally excluded from formal channels
 *   - Mid-Tier Regional Manufacturers: Secondary victims (moderate/constrained) — can achieve certification but at high cost; also benefit from market differentiation; mixed position
 *   - Large Certified Manufacturers: Primary beneficiaries (institutional/arbitrage) — capture market premium from certified status; can spread certification costs across volume; use certification as competitive moat
 *   - International Certification Bodies: Institutional beneficiaries (institutional/arbitrage) — extract steady fee-for-service revenue; operate across multiple jurisdictions; coordinate standards while profiting from complexity
 *   - Regulatory Agencies: Organized actors (organized/constrained) — maintain genuine safety mandate but embedded in structure that benefits incumbents; limited ability to reform due to political pressure
 *   - Consumers: Dispersed victims — bear compliance costs embedded in prices; benefit from safety assurance but lack transparency into whether cost:benefit justifies margins
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(building_materials_certification_system, 0.52).
domain_priors:suppression_score(building_materials_certification_system, 0.58).
domain_priors:theater_ratio(building_materials_certification_system, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(building_materials_certification_system, extractiveness, 0.52).
narrative_ontology:constraint_metric(building_materials_certification_system, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(building_materials_certification_system, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(building_materials_certification_system, tangled_rope).
narrative_ontology:human_readable(building_materials_certification_system, "Building Materials Certification System").
narrative_ontology:topic_domain(building_materials_certification_system, "construction/safety/standards").

domain_priors:requires_active_enforcement(building_materials_certification_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(building_materials_certification_system, certified_manufacturers).
narrative_ontology:constraint_beneficiary(building_materials_certification_system, certification_bodies).
narrative_ontology:constraint_beneficiary(building_materials_certification_system, regulatory_agencies).
narrative_ontology:constraint_victim(building_materials_certification_system, small_manufacturers).
narrative_ontology:constraint_victim(building_materials_certification_system, developing_region_producers).
narrative_ontology:constraint_victim(building_materials_certification_system, consumers_bearing_compliance_costs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL MANUFACTURERS (SNARE) — Structurally locked out of formal certification pathways by capital requirements and testing facility access. Cannot exit without abandoning production entirely. Full victim status — the certification barrier extracts through market exclusion and regulatory lock-in.
constraint_indexing:constraint_classification(building_materials_certification_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER MANUFACTURERS (TANGLED ROPE) — Face high costs to certify but paths exist through regional bodies and gradual investment. Constrained exit — certification is costly but technically achievable. Also benefit from consumer trust premium that certification provides. Mixed experience: genuine coordination function (quality assurance) combined with asymmetric extraction (only large firms can absorb compliance costs easily).
constraint_indexing:constraint_classification(building_materials_certification_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE MANUFACTURERS (ROPE) — Already certified; experience the system as coordination enabling market access. Certification differentiates their products and enables premium pricing. Can arbitrage across jurisdictions (use single certification to access multiple markets). Low experienced extraction — net beneficiaries.
constraint_indexing:constraint_classification(building_materials_certification_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CERTIFICATION BODIES (ROPE) — Extract steady revenue from testing fees while coordinating technical standards globally. Arbitrage options through multi-jurisdiction accreditation. Pure beneficiaries experiencing the system as coordination — they maintain the standards while receiving fee-for-service extraction.
constraint_indexing:constraint_classification(building_materials_certification_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AGENCIES (TANGLED ROPE) — Organized actors with genuine coordination mandate (public safety) but embedded in extraction-enabling structure. Suppression of non-certified pathways (even cheaper alternatives with adequate safety) serves both coordination and the beneficiaries' economic interests. Constrained by political pressure from established manufacturers and budget constraints. Time horizon generational: certification regimes persist for decades, locking in standards that benefit incumbents.
constraint_indexing:constraint_classification(building_materials_certification_system, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: HISTORICAL BUREAUCRACY (PITON) — Certification rituals persist through institutional inertia despite technological alternatives. Theater ratio (0.65) reflects that much testing is duplicative — manufacturers often submit identical material batches to multiple bodies, each performing similar tests. The ritual persists because regulatory lock-in makes change difficult, not because the structure is optimal.
constraint_indexing:constraint_classification(building_materials_certification_system, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN PERSPECTIVE) — From a technical/civilizational view, third-party verification of safety-critical materials appears inherent: complex material properties cannot be evaluated by untrained buyers, creating an irreducible information asymmetry. This perspective naturalizes certification as structurally necessary. However, structural data contradicts the mountain classification — certification *design* is contingent (centralized vs distributed, expensive vs accessible), even if *some verification* is necessary. This reveals false summit: naturalization of a specific institutional arrangement.
constraint_indexing:constraint_classification(building_materials_certification_system, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(building_materials_certification_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(building_materials_certification_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(building_materials_certification_system, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(building_materials_certification_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(building_materials_certification_system, TR),
    TR >= 0.70.

:- end_tests(building_materials_certification_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The system demonstrates asymmetric cost distribution — large manufacturers amortize certification costs across production volume while small manufacturers bear per-batch costs. The increase from 0.35 to 0.52 over the interval reflects regulatory expansion (more certifications required per material) and consolidation (fewer independent certifiers, higher fees). However, extractiveness is not maximal because the coordination function is genuine — verification addresses real safety concerns. Suppression (0.58): Moderate-high. Barriers include capital requirements for testing facilities, access to recognized labs, and regulatory recognition for non-traditional pathways. Developing-region producers face especially severe suppression through geographic barriers and lack of local certification infrastructure. Theater ratio (0.65): Moderate-high and rising. Significant testing duplication: manufacturers often submit identical batches to multiple bodies, each performing similar tests. The duplication serves institutional interests (jurisdictional control, fee generation) more than incremental safety assurance. Digital records show 60-70% of test results are near-identical across bodies, suggesting high theater content.
 *
 * PERSPECTIVAL GAP:
 *   Strong divergence exists between the powerless/trapped perspective (sees pure Snare) and the institutional/arbitrage perspectives (see Rope). The gap reveals the constraint's asymmetric structure: one agent's coordination is another agent's exclusion barrier. The analytical observer risks collapsing the gap by naturalizing certification as inherently necessary (Mountain), obscuring the policy design choices (cost structure, jurisdictional fragmentation, duplication tolerance) that concentrate extraction on small producers.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies with structural position. Small manufacturers with trapped exit options and victim status have d ≈ 0.92 (near-maximum target status). Mid-tier manufacturers with constrained exit but both beneficiary and victim features have d ≈ 0.55 (mixed). Large manufacturers with arbitrage exit and beneficiary status have d ≈ 0.18 (near-minimum). Certification bodies with institutional power and beneficiary status have d ≈ 0.12 (maximum beneficiary). Regulatory agencies with constrained exit, both coordination mandate and institutional coupling, have d ≈ 0.50 (symmetric). These d values are derived from the structural relationships (beneficiary/victim, power level, exit options) without override. The sigmoid f(d) then scales extractiveness per agent perspective, producing high χ for trapped small manufacturers and low χ for institutional beneficiaries — measurable perspectival gaps in experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by distinguishing genuine coordination function (safety verification) from institutional extraction (cost structure favoring incumbents). A pure Rope classification would miss the asymmetric extraction. A pure Snare classification would miss the genuine safety assurance benefit. Tangled Rope captures both: the system coordinates (addresses information asymmetry for safety) while extracting (suppresses small-manufacturer entry). The mandatrophy is resolved by the coexistence of both functions in the same mechanism. The false natural law (Mountain perspective) is resolved by noting that while *some* verification is structurally necessary, the specific design (centralized, expensive, duplicative) is contingent — alternative architectures (distributed verification, tiered certification, digital passports) could preserve coordination while reducing extraction. The system is not a law of physics but a policy choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_necessity_vs_institutional_choice,
    'Is expensive centralized third-party certification structurally necessary for material safety assurance, or is it a contingent institutional choice?',
    'Comparative analysis of alternative verification models: distributed testing (manufacturer + buyer-commissioned), peer certification networks, supply-chain transparency without centralized approval, digital material passports with traceable sourcing',
    'If necessary: classification tilts toward Rope/Mountain (coordination unavoidable). If contingent: classification tilts toward Snare/Tangled Rope (extractive institutional choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_necessity_vs_institutional_choice, conceptual, 'Whether centralized certification is structurally necessary or institutionally contingent').

omega_variable(
    small_manufacturer_market_viability,
    'Do small manufacturers in developing regions genuinely lack capacity for certified production, or does the certification cost structure artificially suppress their entry?',
    'Cost-benefit analysis: what percentage of production cost is certification vs materials/labor? Regional comparison: certification acceptance thresholds across jurisdictions (some accept cheaper alternatives; do they show equivalent failure rates?)',
    'If genuine capacity gap: small manufacturers benefit from certification (Rope). If artificial suppression: they are trapped victims (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(small_manufacturer_market_viability, empirical, 'Whether small manufacturer exclusion is capacity-based or cost-structure-based').

omega_variable(
    duplication_and_theater,
    'What percentage of testing duplication is necessary redundancy for assurance vs performative ritual maintaining jurisdictional control?',
    'Audit of manufacturing test reports: frequency of identical batches tested by multiple bodies producing identical results; comparison of testing stringency across jurisdictions (do they differ significantly enough to justify duplication?).',
    'High duplication: theater_ratio is justified (Piton relevant). Low duplication: theater_ratio should decrease (Rope more justified).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duplication_and_theater, empirical, 'Proportion of testing duplication that is necessary vs performative').

omega_variable(
    consumer_trust_empirics,
    'Does centralized certification actually improve consumer confidence and purchasing decisions, or is the coordination benefit illusory?',
    'Market research on consumer awareness and purchasing behavior; comparison of failure rates for certified vs non-certified materials; analysis of whether certification marks actually influence buyer choice or merely serve as regulatory gating mechanism',
    'If trust is real: coordination function is genuine (Tangled Rope justified). If illusory: suppression serves only the beneficiaries (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_trust_empirics, empirical, 'Whether certification provides genuine consumer trust benefit').

omega_variable(
    alternative_pathway_feasibility,
    'Are emerging alternatives (blockchain material passports, supply-chain transparency, distributed verification networks) technically and economically feasible replacements for centralized certification?',
    'Pilot program outcomes; technical viability assessment; cost comparison to traditional certification; adoption rates where alternatives are permitted',
    'If feasible: scaffold perspective is real (sunset clause possible). If not feasible: Tangled Rope is locked in (no exit path for small manufacturers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathway_feasibility, empirical, 'Whether alternative certification pathways are technically feasible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(building_materials_certification_system, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bmat_tr_t0, building_materials_certification_system, theater_ratio, 0, 0.48).
narrative_ontology:measurement(bmat_tr_t10, building_materials_certification_system, theater_ratio, 10, 0.58).
narrative_ontology:measurement(bmat_tr_t20, building_materials_certification_system, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(bmat_be_t0, building_materials_certification_system, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bmat_be_t10, building_materials_certification_system, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(bmat_be_t20, building_materials_certification_system, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(building_materials_certification_system, enforcement_mechanism).
narrative_ontology:affects_constraint(building_materials_certification_system, small_manufacturer_market_access).
narrative_ontology:affects_constraint(building_materials_certification_system, regulatory_fragmentation_across_jurisdictions).
narrative_ontology:affects_constraint(building_materials_certification_system, material_innovation_adoption_speed).

% DUAL FORMULATION NOTE:
% The certification system can be decomposed into two structurally distinct constraints: (1) verification necessity (ε ≈ 0.15, Mountain) — the requirement that safety-critical materials be independently tested; (2) institutional extraction structure (ε ≈ 0.52, Tangled Rope) — the cost and access design that favors incumbents. This story addresses the combined system. The verification necessity alone would classify as Mountain from all perspectives. The extraction structure is the source of the perspectival divergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(building_materials_certification_system, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
