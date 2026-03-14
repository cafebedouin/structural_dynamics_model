% ============================================================================
% CONSTRAINT STORY: pharmaceutical_supply_chain_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pharmaceutical_supply_chain_opacity, []).

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
 *   constraint_id: pharmaceutical_supply_chain_opacity
 *   human_readable: Pharmaceutical Supply Chain Opacity
 *   domain: healthcare/economics/regulatory
 *
 * SUMMARY:
 *   The pharmaceutical supply chain operates under substantial opacity
 *   regarding drug origin, manufacturing conditions, intermediate handlers,
 *   pricing rationale, and quality verification. This opacity is maintained
 *   through institutional arrangements combining genuine coordination
 *   requirements (compartmentalization for efficiency), legitimate business
 *   interests (trade secrecy, intellectual property protection), and
 *   extractive mechanisms (pricing power capture, information asymmetry
 *   exploitation). The constraint exhibits multiple classification types from
 *   different perspectives: patients experience pure extraction (Snare);
 *   healthcare systems experience mixed coordination and extraction (Tangled
 *   Rope); manufacturers experience coordination benefit (Rope); regulatory
 *   frameworks show degraded oversight (Piton); policy interventions
 *   (track-and-trace, serialization) show sunset logic (Scaffold); and
 *   analytical observers risk naturalizing institutional arrangements as
 *   inherent to complex manufacturing (false Mountain). The extractiveness
 *   has increased over the interval (0.35 → 0.58) as supply chains have
 *   globalized and become more complex, concentrating information
 *   asymmetries. Theater ratio has risen (0.52 → 0.68) as regulatory
 *   inspection regimes have become increasingly performative relative to
 *   actual supply chain complexity — inspectors cannot verify real-time
 *   integrity across distributed networks.
 *
 * KEY AGENTS:
 *   - Patients: Primary victims (powerless/trapped) — lack information on drug quality, origin, and pricing; no exit options from pharmaceutical dependence
 *   - Healthcare Systems: Secondary victims (moderate/constrained) — lack supply chain visibility needed for effective negotiation; face disruption risks from opacity-enabled bottlenecks
 *   - Pharmaceutical Manufacturers: Primary beneficiaries (institutional/arbitrage) — capture pricing power and market control through information asymmetry; can arbitrage transparency requirements across jurisdictions
 *   - Supply Chain Intermediaries: Secondary beneficiaries (organized/constrained) — extract margins from opacity; coordinate logistics while maintaining information barriers
 *   - Regulatory Agencies: Ambiguous (institutional/arbitrage) — theoretically enforcing transparency but often complicit in maintaining opacity through regulatory capture or institutional inertia
 *   - Serialization Coalition: Organized reformers (organized/mobile) — building transparency infrastructure (DSCSA, track-and-trace, blockchain pilots) with clear exit pathways
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing engineered opacity as inherent to pharmaceutical complexity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pharmaceutical_supply_chain_opacity, 0.58).
domain_priors:suppression_score(pharmaceutical_supply_chain_opacity, 0.65).
domain_priors:theater_ratio(pharmaceutical_supply_chain_opacity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pharmaceutical_supply_chain_opacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(pharmaceutical_supply_chain_opacity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(pharmaceutical_supply_chain_opacity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pharmaceutical_supply_chain_opacity, tangled_rope).
narrative_ontology:human_readable(pharmaceutical_supply_chain_opacity, "Pharmaceutical Supply Chain Opacity").
narrative_ontology:topic_domain(pharmaceutical_supply_chain_opacity, "healthcare/economics/regulatory").

domain_priors:requires_active_enforcement(pharmaceutical_supply_chain_opacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pharmaceutical_supply_chain_opacity, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(pharmaceutical_supply_chain_opacity, supply_chain_intermediaries).
narrative_ontology:constraint_beneficiary(pharmaceutical_supply_chain_opacity, regulatory_agencies).
narrative_ontology:constraint_victim(pharmaceutical_supply_chain_opacity, patients).
narrative_ontology:constraint_victim(pharmaceutical_supply_chain_opacity, healthcare_systems).
narrative_ontology:constraint_victim(pharmaceutical_supply_chain_opacity, supply_chain_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT (SNARE) — Trapped in opacity regarding drug origin, manufacturing standards, and price rationale. Cannot verify quality, cannot negotiate, cannot exit the healthcare system. Bears full cost of information asymmetry: counterfeit drugs, supply disruptions, inflated prices. No exit options; maximum experienced extraction.
constraint_indexing:constraint_classification(pharmaceutical_supply_chain_opacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HEALTHCARE SYSTEM (TANGLED ROPE) — Constrained by lack of supply chain visibility; must coordinate with manufacturers to ensure drug availability while lacking data to negotiate effectively. Some coordination function (ensuring stable supply) exists alongside asymmetric extraction (manufacturers capture pricing power through information control). High cost to exit — cannot abandon pharmaceutical supply chains.
constraint_indexing:constraint_classification(pharmaceutical_supply_chain_opacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURER (ROPE) — Benefits from opacity as coordination mechanism: compartmentalized supply chains enable efficiency, intellectual property protection, and cost optimization. Experiences constraint as manageable coordination problem with clear beneficiaries (themselves). Can arbitrage between disclosure levels and regulatory jurisdictions.
constraint_indexing:constraint_classification(pharmaceutical_supply_chain_opacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SUPPLY CHAIN INTERMEDIARIES (TANGLED ROPE) — Organized actors (distributors, warehouses, logistics firms) coordinate physical flow of drugs while benefiting from information asymmetry that allows margin extraction. Some coordination is genuine; much is rent extraction enabled by opacity. Can constrain costs but cannot fully exit — dependent on manufacturer relationships.
constraint_indexing:constraint_classification(pharmaceutical_supply_chain_opacity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY COALITION (SCAFFOLD) — Organized effort (FDA, EMA, WHO, serialization mandates) to build transparency infrastructure with sunset logic: track-and-trace systems (DSCSA, REFEDS) are building alternative verification pathways. Opacity seen as temporary institutional gap with real exit mechanisms (e-serialization, blockchain pilots, supply chain transparency regulations). Sunset estimated 10-15 years as digital infrastructure matures.
constraint_indexing:constraint_classification(pharmaceutical_supply_chain_opacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY REGULATORY FRAMEWORK (PITON) — Traditional pharmaceutical regulation (batch approval, lot testing, periodic inspections) is substantially performative: regulators cannot verify supply chain integrity in real time; inspections detect only fraction of compliance gaps. Framework persists through institutional inertia despite known limitations. Theater-heavy: inspection rituals, batch documentation, periodic audits maintain appearance of control while actual supply chain visibility remains low. Theater ratio driven by gap between inspection frequency and supply chain complexity.
constraint_indexing:constraint_classification(pharmaceutical_supply_chain_opacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CONTINGENCY VIEW (MOUNTAIN) — At civilizational scale, some supply chain opacity appears inherent to complex manufacturing: distributed production, supplier networks, and patent protection create structural friction. Risk of naturalizing contingent institutional arrangements (compartmentalization, trade secrecy, regulatory silos) as immutable laws of manufacturing. However, base properties show this is a false summit — opacity is engineered, maintained, and extractive rather than emergent from physical limits.
constraint_indexing:constraint_classification(pharmaceutical_supply_chain_opacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pharmaceutical_supply_chain_opacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pharmaceutical_supply_chain_opacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pharmaceutical_supply_chain_opacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pharmaceutical_supply_chain_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pharmaceutical_supply_chain_opacity, TR),
    TR >= 0.70.

:- end_tests(pharmaceutical_supply_chain_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The primary source of extraction is pricing power enabled by information asymmetry — patients and healthcare systems cannot verify whether prices reflect legitimate manufacturing costs or monopolistic extraction. Secondary extraction comes from quality verification barriers — opaque supply chains enable counterfeit infiltration (1-10% of drugs in some markets estimated as counterfeit or substandard), creating patient harm that never enters the transparent cost accounting. The value reflects genuine coordination requirements (supply chain compartmentalization does enable efficiency) partially offset by engineered extraction (opacity is maintained at levels beyond what coordination efficiency requires). Suppression (0.65): Moderate-high. Barriers to transparency include: technical complexity (distributed suppliers across multiple countries), legitimate proprietary interests (recipe/process protection), regulatory fragmentation (different transparency requirements by jurisdiction), and active resistance by beneficiaries. These are substantial but not insurmountable — serialization pilots and track-and-trace systems demonstrate feasibility. Theater ratio (0.68): High. Regulatory oversight is substantially performative: FDA inspects ~0.8% of pharmaceutical facilities annually and cannot verify supply chain integrity in real time; lot testing provides backward-looking quality assurance rather than forward-looking supply chain control; batch documentation is theater that does not prevent counterfeiting or substitution. Theater has increased as supply chains have become more complex faster than regulatory capacity has grown.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence from identical structural data. Manufacturers (Rope) see a coordination success: compartmentalized supply chains work efficiently and enable cost control. Patients (Snare) see pure extraction: they cannot verify quality and cannot negotiate price. Regulatory frameworks (Piton) see their own degraded capacity: inspections are theater, real-time verification is impossible at current staffing levels. Transparency coalitions (Scaffold) see a solvable problem with clear exit paths: serialization and track-and-trace infrastructure will mature and replace opacity-based coordination within a decade. Healthcare systems (Tangled Rope) see mixed reality: they need manufacturer coordination for supply stability but experience extraction through asymmetric information on pricing and sourcing. The analytical observer (false Mountain) risks treating opacity as inherent to pharmaceutical complexity rather than as engineered institutional arrangement. The perspectival gaps reveal that classification is not about 'what is the constraint really?' but 'who bears the costs and who captures the benefits?' When these are radically asymmetric (d-spread > 0.70), the constraint is almost certainly Tangled Rope or Snare rather than pure Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by structural position in the extraction flow. Patients (powerless/trapped): d ≈ 0.95, maximum extraction directed at them with no exit. Healthcare systems (moderate/constrained): d ≈ 0.65, moderate extraction with constrained ability to exit or reduce flow. Manufacturers (institutional/arbitrage): d ≈ 0.15, low extraction from their perspective because they control information flow and can arbitrage across jurisdictions. Supply intermediaries (organized/constrained): d ≈ 0.55, moderate extraction but with some agency to negotiate and optimize. Regulatory agencies (institutional/arbitrage): d ambiguous — if captured, d ≈ 0.20; if institutionally inert, d ≈ 0.55 (they are targets of reform pressure). The serialization coalition (organized/mobile): d ≈ 0.40, moderate extraction but with real exit pathways through transparency infrastructure. The directionality spread (0.15 for manufacturers to 0.95 for patients) is 0.80 — a very high perspectival gap indicating substantial structural asymmetry. This large spread is diagnostic for Tangled Rope classification at the healthcare system level: the constraint simultaneously coordinates supply (genuine function) and enables extraction (asymmetric distribution of benefits/costs).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STATUS: UNRESOLVED. The constraint exhibits the classic mandatrophy structure: pharmaceutical supply chain opacity is simultaneously a coordination mechanism (compartmentalization enables efficiency, enables intellectual property protection, coordinates distributed production) AND an extraction mechanism (pricing power capture, quality verification barriers, counterfeit enablement). At extractiveness 0.58, the classification is Tangled Rope if both coordination AND asymmetric extraction are present and enforced. The JSON declares both beneficiaries (manufacturers, intermediaries) and victims (patients, healthcare systems), satisfying the Tangled Rope gate. However, the mandatrophy question remains empirically open: Is opacity maintained primarily because it solves genuine coordination problems that no less-extractive alternative solves (coordination-primary hypothesis) or primarily because it enables extraction and is defended through beneficiary power despite more efficient alternatives existing (extraction-primary hypothesis)? If coordination-primary: opacity is legitimate institutional arrangement; classification is justified Tangled Rope. If extraction-primary: opacity is engineered extraction; more perspectives should classify as Snare or Piton. Resolution requires comparative data: (1) operational efficiency metrics with varying transparency levels, (2) innovation outcomes across different intellectual property regimes, (3) historical cases where transparency increased or decreased without corresponding supply chain disruption. Without this data, the Tangled Rope classification is tentative — a plausible reading of the structure that could shift toward Snare if extraction proves primary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opacity_functionality,
    'How much of supply chain opacity is genuinely required for coordination efficiency vs. how much is engineered rent extraction?',
    'Comparative analysis of transparency levels in regulated vs unregulated markets; correlation between disclosure requirements and operational cost changes; supply chain performance metrics before/after transparency interventions',
    'If high efficiency requirement: opacity is partly justified coordination mechanism, classification shifts toward Rope/Scaffold. If low efficiency requirement: opacity is primarily extraction mechanism, classification shifts toward Snare from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_functionality, empirical, 'Genuine coordination necessity vs engineered opacity').

omega_variable(
    counterfeit_detection_capacity,
    'What fraction of counterfeit/substandard drugs can be detected with current supply chain visibility, and what fraction would be detectable with full transparency?',
    'Epidemiological analysis of detected counterfeit prevalence; pharmaceutical quality surveillance data; comparative studies of supply chains with varying transparency levels',
    'If current detection ~50% and full transparency ~95%: opacity is high-cost extraction mechanism causing patient harm. If current detection ~80% and full transparency ~90%: opacity has lower net extractiveness and higher coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfeit_detection_capacity, empirical, 'Counterfeit detection rates at current vs full transparency').

omega_variable(
    innovation_cost_opacity_link,
    'Does pharmaceutical innovation actually require supply chain compartmentalization and proprietary opacity, or is this institutional preference rather than structural necessity?',
    'Analysis of innovation rates in high-transparency vs high-opacity sectors; comparison with other industries (electronics, automotive) with different transparency models; patent protection efficacy data independent of supply chain secrecy',
    'If required: opacity is legitimate innovation incentive, classification shifts toward legitimate Rope. If institutional preference: opacity is engineered extraction, confirms Snare/Tangled Rope from patient perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_cost_opacity_link, conceptual, 'Whether innovation actually requires supply chain opacity').

omega_variable(
    regulatory_capture_extent,
    'To what extent do pharmaceutical manufacturers control the opacity through regulatory capture vs regulators imposing opacity through institutional inertia?',
    'Historical analysis of transparency mandate resistance; regulatory agency funding sources and revolving-door patterns; comparison of transparency requirements in different jurisdictions with different capture profiles',
    'If high manufacturer capture: extraction is actively enforced by beneficiary, confirms Snare from victims'' perspectives. If high regulatory inertia: constraint is Piton, suggesting sunset mechanisms may be more effective than enforcement changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'Manufacturer capture vs regulatory inertia in maintaining opacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pharmaceutical_supply_chain_opacity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pharma_opac_tr_t0, pharmaceutical_supply_chain_opacity, theater_ratio, 0, 0.52).
narrative_ontology:measurement(pharma_opac_tr_t15, pharmaceutical_supply_chain_opacity, theater_ratio, 15, 0.62).
narrative_ontology:measurement(pharma_opac_tr_t30, pharmaceutical_supply_chain_opacity, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(pharma_opac_be_t0, pharmaceutical_supply_chain_opacity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pharma_opac_be_t15, pharmaceutical_supply_chain_opacity, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(pharma_opac_be_t30, pharmaceutical_supply_chain_opacity, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pharmaceutical_supply_chain_opacity, resource_allocation).
narrative_ontology:boltzmann_floor_override(pharmaceutical_supply_chain_opacity, 0.12).
narrative_ontology:affects_constraint(pharmaceutical_supply_chain_opacity, drug_counterfeiting).
narrative_ontology:affects_constraint(pharmaceutical_supply_chain_opacity, pharmaceutical_price_opacity).
narrative_ontology:affects_constraint(pharmaceutical_supply_chain_opacity, healthcare_supply_disruption).

% DUAL FORMULATION NOTE:
% Pharmaceutical supply chain opacity is the upstream constraint affecting drug counterfeiting (downstream: ε=0.72, Snare from patient perspective), pharmaceutical price opacity (downstream: ε=0.65, Snare from patient perspective), and supply disruption risks (downstream: ε=0.50, Tangled Rope from healthcare system perspective). This story models the structural information architecture; downstream stories model specific extraction mechanisms enabled by this opacity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pharmaceutical_supply_chain_opacity, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
