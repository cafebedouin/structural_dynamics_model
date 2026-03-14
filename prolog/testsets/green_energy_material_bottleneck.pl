% ============================================================================
% CONSTRAINT STORY: green_energy_material_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_green_energy_material_bottleneck, []).

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
 *   constraint_id: green_energy_material_bottleneck
 *   human_readable: Green Energy Material Supply Bottleneck and Rent Extraction
 *   domain: energy_policy/materials_science
 *
 * SUMMARY:
 *   The green energy transition faces a critical material bottleneck:
 *   renewable energy technologies (wind turbines, solar panels, battery
 *   storage) depend on materials with no ready substitutes — rare earth
 *   elements for magnets, lithium for batteries, cobalt for cathodes, nickel
 *   for long-duration storage. The supply of these materials is
 *   geographically concentrated (China controls 70% of rare earth processing,
 *   DRC provides 70% of cobalt, Indonesia 30% of nickel) and historically
 *   controlled by extraction-focused oligopolies. This creates a structural
 *   tension: the climate transition requires massive material flows on
 *   accelerated timelines, but supply is constrained by geopolitical control,
 *   environmental extraction limits, and deliberate supply management by
 *   incumbent producers. The constraint exhibits all six DR types depending
 *   on observational position, but the core structure is Tangled Rope:
 *   genuine coordination (materials must be allocated to highest-priority
 *   uses) embedded within asymmetric extraction (scarcity rents flow to
 *   control holders, decarbonization costs rise, developing nations bear
 *   environmental cost).
 *
 * KEY AGENTS:
 *   - Decarbonization Pathway: Primary victim (powerless/trapped) — no substitutes, no negotiating leverage, bears acceleration costs
 *   - Renewable Energy Manufacturers: Secondary victim (moderate/constrained) — dependent on material supply, face price inflation and allocation risk, but have some alternatives (technology pivots, substitution pathways)
 *   - Developing Nations with Mineral Resources: Primary victim (powerless/trapped) — trapped between extraction dependency and environmental devastation; no leverage to demand fair pricing or environmental protection
 *   - Mining Oligopolies and Supply Controllers: Primary beneficiary (institutional/arbitrage) — control allocation, capture scarcity rents, have complete flexibility in supply management
 *   - Circular Economy Coalition: Organized actors (organized/constrained) — arXiv, recycling mandates, material efficiency standards; see sunset pathway
 *   - National Strategic Reserve Systems: Institutional theater (institutional/arbitrage) — maintain geopolitical framing as justification for supply control; function has degraded
 *   - Analytical Observer: Civilizational context (analytical/analytical) — sees both genuine coordination need and deliberate extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(green_energy_material_bottleneck, 0.58).
domain_priors:suppression_score(green_energy_material_bottleneck, 0.62).
domain_priors:theater_ratio(green_energy_material_bottleneck, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(green_energy_material_bottleneck, extractiveness, 0.58).
narrative_ontology:constraint_metric(green_energy_material_bottleneck, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(green_energy_material_bottleneck, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(green_energy_material_bottleneck, tangled_rope).
narrative_ontology:human_readable(green_energy_material_bottleneck, "Green Energy Material Supply Bottleneck and Rent Extraction").
narrative_ontology:topic_domain(green_energy_material_bottleneck, "energy_policy/materials_science").

domain_priors:requires_active_enforcement(green_energy_material_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(green_energy_material_bottleneck, incumbent_rare_earth_producers).
narrative_ontology:constraint_beneficiary(green_energy_material_bottleneck, mining_oligopolies).
narrative_ontology:constraint_beneficiary(green_energy_material_bottleneck, supply_control_gatekeepers).
narrative_ontology:constraint_victim(green_energy_material_bottleneck, renewable_energy_developers).
narrative_ontology:constraint_victim(green_energy_material_bottleneck, decarbonization_pathway).
narrative_ontology:constraint_victim(green_energy_material_bottleneck, developing_nations).
narrative_ontology:constraint_victim(green_energy_material_bottleneck, green_tech_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DECARBONIZATION IMPERATIVE (SNARE) — The transition to renewable energy depends entirely on materials with no substitutes (rare earths, lithium, cobalt). No alternative pathway exists. Bears full extraction cost through accelerated timeline and inflated procurement prices. Cannot exit or renegotiate terms. Maximum structural vulnerability — extraction flow runs entirely inward with no coordination benefit.
constraint_indexing:constraint_classification(green_energy_material_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATIONS AS VICTIMS (SNARE) — Nations with critical mineral reserves (DRC for cobalt, Indonesia for nickel, China for rare earths) face either (a) extraction of value via controlled export quotas and low commodity prices, or (b) environmental devastation from accelerated mining to meet global demand. Trapped: no alternative markets, no leverage, no exit. Superpowers extract resources and environmental cost simultaneously.
constraint_indexing:constraint_classification(green_energy_material_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: GREEN TECH MANUFACTURERS (TANGLED ROPE) — Face both coordination (supply chain stability, long-term volume commitments) and extraction (locked into multi-year contracts at escalating prices, supply rationing). Some coordination function exists — suppliers have incentive to reliably serve manufacturers — but asymmetric extraction embedded within: manufacturers bear price inflation and allocation risk while suppliers capture scarcity rents. Constrained exit: switching suppliers is costly and limited by oligopolistic supply.
constraint_indexing:constraint_classification(green_energy_material_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CIRCULAR ECONOMY TRANSITION (SCAFFOLD) — Organized actors (battery recycling mandates, circular design standards, closed-loop manufacturing) perceive the bottleneck as temporary and solvable through material substitution and recycling infrastructure. Low effective extraction because these agents see an exit pathway with a sunset: as recycling scales, the dependency on primary mining diminishes. Theater is lower here because the coalition measures success by actual material flows recovered, not by procurement announcements. Sunset clause: estimated 15-30 years for circular infrastructure to mature enough to substantially reduce primary material demand.
constraint_indexing:constraint_classification(green_energy_material_bottleneck, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: MINING OLIGOPOLIES (ROPE) — Institutional beneficiaries experience the constraint as pure coordination: controlling allocation solves the coordination problem of distributing scarce materials to highest bidders. They extract rents, but from their perspective this is legitimate reward for solving the allocation problem. High arbitrage options: can shift production between regions, hold production to maintain prices, or pivot to other commodities. Constraint appears as opportunity, not limitation.
constraint_indexing:constraint_classification(green_energy_material_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATIONAL STRATEGIC RESERVES (PITON) — Governments maintain stockpile policies and export control regimes that are nominally about strategic security but are substantially theater: the geopolitical framing justifies supply restrictions that are actually driven by commercial extraction incentives. The institutional mechanism persists through inertia and bureaucratic legitimacy, not because stockpiling actually prevents dependency (it doesn't — dependency is permanent). Theater ratio high because the security narrative masks economic rent-seeking. Function has degraded as climate transition has revealed that no stockpile size can substitute for sustained supply; the constraint is now maintained by institutional path-dependence rather than strategic necessity.
constraint_indexing:constraint_classification(green_energy_material_bottleneck, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational view, the green energy material bottleneck combines genuine coordination (materials must be allocated somewhere, supply must be matched to demand) with asymmetric extraction (scarcity rents, geopolitical manipulation, environmental cost externalization). The constraint is not natural law — alternative material pathways, recycling infrastructure, and synthetic substitutes could substantially reduce the bottleneck — but the institutions maintaining scarcity are deliberately preserved because beneficiaries profit from artificial constraint. The system is changeable but powerful actors resist change.
constraint_indexing:constraint_classification(green_energy_material_bottleneck, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(green_energy_material_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(green_energy_material_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(green_energy_material_bottleneck, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(green_energy_material_bottleneck, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(green_energy_material_bottleneck, TR),
    TR >= 0.70.

:- end_tests(green_energy_material_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate and increasing over the interval. The constraint's extractiveness has risen as the climate transition has accelerated demand while supply remains controlled by oligopolies. Initial extractiveness (0.32) reflected a period when material demand was modest and alternatives seemed available. Current extractiveness (0.58) reflects the reality that energy transition timelines are compressing, and scarcity rents are being captured by incumbent producers faster than alternatives (recycling, substitution) can mature. The trajectory shows institutional lock-in: as demand rises, control holders tighten supply restrictions, raising prices beyond commodity levels into pure rent extraction. Suppression (0.62): Moderate-high and structural. Barriers to escaping the bottleneck include: (1) no technical substitutes for many critical minerals in near-term (15-20 years), (2) high switching costs for green tech manufacturers to redesign products, (3) limited recycling infrastructure that cannot yet supply meaningful percentages, (4) geopolitical export controls and strategic reserves that restrict competition, (5) environmental/social barriers that make alternative sourcing (artisanal mining, informal supply) illegal or unethical. Victims cannot easily exit; beneficiaries have complete flexibility. Theater ratio (0.48): Moderate and stable. Unlike the verification bottleneck (high theater), the material bottleneck has primarily functional constraints (genuine material limits) with some performative elements (strategic security narratives). The constraint is not sustained by theater alone — it is materially grounded — but geopolitical framing adds a layer of legitimacy that masks economic rent-seeking.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon generates opposite classifications from different positions. The beneficiary (mining oligopolies) perceives the constraint as Rope — 'we are solving the critical allocation problem of distributing scarce materials.' This is functionally accurate: they ARE providing coordination. But from the victim's perspective (developing nations), the constraint is Snare — 'we are locked into dependency, extracting our environmental and social capital with no exit.' This is also functionally accurate: they ARE trapped with no alternatives. The gap reveals that the constraint is not determined by some objective property but by who has power, who bears costs, and who can exit. The analytical observer position is crucial: it can see both the coordination function AND the asymmetric extraction, which is why Tangled Rope is the claimed type. The Rope perspective naturalizes extraction as compensation for coordination; the Snare perspective experiences the coordination frame as a cover story for extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position: their power, exit options, and benefit/cost flow. Mining oligopolies with arbitrage options (can shift production, hold inventory, pivot to other commodities) experience low effective extraction because they are beneficiaries with exit capacity — d ≈ 0.10-0.20, f(d) ≈ -0.05 to 0.15. Developing nations with trapped status (no alternative economies, no export leverage, bounded by geography) and victim status (bear environmental cost) experience maximum extraction — d ≈ 0.95-1.0, f(d) ≈ 1.4. Renewable manufacturers with constrained exit (can switch suppliers but costly) and mixed benefit/cost (can pass costs to consumers but not fully) experience moderate extraction — d ≈ 0.55-0.65, f(d) ≈ 0.75-0.95. The analytical observer, positioned outside the constraint, derives d from the observed structural asymmetry (net flow of extraction from victims to beneficiaries) — d ≈ 0.72, f(d) ≈ 1.15. Scope amplification: the constraint operates at global scope (σ(S) ≈ 1.2), which amplifies the effective extractiveness of institutional beneficiaries. A local scarcity would be less severe because alternative suppliers could enter; global scarcity with geopolitical control prevents entry.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint does NOT exhibit mandatrophy because the analytical perspective correctly identifies Tangled Rope. The coordinate has two genuine structural functions: (1) allocation of scarce materials to highest-value uses (coordination), and (2) capture of scarcity rents by control holders (extraction). Both are real and irreducible. The mandatrophy trap would be if the analytical observer tried to force it into pure Rope (denying the extraction) or pure Snare (denying the coordination). The true structure is Tangled Rope because the beneficiary's Rope classification is NOT the whole story — it captures only the benefit side — and the victim's Snare classification is NOT the whole story — it captures only the cost side. The analytical classification integrates both: yes, coordination happens, AND yes, extraction happens, AND these are structurally linked (the extraction mechanism is embedded within the coordination institution). The intermediate ε value (0.58) reflects this: high enough to indicate significant extraction, but not extreme enough to indicate pure Snare, because genuine coordination value is being provided (materials ARE being allocated efficiently to uses that matter most for the transition).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_substitution_feasibility,
    'Can critical minerals (rare earths, lithium, cobalt) be technically substituted or eliminated through alternative chemistries and designs within the next 10-20 years?',
    'Laboratory breakthrough tracking; commercialization timelines for alternative battery chemistries (sodium-ion, solid-state, lithium-iron-phosphate); cost/performance curves for magnet-free motor designs and alternative permanent magnets',
    'If substitution is feasible: the bottleneck is temporary coordination problem (Scaffold). If substitution is infeasible: the bottleneck is structural constraint requiring long-term supply management (Tangled Rope persists). If substitution is possible but economically blocked: the bottleneck is deliberate artificial constraint (Snare classification strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_substitution_feasibility, empirical, 'Whether technical alternatives to critical minerals are commercially viable').

omega_variable(
    recycling_infrastructure_scaling,
    'Can recycling infrastructure scale fast enough (target: 50% material recovery by 2045) to reduce primary mining dependency to manageable levels?',
    'Recycling collection rate projections; cost curves for battery disassembly and material recovery; capacity buildout timelines for pyrometallurgical and hydrometallurgical processes; comparison of recycling energy cost vs. primary mining energy cost across full lifecycle',
    'If scaling is achievable: scaffold sunset is real (circular economy pathway viable). If scaling is blocked by economics or infrastructure: primary mining dependency persists (bottleneck is permanent feature, not temporary constraint). If scaling requires protectionist policy or subsidy: the bottleneck reveals policy choice rather than material limit (extraction mechanism clarified).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recycling_infrastructure_scaling, empirical, 'Whether recycling can scale to reduce primary mining dependency').

omega_variable(
    geopolitical_supply_manipulation,
    'How much of the perceived material scarcity is structural (genuine geological/extraction limits) vs. deliberate supply restriction (export controls, production quotas, speculative withholding)?',
    'Analysis of production vs. proven reserves ratios; tracking of export quota adjustments and strategic stockpile changes; correlation between announced ''shortages'' and price movements; comparison of reserves-to-consumption ratios for various critical minerals vs. historical commodity cycles',
    'If scarcity is primarily structural: bottleneck is legitimate coordination problem (Rope/Tangled Rope classification correct). If scarcity is primarily deliberate: bottleneck is a Snare (pure extraction mechanism). If mixed: the proportional split determines whether victims have unilateral escape options (lower scarcity control = higher exit capacity for manufacturers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_supply_manipulation, empirical, 'Structural scarcity vs. deliberate supply manipulation').

omega_variable(
    environmental_externality_internalization,
    'If environmental and health costs of mining were fully internalized into material prices, would the scarcity premium disappear or would genuine material limits emerge?',
    'Life-cycle assessment costing for cobalt mining (DRC), nickel mining (Indonesia), rare earth processing (China); health impact quantification (respiratory disease, water contamination); remediation cost estimates; comparison of true cost vs. current market price',
    'If internalization eliminates premium: extraction is largely rent-seeking (Snare strengthened). If internalization reveals genuine scarcity: bottleneck is natural constraint, not artificial (Mountain classification possible at local scope). If internalization creates new bottleneck in remediation capacity: the constraint shifts from material scarcity to environmental/social cost absorption capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_externality_internalization, empirical, 'Impact of environmental cost internalization on apparent scarcity').

omega_variable(
    developing_nation_agency_alternatives,
    'Do developing nations with critical mineral reserves have feasible alternatives to accepting either low commodity prices or environmental extraction, or is structural dependency irreversible?',
    'Analysis of vertical integration attempts (value-added processing, finished good manufacturing); examination of successful mineral-wealth-to-sovereignty transitions (e.g., Botswana diamonds vs. DRC cobalt); assessment of whether domestic renewable energy development could substitute for export revenue',
    'If alternatives exist: developing nations are Tangled Rope victims (high but not total extraction, some agency). If alternatives are blocked by power asymmetries: developing nations are Snare victims (complete dependency, no exit). If alternatives require coalition formation: organized response via OPEC-like coordination becomes possible (converting Snare to Tangled Rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(developing_nation_agency_alternatives, empirical, 'Whether developing nations have viable alternatives to resource extraction dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(green_energy_material_bottleneck, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gemb_tr_t0, green_energy_material_bottleneck, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gemb_tr_t5, green_energy_material_bottleneck, theater_ratio, 5, 0.42).
narrative_ontology:measurement(gemb_tr_t10, green_energy_material_bottleneck, theater_ratio, 10, 0.48).
narrative_ontology:measurement(gemb_tr_t15, green_energy_material_bottleneck, theater_ratio, 15, 0.51).

% Extraction over time
narrative_ontology:measurement(gemb_be_t0, green_energy_material_bottleneck, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gemb_be_t5, green_energy_material_bottleneck, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(gemb_be_t10, green_energy_material_bottleneck, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(gemb_be_t15, green_energy_material_bottleneck, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(green_energy_material_bottleneck, resource_allocation).
narrative_ontology:affects_constraint(green_energy_material_bottleneck, renewable_energy_supply_chain_fragility).
narrative_ontology:affects_constraint(green_energy_material_bottleneck, developing_nation_environmental_extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(green_energy_material_bottleneck, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
