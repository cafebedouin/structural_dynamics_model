% ============================================================================
% CONSTRAINT STORY: unclos_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_2026, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unclos_2026
 *   human_readable: UN Convention on the Law of the Sea (2026 Context)
 *   domain: legal/geopolitical/environmental
 *
 * SUMMARY:
 *   The UN Convention on the Law of the Sea (UNCLOS, 1982) established a
 *   global framework for maritime governance, creating jurisdictional zones
 *   (territorial seas, exclusive economic zones, continental shelves, high
 *   seas) and institutions (International Seabed Authority, International
 *   Tribunal for the Law of the Sea) to manage ocean use. In the 2026
 *   context, UNCLOS functions simultaneously as a coordination mechanism
 *   enabling global maritime commerce, fisheries, and resource management,
 *   and as an extraction mechanism protecting developed-state and industrial
 *   interests while constraining developing-state autonomy and Indigenous
 *   ocean communities. The constraint exhibits all six Deferential Realism
 *   types from different structural positions: developed maritime states and
 *   industrial corporations experience UNCLOS as enabling coordination
 *   (Rope); developing coastal states experience mixed coordination and
 *   extraction (Tangled Rope); small island developing states and Indigenous
 *   communities experience pure extraction with no exit option (Snare); the
 *   International Seabed Authority functions as a degraded, performative
 *   institution (Piton); emerging enforcement technologies (RFMOs, satellite
 *   monitoring, blockchain catch documentation) are building alternative
 *   pathways to UNCLOS compliance (Scaffold); and from a civilizational view,
 *   UNCLOS appears as a natural solution to inherent ocean governance
 *   problems (Mountain, but with false-summit characteristics revealing
 *   beneficiary design).
 *
 * KEY AGENTS:
 *   - Developed Maritime States (institutional/arbitrage): Primary beneficiaries — control enforcement capacity, flag-state registry, technological dominance; can arbitrage between compliance and non-compliance
 *   - Industrial Fishing and Resource Extraction Corporations (powerful/arbitrage): Primary beneficiaries — exploit fish stocks and minerals through flag-state arbitrage, IUU fishing, and technical non-compliance before enforcement catches up
 *   - Small Island Developing States (powerless/trapped): Primary victims — nominal EEZ ownership provides no functional protection against industrial fleets; fish stocks collapse before SIDS can build enforcement capacity
 *   - Indigenous Ocean Communities (powerless/trapped): Primary victims — structurally excluded from decision-making; UNCLOS institutionalizes state sovereignty over zones traditionally governed by community norms
 *   - Developing Coastal States (moderate/constrained): Secondary victims — benefit from nominal EEZ control but lack enforcement capacity; high compliance costs with limited revenue capture
 *   - International Seabed Authority (institutional/constrained): Institutional actor — degraded by state capture and industry pressure; functionless authority maintaining theater of regulation without real oversight of deep-sea mining
 *   - Regional Fisheries Management Organizations (organized/constrained): Emerging alternative pathway — building real-time enforcement infrastructure (electronic monitoring, satellite tracking, blockchain documentation) that may supersede UNCLOS-mandated state reporting
 *   - Analytical Observer (analytical/analytical): Civilizational perspective — risks naturalizing UNCLOS boundary structures as immutable features of ocean governance rather than contingent design choices that encoded beneficiary advantages
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_2026, 0.58).
domain_priors:suppression_score(unclos_2026, 0.62).
domain_priors:theater_ratio(unclos_2026, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(unclos_2026, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unclos_2026, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_2026, tangled_rope).
narrative_ontology:human_readable(unclos_2026, "UN Convention on the Law of the Sea (2026 Context)").
narrative_ontology:topic_domain(unclos_2026, "legal/geopolitical/environmental").

domain_priors:requires_active_enforcement(unclos_2026).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_2026, '87b409ca-e823-468a-bf60-e567cfabf2ab').
narrative_ontology:cs_kernel_codification('87b409ca-e823-468a-bf60-e567cfabf2ab', formalized).
narrative_ontology:cs_authority_grounding('87b409ca-e823-468a-bf60-e567cfabf2ab', extraction).
narrative_ontology:cs_interpretation_layer_present('87b409ca-e823-468a-bf60-e567cfabf2ab').
narrative_ontology:cs_reading_relation('87b409ca-e823-468a-bf60-e567cfabf2ab', unclos_global_commons_reading, coexists_with).
narrative_ontology:cs_reading_relation('87b409ca-e823-468a-bf60-e567cfabf2ab', unclos_sovereignty_maximization_reading, coexists_with).
narrative_ontology:cs_reading_relation('87b409ca-e823-468a-bf60-e567cfabf2ab', unclos_indigenous_ocean_rights_reading, forecloses).
narrative_ontology:cs_axiom('87b409ca-e823-468a-bf60-e567cfabf2ab', foundational, coastal_state_maritime_sovereignty).
narrative_ontology:cs_axiom_status(coastal_state_maritime_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('87b409ca-e823-468a-bf60-e567cfabf2ab', coastal_state_maritime_sovereignty, conventional).
narrative_ontology:cs_axiom('87b409ca-e823-468a-bf60-e567cfabf2ab', foundational, high_seas_freedom_of_navigation).
narrative_ontology:cs_axiom_status(high_seas_freedom_of_navigation, holdable).
narrative_ontology:cs_axiom_grounding('87b409ca-e823-468a-bf60-e567cfabf2ab', high_seas_freedom_of_navigation, conventional).
narrative_ontology:cs_axiom('87b409ca-e823-468a-bf60-e567cfabf2ab', secondary, isba_authority_over_area_minerals).
narrative_ontology:cs_axiom_status(isba_authority_over_area_minerals, overridden).
narrative_ontology:cs_axiom_grounding('87b409ca-e823-468a-bf60-e567cfabf2ab', isba_authority_over_area_minerals, empirically_contingent).
narrative_ontology:cs_reference_frame('87b409ca-e823-468a-bf60-e567cfabf2ab', post_colonial_maritime_state_equality).
narrative_ontology:cs_drift_state('87b409ca-e823-468a-bf60-e567cfabf2ab', contemporary_2026_extraction_asymmetry, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('87b409ca-e823-468a-bf60-e567cfabf2ab', '2026-02-27T14:32:18Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_2026, developed_maritime_states).
narrative_ontology:constraint_beneficiary(unclos_2026, industrial_fishing_fleets).
narrative_ontology:constraint_beneficiary(unclos_2026, multinational_resource_companies).
narrative_ontology:constraint_victim(unclos_2026, small_island_developing_states).
narrative_ontology:constraint_victim(unclos_2026, indigenous_ocean_communities).
narrative_ontology:constraint_victim(unclos_2026, global_fish_stocks).
narrative_ontology:constraint_victim(unclos_2026, deep_sea_mining_adjacent_ecosystems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SIDS (SNARE) — Trapped within UNCLOS zones without capacity to enforce claims against industrial fleets or developed states. Nominal 200 nm EEZ provides no functional protection; fish stocks collapse before SIDS can build enforcement capacity. Maximum extraction with no exit option.
constraint_indexing:constraint_classification(unclos_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIGENOUS COMMUNITIES (SNARE) — Structural exclusion from decision-making despite high-seas commons traditionally governed through community norms. UNCLOS institutionalizes state sovereignty over zones where communities historically harvested resources. Generational extraction through dispossession.
constraint_indexing:constraint_classification(unclos_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DEVELOPING COASTAL STATES (TANGLED ROPE) — Benefit from nominal EEZ control and resource claims (coordination function), but lack enforcement capacity against industrial fleets. High compliance costs (monitoring, reporting, capacity-building) with limited revenue capture. Constrained exit — cannot withdraw from UNCLOS without geopolitical cost.
constraint_indexing:constraint_classification(unclos_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: DEVELOPED MARITIME STATES (ROPE) — Primary beneficiaries. Possess enforcement capacity, flag-state control over merchant fleets, deep-sea resource access, and technological dominance. Experience UNCLOS as enabling coordination for global maritime commerce (high-seas freedom, predictable EEZ boundaries) while capturing asymmetric benefits. Can arbitrage between UNCLOS compliance and flag-state registry shopping.
constraint_indexing:constraint_classification(unclos_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INDUSTRIAL EXTRACTION CORPORATIONS (ROPE) — Operate across maritime zones using flag-state arbitrage, IUU (illegal, unreported, unregulated) fishing, and technical non-compliance. Access open-access fish stocks and mineral deposits before regulatory capacity catches up. Experience UNCLOS as coordination mechanism for market access with minimal enforcement cost.
constraint_indexing:constraint_classification(unclos_2026, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL SEABED AUTHORITY (PITON) — Created by UNCLOS to regulate deep-sea mineral extraction in the Area (commons beyond continental shelves). Functionally degraded by state capture, industry pressure, and lack of enforcement resources. Theater-heavy meetings produce licensing frameworks that lack real verification capacity. Persists through institutional inertia despite low functional authority over actual mining operations.
constraint_indexing:constraint_classification(unclos_2026, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: RFMO COALITION (SCAFFOLD) — Structured as temporary coordination overlays on UNCLOS (NAFO, CCAMLR, WCPFC). Display sunset logic in the 2026 context: electronic monitoring, blockchain catch documentation, and real-time satellite enforcement are creating alternatives to UNCLOS-mandated state-level reporting. Expected to supersede traditional UNCLOS compliance by ~2035 in major fishing zones.
constraint_indexing:constraint_classification(unclos_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: CIVILIZATIONAL ANALYSIS / NATURAL LAW VIEW (MOUNTAIN) — From a universal timescale, ocean governance requires boundary-setting and dispute resolution that appear as natural/inevitable: dividing maritime space, allocating fishing rights, managing commons dilemmas. UNCLOS appears as the least-bad solution to inherent coordination problems. However, structural beneficiary data contradicts this naturalization — developed states designed UNCLOS to encode their advantages as permanent features.
constraint_indexing:constraint_classification(unclos_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unclos_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unclos_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unclos_2026, TR),
    TR >= 0.70.

:- end_tests(unclos_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated and stable. UNCLOS creates nominal protections for coastal states (200 nm EEZs, continental shelf claims) while providing no enforcement mechanism to prevent industrial fleets from harvesting stocks faster than regulatory capacity can respond. Extractiveness rose from 0.35 (1982, when states simply didn't have the capacity to exploit the asymmetry) to 0.48 (1997, as fishing-fleet technology accelerated and IUU fishing became industrialized) to 0.58 (2012-2026, as deep-sea mining intensified and small island states faced unprecedented depletion of exclusive resources). The plateau at 0.58 from 2012 onward reflects that further extraction would trigger institutional breakdown (UNCLOS renegotiation pressure, SIDS coalition defection). Suppression (0.62): Moderate-high and rising. Suppression mechanisms include: enforcement asymmetry (developed states can monitor their fleets; SIDS cannot), technology barriers (satellite monitoring, underwater surveys require capital SIDS lack), institutional capture (ISA controlled by mining interests and developed states), and structural dependence (SIDS have no alternative governance framework and face geopolitical cost of UNCLOS withdrawal). Theater ratio (0.68): High and rising. ISA licensing procedures, ITLOS dispute rulings, and RFMO scientific committees produce extensive documentation and procedural compliance (theater) while actual fishing capacity, mineral extraction, and ecosystem damage proceed largely uncontrolled by these bodies. Theater increased from 0.42 (1982 — when UNCLOS was newly empowering) to 0.68 (2026 — as institutions mature without functional enforcement). This trajectory tracks the piton signature: original function (resource governance) degraded; institutional persistence maintained through procedural theater.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival range reveals the constraint's Tangled Rope character: genuine coordination function coexists with asymmetric extraction. Developed states and corporations see coordination (Rope) — UNCLOS predictably allocates maritime space, provides rules for commerce, and enables resource access. Developing coastal states see mixed benefit and cost (Tangled Rope) — nominal sovereignty over EEZs provides coordination value (disputed boundaries resolved through UNCLOS rather than military confrontation) but extraction is severe (inability to enforce boundaries means fish stocks and minerals drain away). SIDS and Indigenous communities see pure extraction (Snare) — they are locked into a system that provides no protection and from which withdrawal incurs geopolitical cost. The ISA demonstrates piton dynamics: original regulatory function has atrophied (ISA cannot enforce mining standards) but institutional structures persist through theater. RFMOs show scaffold logic: new enforcement technologies (satellite monitoring, real-time reporting, blockchain documentation) are creating a parallel governance pathway that could supersede UNCLOS-mandated state-level enforcement within 10-15 years. The mountain perspective reveals the false summit: UNCLOS structures appear 'natural' (boundary-drawing is necessary for ocean governance) but are actually contingent design choices that encoded developed-state advantages.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) represents each agent's structural relationship to the constraint. Developed maritime states are net beneficiaries with exit arbitrage (d ≈ 0.05, low extraction experienced) — they designed UNCLOS, can flag-state arbitrage between regimes, and benefit from predictable rules. Industrial extraction corporations are similar beneficiaries with technological escape routes (d ≈ 0.15, low extraction) — they can shift operations, change flags, use IUU tactics. Small island developing states are full extraction targets locked into the system (d ≈ 0.95, maximum extraction) — they cannot exit UNCLOS without geopolitical isolation, cannot enforce boundaries against industrial fleets, and lack capital to exploit their own resources. Indigenous communities face structural exclusion with identity-locking (d ≈ 0.89) — they are both materially trapped (no autonomous governance structure outside UNCLOS) and identity-locked (their ocean identities are constituted through community relationships to marine resources that UNCLOS forbids them to govern). Developing coastal states occupy intermediate positions (d ≈ 0.65, moderate-high extraction) — they benefit from nominal EEZ ownership but face suppression through enforcement asymmetry. The ISA experiences moderate extraction (d ≈ 0.55) — its institutional mandate conflicts with pressure from mining interests, and its constrained exit (cannot withdraw without delegitimizing UNCLOS) creates persistent conflict.
 *
 * MANDATROPHY ANALYSIS:
 *   UNCLOS resolves the mandatrophy by demonstrating that Tangled Rope classification is correct: the constraint simultaneously coordinates and extracts. Pure coordination (Rope) cannot explain why fish stocks collapse in SIDS EEZs despite nominal legal protections. Pure extraction (Snare) cannot explain why developed states invested decades in negotiating UNCLOS — they could have simply excluded SIDS through military dominance. The hybrid (Tangled Rope) explains both: UNCLOS genuinely coordinates global maritime commerce, provides dispute-resolution institutions, and enables resource claims (coordination function), while simultaneously encoding enforcement asymmetries that allow developed states and corporations to extract resources from developing-state EEZs (extraction function). The constraint requires active enforcement (Tangled Rope gate: true) because beneficiaries must actively suppress developing-state enforcement capacity to maintain the extraction advantage. The false summit (mountain perspective naturalizing UNCLOS as inherent to ocean governance) is unmasked by the beneficiary data: if UNCLOS were truly natural law, the boundary widths would be immutable rather than actively defended by developed states in 2026 renegotiation disputes. The ISA degradation (piton classification) demonstrates what happens when extraction exceeds coordination: the institution designed to manage the global commons has lost functional authority and persists through procedural theater. The scaffold perspective (RFMOs building real enforcement) shows the constraint's vulnerability: if alternative coordination mechanisms with lower extraction can develop, UNCLOS enforcement layer becomes supersedable within a generation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity_asymmetry,
    'Is UNCLOS enforcement failure structural (no resources) or intentional (beneficial states block capacity-building)?',
    'Historical analysis of ISA/ITLOS budget allocations, state compliance with monitoring requirements, and comparison with enforcement budgets in other international regimes (IAEA, WTO)',
    'If structural/resource scarcity: reformable through funding (reduces snare classification for SIDS). If intentional underinvestment: reveals extraction mechanism and sustains snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_asymmetry, empirical, 'Whether enforcement failure is resource or intentional constraint').

omega_variable(
    eez_nominal_vs_functional_control,
    'Does nominal EEZ ownership provide any actual control over fish stocks or mineral access to developing states, or is it purely ceremonial?',
    'Empirical comparison: developing-state revenue from EEZ resources vs IUU fishing losses; time-series analysis of fish stock recovery in adequately-monitored vs under-monitored EEZs; satellite data on vessel compliance patterns',
    'If functional: UNCLOS provides genuine coordination benefit (raises rope/tangled_rope classification). If nominal: EEZ is pure theater masking extraction (sustains snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eez_nominal_vs_functional_control, empirical, 'Functional control: EEZ protection in developing states').

omega_variable(
    natural_law_vs_contingent_design,
    'Are UNCLOS boundary structures (territorial seas, EEZ widths, continental shelf rules) immutable features of ocean governance or contingent design choices that benefited state actors present in 1982?',
    'Historical analysis of UNCLOS negotiation records; counterfactual exploration of alternative boundary regimes (narrow EEZs, expanded commons, Indigenous co-management); evidence from pre-1982 maritime disputes and customary law traditions',
    'If natural law: mountain classification justified. If contingent design: reveals false summit and reclassifies as tangled_rope (beneficiary-designed system naturalizing extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_design, conceptual, 'Natural law vs contingent design of UNCLOS structures').

omega_variable(
    iuu_fishing_causation,
    'Is IUU fishing (illegal, unreported, unregulated) a consequence of UNCLOS enforcement failures or a fundamental feature of open-access fish-stock dynamics that UNCLOS was designed to minimize?',
    'Time-series analysis of IUU prevalence before/after UNCLOS entry-into-force; comparison with equivalent commons-management problems outside maritime (terrestrial wildlife, groundwater); analysis of IUU economics relative to UNCLOS-compliant fishing profit margins',
    'If enforcement-driven: UNCLOS is improvable (scaffold logic applies). If fundamental: UNCLOS structure itself incentivizes extraction (snare logic applies).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iuu_fishing_causation, empirical, 'IUU fishing: enforcement failure or structural incentive').

omega_variable(
    deep_sea_mining_extraction_boundary,
    'Does deep-sea mineral extraction under ISA oversight represent genuine coordination for global benefit or exploitation of the global commons by states with capital/technology to extract?',
    'Analysis of ISA licensing criteria, benefit-sharing mechanisms, and actual revenue flows to non-extracting states; comparison with terrestrial mining governance; ecological impact assessments vs revenue projections',
    'If genuine coordination: ISA moves from piton toward rope/scaffold. If extraction: ISA is snare by another name, and the constraint reclassifies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deep_sea_mining_extraction_boundary, empirical, 'Deep-sea mining: coordination or commons extraction').

omega_variable(
    historical_custom_law_displacement,
    'Did UNCLOS institutional formalization displace existing customary maritime law traditions (Indigenous governance, regional norms) in ways that reduced community autonomy and increased state extraction?',
    'Comparative legal history: pre-UNCLOS customary regimes vs post-UNCLOS governance; analysis of dispute outcomes in ITLOS involving Indigenous or community claims; archival evidence of negotiation positions by colonized vs colonizer states',
    'If displacement occurred: reveals institutional extraction mechanism underlying UNCLOS (increases snare classification for affected communities). If integration attempted: demonstrates tangled_rope coordination-extraction hybrid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_custom_law_displacement, empirical, 'UNCLOS displacement of customary maritime law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_2026, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_theater_1982, unclos_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(unclos_theater_1997, unclos_2026, theater_ratio, 15, 0.58).
narrative_ontology:measurement(unclos_theater_2012, unclos_2026, theater_ratio, 30, 0.65).
narrative_ontology:measurement(unclos_theater_2026, unclos_2026, theater_ratio, 44, 0.68).

% Extraction over time
narrative_ontology:measurement(unclos_extractiveness_1982, unclos_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(unclos_extractiveness_1997, unclos_2026, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(unclos_extractiveness_2012, unclos_2026, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(unclos_extractiveness_2026, unclos_2026, base_extractiveness, 44, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(unclos_suppression_1982, unclos_2026, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(unclos_suppression_1997, unclos_2026, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(unclos_suppression_2012, unclos_2026, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(unclos_suppression_2026, unclos_2026, suppression_requirement, 44, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_2026, resource_allocation).
narrative_ontology:boltzmann_floor_override(unclos_2026, 0.18).
narrative_ontology:affects_constraint(unclos_2026, iuu_fishing_industrial_structure).
narrative_ontology:affects_constraint(unclos_2026, deep_sea_mining_regulatory_capture).
narrative_ontology:affects_constraint(unclos_2026, island_state_climate_migration_lock).
narrative_ontology:affects_constraint(unclos_2026, indigenous_ocean_rights_exclusion).

% DUAL FORMULATION NOTE:
% UNCLOS decomposes into structurally distinct constraints along observable lines: (1) UNCLOS as coordination framework for maritime commerce (lower ε, higher coordination function) vs (2) UNCLOS as extraction lock for resource-dependent developing states (higher ε, lower coordination function for victims). The 0.58 base extractiveness value represents the aggregate structural property of the constraint; but a view emphasizing commerce coordination would produce lower ε (~0.35), while a view emphasizing resource extraction lock would produce higher ε (~0.75). Per ε-invariance principle, these observables indicate constraint family rather than single-observable-dependent constraint. The family is linked through network edges showing how maritime commerce coordination depends on resource extraction asymmetries being suppressed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_2026, institutional, 0.08).
constraint_indexing:directionality_override(unclos_2026, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
