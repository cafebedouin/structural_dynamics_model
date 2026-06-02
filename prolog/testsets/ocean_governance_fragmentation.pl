% ============================================================================
% CONSTRAINT STORY: ocean_governance_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ocean_governance_fragmentation, []).

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
 *   constraint_id: ocean_governance_fragmentation
 *   human_readable: Ocean Governance Fragmentation: State Sovereignty vs. Transnational Ecosystem Coordination
 *   domain: international_environmental/maritime_governance
 *
 * SUMMARY:
 *   Ocean governance fragmentation represents a structural tension between
 *   the foundational international law principle of state sovereignty and the
 *   biological reality that marine ecosystems operate at transnational
 *   scales. The constraint manifests across three nested jurisdictional
 *   levels: (1) the high seas (Areas Beyond National Jurisdiction, ABNJ),
 *   governed by sectoral organizations with minimal coordination capacity;
 *   (2) exclusive economic zones (EEZs) claimed by coastal states under
 *   UNCLOS, where states have resource rights but limited ability to manage
 *   migratory stocks; and (3) regional seas, where multiple states and
 *   organizations create overlapping, non-binding governance frameworks. The
 *   fragmentation creates a classic tragedy-of-the-commons structure where
 *   individual actors (coastal states, industrial operators) benefit from
 *   extracting resources while collective constraints (ecosystem integrity,
 *   food security for excluded populations) degrade. The constraint exhibits
 *   all three extraction mechanisms simultaneously: (A) asymmetric benefit
 *   capture by states and operators who have sovereignty or flag authority;
 *   (B) suppression of alternatives through legal structures that enshrine
 *   state sovereignty and exclude non-state actors; (C) theatrical governance
 *   frameworks (regional seas organizations, voluntary agreements) that
 *   perform coordination while minimizing actual constraint on extraction.
 *   The extractiveness metric (0.58) reflects that the system extracts
 *   substantial value for beneficiaries while degrading shared resources. The
 *   suppression (0.62) is high but not absolute — states have formal
 *   structures for cooperation (UNCLOS, regional bodies, emerging BBNJ
 *   agreement), but enforcement capacity is minimal. Theater (0.68) is
 *   substantial because regional seas organizations, environmental
 *   agreements, and marine protection frameworks perform significant
 *   ceremonial functions while ecosystem-scale governance remains minimal.
 *
 * KEY AGENTS:
 *   - Marine Ecosystems: Primary victim (powerless/trapped) — cannot exit, cannot organize, bears extraction through overfishing and mining in unregulated zones
 *   - Small-Scale Fishing Communities (Non-Coastal): Primary victim (powerless/trapped) — excluded by sovereignty regime, no alternative access, face protein insecurity
 *   - Landlocked & Geographically Disadvantaged States: Primary victim (powerless/trapped) — excluded by geography and sovereignty doctrine, bear costs without benefits
 *   - Coastal States (Small/Medium): Secondary victim (moderate/constrained) — experience mixed benefits and costs; constrained by inability to manage migratory stocks alone
 *   - Major Coastal States: Primary beneficiary (institutional/arbitrage) — extract rents from EEZs, control flag-state operators, arbitrage between permissive and strict jurisdictions
 *   - Industrial Fishing & Mineral Extraction Corporations: Primary beneficiary (powerful/mobile) — operate under favorable flags, exploit unregulated high seas, high exit mobility
 *   - Environmental & Conservation Coalitions: Organized agent (organized/constrained) — building alternative governance frameworks (MPAs, ABMTs, BBNJ) with sunset vision
 *   - Regional Seas Organizations: Institutional actor (institutional/arbitrage) — maintain governance theater; low enforcement capacity; persist through legal obligation and inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent sovereignty regime as immutable feature of international relations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ocean_governance_fragmentation, 0.58).
domain_priors:suppression_score(ocean_governance_fragmentation, 0.62).
domain_priors:theater_ratio(ocean_governance_fragmentation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ocean_governance_fragmentation, extractiveness, 0.58).
narrative_ontology:constraint_metric(ocean_governance_fragmentation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ocean_governance_fragmentation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ocean_governance_fragmentation, tangled_rope).
narrative_ontology:human_readable(ocean_governance_fragmentation, "Ocean Governance Fragmentation: State Sovereignty vs. Transnational Ecosystem Coordination").
narrative_ontology:topic_domain(ocean_governance_fragmentation, "international_environmental/maritime_governance").

domain_priors:requires_active_enforcement(ocean_governance_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ocean_governance_fragmentation, coastal_states).
narrative_ontology:constraint_beneficiary(ocean_governance_fragmentation, industrial_fishing_operators).
narrative_ontology:constraint_beneficiary(ocean_governance_fragmentation, mineral_extraction_companies).
narrative_ontology:constraint_victim(ocean_governance_fragmentation, marine_ecosystem_integrity).
narrative_ontology:constraint_victim(ocean_governance_fragmentation, non_coastal_states).
narrative_ontology:constraint_victim(ocean_governance_fragmentation, fishing_communities_in_excluded_zones).
narrative_ontology:constraint_victim(ocean_governance_fragmentation, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARINE ECOSYSTEM INTEGRITY (SNARE) — Trapped within a fragmented governance structure that cannot coordinate at ecosystem scale. Pelagic fish stocks migrate across EEZs and high seas with no authority capable of managing them as integrated systems. Suppression is absolute: the ecosystem has no voice in governance structures, cannot exit the constraint, and bears extraction through overfishing, pollution, and mining in unregulated zones.
constraint_indexing:constraint_classification(ocean_governance_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL-SCALE FISHING COMMUNITIES OUTSIDE EEZs (SNARE) — Excluded from oceanic resources by state sovereignty claims while large industrial operators exploit shared stocks. Trapped by geography and lack of state backing. No alternative livelihoods available in maritime regions. Suppression is structural: exclusion is legally enforced; exit requires relocation or abandonment of traditional practices.
constraint_indexing:constraint_classification(ocean_governance_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: COASTAL STATES — MEDIUM POWER (TANGLED ROPE) — Experience mixed costs and benefits. Fragmentation allows them to exclude foreign fishing (coordination benefit) but prevents them from enforcing ecosystem-scale conservation (extraction cost). Exit is constrained by sovereignty expectations and inability to manage migratory stocks alone. Effective extraction moderate: states capture resource rents but cannot prevent stock collapse due to unregulated high seas fishing.
constraint_indexing:constraint_classification(ocean_governance_fragmentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: MAJOR COASTAL STATES & FLAG STATE OPERATORS (ROPE) — Net beneficiaries through sovereignty extraction. Control their EEZs, authorize industrial operators under their flags, capture resource rents and license fees. Fragmentation is their coordination success: it enables extraction while minimizing external accountability. Effective extraction high but experienced as legitimate coordination because they control the regime. Exit via integration would reduce their extraction — they have arbitrage capacity to maintain current structure.
constraint_indexing:constraint_classification(ocean_governance_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LANDLOCKED & DISADVANTAGED STATES (SNARE) — Excluded from ocean resource access by geography and state sovereignty regime. Suppression absolute: no exit option, no voice in ocean governance, bear costs of overfishing (protein security, trade disruption) without benefit. Trapped by the fundamental fragmentation of the regime.
constraint_indexing:constraint_classification(ocean_governance_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: INDUSTRIAL OPERATORS (TANGLED ROPE) — Primary extractive beneficiaries with exit mobility. Fragmentation enables extraction: they operate under favorable flags, exploit unregulated high seas stocks, and escape accountability through jurisdictional gaps. But they also depend on the coordination framework (flag registries, port facilities, enforcement mechanisms). Exit is mobile: can relocate flag registry, change operational zones, arbitrage regulatory gaps. Effective extraction very high with some coordination dependence.
constraint_indexing:constraint_classification(ocean_governance_fragmentation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ENVIRONMENTAL COALITIONS (SCAFFOLD) — Organized agents (IUCN, WWF, ocean conservation groups) see fragmentation as a temporary coordination failure being addressed through treaty-building and ecosystem-based management frameworks. Marine Protected Areas (MPAs), Area-Based Management Tools (ABMTs), and the recent BBNJ agreement represent sunset mechanisms. Constrained by lack of enforcement power and state opposition, but optimistic about governance maturation. Classify as scaffold because coalition agents envision and are building concrete alternative pathways (ecosystem-based coordination over flag-state extraction).
constraint_indexing:constraint_classification(ocean_governance_fragmentation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: REGIONAL SEAS ORGANIZATIONS (PITON) — Barcelona Convention, Nairobi Convention, OSPAR, NOAA Regional Fishery Bodies perform substantial ceremonial functions while functional coordination remains minimal. Theater ratio high (0.72+): organizations produce reports, convene meetings, issue recommendations with minimal enforcement capacity. They persist through institutional inertia and legal obligation rather than effective ecosystem governance. Theaters persist because the alternative — admitting governance failure — would delegitimize state sovereignty claims.
constraint_indexing:constraint_classification(ocean_governance_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, fragmentation appears as an immutable feature of international relations: state sovereignty is a foundational principle, and the tragedy of the commons is a natural consequence of shared resource access. Ocean governance fragmentation emerges from irreducible structural properties of the anarchic international system. However, this perspective risks false summitry — the constraint exhibits clear beneficiaries (coastal states, industrial operators) and victims (ecosystems, excluded communities), indicating that 'inevitable fragmentation' naturalizes contingent institutional arrangements.
constraint_indexing:constraint_classification(ocean_governance_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ocean_governance_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ocean_governance_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ocean_governance_fragmentation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ocean_governance_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ocean_governance_fragmentation, TR),
    TR >= 0.70.

:- end_tests(ocean_governance_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts substantial value for coastal states and industrial operators through resource capture, but is not as extreme as pure piracy (0.80+) because formal legal frameworks create the appearance and some substance of coordination. The increase from 0.35 to 0.58 over 20 years reflects intensification of extraction as technology enables deeper fishing and mining, while governance capacity remains static. The metric accounts for: (1) rents captured by coastal states and flag operators; (2) ecosystem degradation costs borne by non-beneficiaries; (3) protein insecurity for excluded populations. Suppression (0.62): High. States enforce sovereignty through naval power, port controls, and legal exclusion. Excluded populations have no formal voice in governance, no alternative institutional channels, no exit. However, suppression is not absolute — international agreements exist, some states have conservation commitments, and organized coalitions are building alternative frameworks. Theater ratio (0.68): High and rising. Regional seas organizations perform substantial governance theater: they issue recommendations, convene meetings, produce environmental assessments, and declare protective areas, but have minimal enforcement capacity and make marginal impact on extraction rates. New governance layers (MPAs, ABMTs, BBNJ) add complexity without yet reducing extraction — thus increasing the theater/function ratio. The rising trajectory (0.52 → 0.68) reflects accumulation of governance machinery that constrains behavior only at the margins.
 *
 * PERSPECTIVAL GAP:
 *   Ocean governance fragmentation produces the full range of perspectival disagreement. Major coastal states and industrial operators genuinely experience the constraint as rope — a coordination success that enables mutual benefit while maintaining their sovereignty and extraction capacity. The coordination benefit is real: the EEZ framework, flag state system, and port state control create a governance structure that enables tracking, financing, and orderly resource access rather than open-access anarchy. Environmental coalitions see scaffold — the constraint as a temporary coordination failure being progressively resolved through new ecosystem-based frameworks (BBNJ, marine protected areas, regional seas integration). The scaffold perspective is not aspirational fantasy; it identifies real institutional mechanisms (BBNJ implementation timelines, MPA expansion, benefit-sharing agreements) that are creating alternative pathways. However, victims experience snare: marine ecosystems and excluded populations perceive the constraint as pure extraction with no coordination benefit. The gap between rope (beneficiary), scaffold (organized reformers), and snare (victim) is not a matter of different measurements of the same underlying reality — it reflects genuinely incompatible structural positions. The rope observer has exit mobility and authority; the snare observer has neither. The falseness of the mountain perspective is evident: the analytical observer risks framing fragmentation as an inevitable consequence of state sovereignty, but the structural data reveals concrete beneficiaries and victims, indicating the constraint is not natural law but rather an institutional arrangement that distributes costs asymmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from agent position relative to extraction flow. Coastal state operators have d ≈ 0.15 (low directionality toward extraction target role — they are beneficiaries with exit mobility via flag arbitrage). Enclosed communities have d ≈ 0.95 (high directionality toward target role — trapped, no exit, victim status). Small-to-medium coastal states have d ≈ 0.60 (moderate-high — they experience some benefit via EEZ control but also loss from inability to manage shared stocks). The analytical observer has d ≈ 0.72 (standard for analytical position). The constraint's perspectival gap is extreme: beneficiaries experience rope (coordination with exit), while victims experience snare (extraction with no exit). The magnitude of the perspectival gap (rope → snare across beneficiary/victim contexts) indicates high asymmetry, which Boltzmann analysis flags as potential contamination — when different agents at different power levels experience fundamentally incompatible constraint types, structural coupling suggests the constraint's observed ε may be suppressed by power asymmetry, and true extractiveness is higher than measured.
 *
 * MANDATROPHY ANALYSIS:
 *   Ocean governance fragmentation resolves the mandatrophy through perspectival relativity and institutional decomposition. The constraint is genuinely tangled rope at the institutional level (coastal states perceive coordination with extraction) and genuinely snare at the ecosystem level (ecosystems perceive pure extraction). The mandatrophy question 'which type is correct?' dissolves when we recognize that tangled rope and snare are measuring different structural positions within the same constraint system. A coastal state and a marine ecosystem are not disagreeing about the constraint's type — they are occupying incompatible structural positions within it. The state has governance authority (coordination function) that extracts rents. The ecosystem has no authority and experiences extraction. Both observations are structurally correct; they describe different roles in the same system. The mandatrophy resolves by recognizing that asymmetrically distributed institutional authority creates tangled rope for beneficiaries and snare for victims. The emergence of ecosystem-based governance alternatives (BBNJ, ABMTs, integrated marine spatial planning) represents a genuine institutional development: if these frameworks mature to implementation, the snare victims would transition to constrained (still high costs, but genuine voice in governance decisions) and eventually to mobile or constrained rope (actual participation in governance). The scaffold classification documents this institutional development as a real trajectory, not an aspirational fantasy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marine_ecosystem_definition_boundary,
    'What scale of ecosystem organization constitutes the correct governance unit — individual stocks, regions, ocean basins, or the global system?',
    'Empirical determination of population connectivity, larval dispersal patterns, and migratory route dependencies for key species. Comparison of stock collapse rates under region-scale vs. basin-scale management.',
    'If correct unit is stock-level: regional fragmentation is adequate coordination. If unit is basin or global: fragmentation is inherently inadequate and snare dominates. If unit is genuinely multi-scalar: tangled rope classification is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marine_ecosystem_definition_boundary, empirical, 'Correct spatial scale of marine ecosystem governance units').

omega_variable(
    flag_state_enforcement_capacity,
    'Can flag states genuinely enforce conservation standards on their operators, or does sovereignty provide effective immunity for extraction?',
    'Analysis of compliance rates for flag state conservation orders; comparison of enforcement actions per operator between strict and permissive flag registries; tracking of operators changing flags after enforcement.',
    'If strong enforcement: rope classification for beneficiaries is confirmed — genuine coordination with oversight. If weak enforcement: snare classification dominates — flag state system is extraction mechanism, not governance. If enforcement is selectively applied: tangled rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(flag_state_enforcement_capacity, empirical, 'Actual enforcement capacity of flag state regime').

omega_variable(
    ecosystem_based_management_implementation,
    'Are the new BBNJ agreement and Marine Protected Area frameworks genuinely creating ecosystem-scale governance, or are they theatrical overlays on unchanged flag-state extraction?',
    'Tracking MPA effectiveness metrics (biomass recovery, larval recruitment, population stability) over 10-year implementation period; monitoring whether new ABMTs in high seas reduce effective extraction or merely redistribute it; assessing enforcement of BBNJ provisions.',
    'If effective: scaffold sunset logic confirmed; governance fragmentation is being resolved. If theatrical: piton classification dominates; fragmentation persists with added complexity layer. If partial: tangled rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_based_management_implementation, empirical, 'Whether ecosystem-based management frameworks provide genuine governance alternatives').

omega_variable(
    state_sovereignty_non_negotiability,
    'Is state sovereignty an immutable feature of international governance, or a contingent institutional arrangement that could be reformed?',
    'Historical analysis of sovereignty doctrine evolution; comparative cases where states have voluntarily surrendered jurisdiction (EU model, international courts); counterfactual reasoning about federation or planetary governance models.',
    'If immutable: mountain classification appears justified; fragmentation is natural law. If contingent: false summit confirmed; beneficiary identification (coastal states, industrial operators) becomes primary analysis. If reformable but prohibitively costly: tangled rope confirmed at institutional level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_sovereignty_non_negotiability, conceptual, 'Whether state sovereignty is immutable or contingent').

omega_variable(
    landlocked_state_exclusion_necessity,
    'Is geographic exclusion of landlocked states from ocean resources a necessary consequence of the EEZ framework, or a policy choice that could be redistributed?',
    'Comparison of different ocean revenue-sharing models (benefit-sharing agreements exist for some resources); analysis of whether landlocked state access programs reduce overall extraction or merely add distribution mechanism.',
    'If necessary consequence: snare classification confirmed. If redistributable: constraint decomposes into separate extraction and allocation constraints, with allocation being potentially reformable scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(landlocked_state_exclusion_necessity, preference, 'Whether landlocked state exclusion is necessary or policy choice').

omega_variable(
    high_seas_mining_extraction_potential,
    'Will deep-sea polymetallic nodule mining create pressure for new ocean governance frameworks, or will it reinforce fragmentation through competing flag state mining licenses?',
    'Monitoring of mining applications and license issuance through ISA; tracking of environmental impact assessments; assessment of whether mining creates sufficient coordination pressure to accelerate ecosystem-based governance or accelerates destructive extraction.',
    'If creates coordination pressure: scaffold sunset logic accelerates. If reinforces flag state extraction: snare dynamics intensify; suppression increases. Critical for forecasting governance trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(high_seas_mining_extraction_potential, empirical, 'Deep-sea mining as governance inflection point').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ocean_governance_fragmentation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ogf_tr_t0, ocean_governance_fragmentation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ogf_tr_t10, ocean_governance_fragmentation, theater_ratio, 10, 0.62).
narrative_ontology:measurement(ogf_tr_t20, ocean_governance_fragmentation, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(ogf_be_t0, ocean_governance_fragmentation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ogf_be_t10, ocean_governance_fragmentation, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ogf_be_t20, ocean_governance_fragmentation, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ogf_su_t0, ocean_governance_fragmentation, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ogf_su_t10, ocean_governance_fragmentation, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(ogf_su_t20, ocean_governance_fragmentation, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ocean_governance_fragmentation, resource_allocation).
narrative_ontology:affects_constraint(ocean_governance_fragmentation, eez_sovereignty_extraction).
narrative_ontology:affects_constraint(ocean_governance_fragmentation, flag_state_jurisdiction_loophole).
narrative_ontology:affects_constraint(ocean_governance_fragmentation, high_seas_commons_tragedy).
narrative_ontology:affects_constraint(ocean_governance_fragmentation, marine_biodiversity_protection_mandate).
narrative_ontology:affects_constraint(ocean_governance_fragmentation, climate_ocean_carbon_sequestration).

% DUAL FORMULATION NOTE:
% Ocean governance fragmentation is upstream of multiple specific extraction mechanisms (EEZ sovereignty over particular stocks, flag state regulatory arbitrage, high seas open-access overfishing, mining jurisdictional gaps). Each downstream constraint has its own epsilon reflecting specific extraction modality. Fragmentation provides the structural conditions enabling multiple extraction mechanisms to coexist without accountability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ocean_governance_fragmentation, institutional, 0.18).
constraint_indexing:directionality_override(ocean_governance_fragmentation, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
