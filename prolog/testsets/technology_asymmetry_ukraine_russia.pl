% ============================================================================
% CONSTRAINT STORY: technology_asymmetry_ukraine_russia
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_asymmetry_ukraine_russia, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technology_asymmetry_ukraine_russia
 *   human_readable: Technology Asymmetry in Ukraine-Russia Conflict
 *   domain: political_economy/regime_stability/military_conflict
 *
 * SUMMARY:
 *   The technology asymmetry between Ukraine and Russia in the 2022-2025
 *   conflict presents as a structural constraint with mountain
 *   characteristics from multiple perspectives but with identifiable
 *   beneficiaries that flag it as a false summit candidate. Ukraine's
 *   distributed defense innovation sector (300+ private companies) operates
 *   with rapid prototyping cycles, commercial technology integration
 *   (Starlink, Telegram, commercial satellite imagery), and flexible
 *   procurement. Russia's centralized military-industrial complex faces
 *   Western sanctions cutting access to critical semiconductors and dual-use
 *   technology, brain drain, and institutional rigidity. Observable metrics
 *   include drone production rates (Ukraine contracted 50,000 UGVs for 2026),
 *   kill zone depth (1,000-1,500 miles from Ukrainian territory), and
 *   refinery attack success (158 attacks, 10 major refineries damaged by May
 *   2026). The constraint appears immutable at tactical and operational
 *   timescales — Russian frontline units cannot overcome the drone swarm
 *   advantage, and the Russian MIC cannot rapidly reconstitute semiconductor
 *   supply chains. However, the asymmetry benefits identifiable actors
 *   (Ukrainian defense sector, Western technology suppliers) and depends on
 *   active enforcement mechanisms (sanctions, export controls), raising the
 *   question of whether this is a natural law of innovation systems or a
 *   contingent geopolitical advantage naturalized as inevitable.
 *
 * KEY AGENTS:
 *   - Russian Frontline Units: Primary target (powerless/trapped) — experience technology gap as immutable battlefield reality; cannot exit or adapt at tactical timescales
 *   - Russian Military-Industrial Complex: Institutional target (institutional/constrained) — faces sanctions, supply chain collapse, brain drain; constrained by centralized procurement and Western technology bans
 *   - Ukraine Defense Innovation Sector: Primary beneficiary (institutional/arbitrage) — 300+ companies with rapid innovation cycles; benefits from distributed structure and Western technology access
 *   - Western Technology Suppliers: Secondary beneficiary (institutional/arbitrage) — Starlink, satellite imagery providers, dual-use technology companies benefit from expanded military applications and strategic partnerships
 *   - Distributed Innovation Networks: Abstract beneficiary — the structural form itself (decentralized, rapid-cycle, commercial integration) is validated and strengthened by the asymmetry
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent geopolitical arrangement (Western sanctions + technology access) as an immutable law of innovation systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_asymmetry_ukraine_russia, 0.15).
domain_priors:suppression_score(technology_asymmetry_ukraine_russia, 0.2).
domain_priors:theater_ratio(technology_asymmetry_ukraine_russia, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_asymmetry_ukraine_russia, extractiveness, 0.15).
narrative_ontology:constraint_metric(technology_asymmetry_ukraine_russia, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(technology_asymmetry_ukraine_russia, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_asymmetry_ukraine_russia, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(technology_asymmetry_ukraine_russia, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_asymmetry_ukraine_russia, mountain).
narrative_ontology:human_readable(technology_asymmetry_ukraine_russia, "Technology Asymmetry in Ukraine-Russia Conflict").
narrative_ontology:topic_domain(technology_asymmetry_ukraine_russia, "political_economy/regime_stability/military_conflict").

domain_priors:emerges_naturally(technology_asymmetry_ukraine_russia).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_asymmetry_ukraine_russia, ukraine_defense_sector).
narrative_ontology:constraint_beneficiary(technology_asymmetry_ukraine_russia, western_technology_suppliers).
narrative_ontology:constraint_beneficiary(technology_asymmetry_ukraine_russia, distributed_innovation_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RUSSIAN FRONTLINE UNITS (MOUNTAIN) — Experience the technology gap as an immutable battlefield reality. Cannot exit the constraint (trapped by military command structure), cannot change the innovation deficit at tactical timescales. The drone swarms, precision strikes, and real-time intelligence appear as unchangeable features of the operational environment.
constraint_indexing:constraint_classification(technology_asymmetry_ukraine_russia, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RUSSIAN MIC (MOUNTAIN) — Constrained by sanctions, supply chain dependencies, and centralized procurement. The technology gap appears as a structural limit: Western component bans, brain drain, and institutional rigidity create barriers that cannot be overcome within biographical timescales through institutional action alone. The constraint is experienced as external and immutable.
constraint_indexing:constraint_classification(technology_asymmetry_ukraine_russia, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UKRAINE DEFENSE SECTOR (ROPE) — 300+ private companies with rapid prototyping cycles experience the constraint as pure coordination: matching innovation capacity to battlefield requirements. Net beneficiary of the asymmetry. The distributed structure enables arbitrage across multiple technology pathways and supplier networks.
constraint_indexing:constraint_classification(technology_asymmetry_ukraine_russia, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: WESTERN SUPPLIERS (ROPE) — Starlink, commercial satellite imagery, dual-use technology providers experience the constraint as coordination: aligning commercial technology with military applications. Beneficiaries of expanded market access and strategic partnerships. Arbitrage exit through multiple client states and commercial markets.
constraint_indexing:constraint_classification(technology_asymmetry_ukraine_russia, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the technology asymmetry reflects a deeper structural principle: distributed innovation networks systematically outpace centralized command economies in complex adaptive domains. This appears as a near-universal pattern across historical conflicts where institutional flexibility meets institutional rigidity. However, the declared beneficiaries (Ukraine defense sector, Western suppliers) flag this as a false summit candidate — what appears as natural law may naturalize contingent geopolitical arrangements.
constraint_indexing:constraint_classification(technology_asymmetry_ukraine_russia, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_asymmetry_ukraine_russia_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(technology_asymmetry_ukraine_russia, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(technology_asymmetry_ukraine_russia, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(technology_asymmetry_ukraine_russia, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(technology_asymmetry_ukraine_russia, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(technology_asymmetry_ukraine_russia, ExtMetricName, E),
    domain_priors:suppression_score(technology_asymmetry_ukraine_russia, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(technology_asymmetry_ukraine_russia),
    narrative_ontology:constraint_metric(technology_asymmetry_ukraine_russia, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(technology_asymmetry_ukraine_russia, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(technology_asymmetry_ukraine_russia_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low but non-zero. The asymmetry extracts from Russian military effectiveness and regime stability, but the extraction is modest because the constraint operates primarily through capability denial rather than active rent-seeking. Ukrainian defense companies capture market share and Western suppliers gain strategic partnerships, but the extraction is not the primary function — the primary function is military effectiveness. The rising trajectory (0.05 → 0.15 over 40 months) reflects increasing commercialization and market concentration as successful Ukrainian defense companies scale. Suppression (0.20): Low-moderate. Russia faces significant barriers to closing the technology gap (sanctions, supply chain dependencies, institutional rigidity) but suppression is not total — smuggling networks, domestic substitution efforts, and potential sanctions erosion provide partial exit paths. The rising trajectory (0.10 → 0.20) reflects tightening Western export controls and secondary sanctions. Theater ratio (0.10): Very low, declining. The technology asymmetry is highly functional — drone strikes, precision targeting, and real-time intelligence produce measurable battlefield effects. The declining trajectory (0.15 → 0.10) reflects increasing operational integration and reduced performative signaling as the technology matures from demonstration to standard practice. Accessibility collapse (0.85): Very high. Once the distributed innovation advantage is understood, alternative pathways collapse — centralized procurement cannot match the adaptation speed of 300+ competing firms. Resistance (0.15): Low. The asymmetry meets minimal active resistance because it appears as a natural consequence of institutional structure rather than as an imposed constraint. Russian attempts to reform the MIC face institutional inertia rather than organized opposition.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a critical false summit pattern: what appears as immutable natural law from the trapped and constrained perspectives (Russian units and MIC) is experienced as pure coordination by the beneficiaries (Ukrainian defense sector, Western suppliers). The analytical observer's mountain classification naturalizes the asymmetry as an inevitable consequence of distributed vs centralized innovation systems, but the structural data reveals identifiable beneficiaries and active enforcement mechanisms (sanctions, export controls). The gap between the Russian MIC's mountain experience (biographical timescale, constrained exit) and the Ukrainian sector's rope experience (immediate timescale, arbitrage exit) is not merely perspectival — it reflects real asymmetric power and exit options. The theater ratio (0.10) confirms the asymmetry is highly functional, not performative, which strengthens the mountain appearance but does not resolve whether the underlying structure is natural law or enforced advantage.
 *
 * DIRECTIONALITY LOGIC:
 *   Russian frontline units are full targets (d ≈ 0.95): trapped exit, powerless, no beneficiary status — they bear maximum extraction through capability denial and attrition. Russian MIC is institutional target (d ≈ 0.70): constrained exit, institutional power provides some agency, but victim status and sanctions lock dominate — substantial extraction through technology denial and market exclusion. Ukraine defense sector is institutional beneficiary (d ≈ 0.10): arbitrage exit, institutional power, primary beneficiary status — experiences the constraint as coordination opportunity with minimal extraction. Western suppliers are institutional beneficiaries (d ≈ 0.15): arbitrage exit, institutional power, secondary beneficiary status — net positive but less concentrated benefit than Ukrainian firms. The analytical observer (d ≈ 0.50) occupies the symmetric position by definition, but the presence of clear beneficiaries flags the mountain classification as potentially naturalizing a contingent arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   FALSE SUMMIT DIAGNOSTIC: This constraint resolves the mandatrophy by demonstrating how a genuine capability asymmetry (distributed innovation outpacing centralized procurement) can be both structurally real AND contingent on enforced conditions. The mountain classification from Russian perspectives is their authentic experience — they cannot overcome the technology gap within their institutional constraints. The rope classification from Ukrainian and Western perspectives is their authentic experience — they coordinate innovation and technology transfer with minimal friction. The analytical mountain is the naturalization error: treating the asymmetry as an immutable law of innovation systems when it depends on active Western enforcement (sanctions preventing Russian technology access) and geopolitical alignment (Western willingness to supply Ukraine). The constraint is not 'really' a mountain or 'really' a rope — it is both simultaneously from different structural positions, and the false summit signature (mountain + beneficiaries + enforcement) flags that the analytical framing naturalizes what is actually a maintained advantage. The omega variables document the irreducible uncertainties: Would the asymmetry persist without sanctions? Can Russia adapt institutionally? Is Ukraine structurally dependent on Western technology? These questions cannot be resolved from within the current configuration, which is exactly what makes this a false summit rather than a genuine natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_contingent_advantage,
    'Is the technology asymmetry an immutable property of distributed vs centralized innovation systems, or a contingent advantage dependent on Western sanctions, supply chain access, and geopolitical alignment?',
    'Counterfactual analysis: Would the asymmetry persist if sanctions were lifted and Russia regained access to Western semiconductor supply chains? Historical comparison with other conflicts where centralized states had technology access.',
    'If natural law: Mountain classification holds — distributed innovation is structurally superior regardless of external conditions. If contingent: Reclassify as Tangled Rope — the asymmetry is maintained through active Western enforcement (sanctions, export controls) and benefits identifiable actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_advantage, conceptual, 'Whether technology asymmetry is natural law or contingent geopolitical advantage').

omega_variable(
    innovation_cycle_sustainability,
    'Can Ukraine''s 300+ company distributed innovation model sustain its current pace, or does it depend on wartime emergency conditions and Western financial support that may not persist?',
    'Longitudinal tracking of Ukrainian defense sector: company survival rates, innovation output, and technology deployment rates in post-conflict scenarios or under reduced Western aid.',
    'If sustainable: The distributed model is a genuine structural advantage (Mountain holds). If dependent on emergency conditions: The asymmetry is temporary (Scaffold) or maintained through external subsidy (Tangled Rope with Western donors as beneficiaries).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_cycle_sustainability, empirical, 'Sustainability of distributed innovation model beyond wartime conditions').

omega_variable(
    centralized_adaptation_capacity,
    'Is Russia''s centralized military-industrial complex structurally incapable of adapting to distributed threats, or merely slower to adapt within the current institutional configuration?',
    'Analysis of Russian institutional reforms, decentralization attempts, and adaptation rates. Comparison with historical cases of centralized militaries adapting to asymmetric threats (Soviet Afghanistan, US Vietnam).',
    'If structurally incapable: Mountain classification confirmed — the constraint is immutable given the institutional form. If adaptation is possible but slow: Reclassify based on adaptation timeline and whether the slowness itself constitutes extraction from Russian military effectiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(centralized_adaptation_capacity, empirical, 'Whether centralized adaptation is impossible or merely slow').

omega_variable(
    western_technology_dependence,
    'Does Ukraine''s technology advantage depend on continued access to Western dual-use technology (Starlink, commercial satellites, semiconductors) in ways that create structural dependency?',
    'Supply chain analysis: identification of single points of failure, Western veto points, and Ukrainian indigenous technology capacity. Scenario modeling of Western technology cutoff.',
    'If structurally dependent: The asymmetry is not a natural law but a Tangled Rope where Ukraine coordinates with Western suppliers while bearing dependency risk. If Ukraine has developed indigenous alternatives: Mountain classification more defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(western_technology_dependence, empirical, 'Degree of Ukrainian structural dependency on Western technology access').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_asymmetry_ukraine_russia, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_asym_theater_2022_02, technology_asymmetry_ukraine_russia, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tech_asym_theater_2023_02, technology_asymmetry_ukraine_russia, theater_ratio, 12, 0.12).
narrative_ontology:measurement(tech_asym_theater_2024_02, technology_asymmetry_ukraine_russia, theater_ratio, 24, 0.1).
narrative_ontology:measurement(tech_asym_theater_2025_02, technology_asymmetry_ukraine_russia, theater_ratio, 36, 0.1).
narrative_ontology:measurement(tech_asym_theater_2025_06, technology_asymmetry_ukraine_russia, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(tech_asym_extract_2022_02, technology_asymmetry_ukraine_russia, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(tech_asym_extract_2023_02, technology_asymmetry_ukraine_russia, base_extractiveness, 12, 0.08).
narrative_ontology:measurement(tech_asym_extract_2024_02, technology_asymmetry_ukraine_russia, base_extractiveness, 24, 0.12).
narrative_ontology:measurement(tech_asym_extract_2025_02, technology_asymmetry_ukraine_russia, base_extractiveness, 36, 0.15).
narrative_ontology:measurement(tech_asym_extract_2025_06, technology_asymmetry_ukraine_russia, base_extractiveness, 40, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(tech_asym_suppress_2022_02, technology_asymmetry_ukraine_russia, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(tech_asym_suppress_2023_02, technology_asymmetry_ukraine_russia, suppression_requirement, 12, 0.15).
narrative_ontology:measurement(tech_asym_suppress_2024_02, technology_asymmetry_ukraine_russia, suppression_requirement, 24, 0.18).
narrative_ontology:measurement(tech_asym_suppress_2025_02, technology_asymmetry_ukraine_russia, suppression_requirement, 36, 0.2).
narrative_ontology:measurement(tech_asym_suppress_2025_06, technology_asymmetry_ukraine_russia, suppression_requirement, 40, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_asymmetry_ukraine_russia, resource_allocation).

% DUAL FORMULATION NOTE:
% The technology asymmetry is a single constraint with multiple structural readings. It could be decomposed into separate stories for sanctions enforcement (Tangled Rope — Western states coordinate to deny Russia technology while extracting geopolitical leverage) and innovation system structure (Mountain — distributed networks inherently outpace centralized systems). However, the two mechanisms are deeply entangled in this case: the innovation advantage depends on technology access, and the sanctions depend on the innovation gap to be strategically meaningful. Modeling as a single constraint with false summit characteristics captures this entanglement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
