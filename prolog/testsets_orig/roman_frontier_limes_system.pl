% ============================================================================
% CONSTRAINT STORY: roman_frontier_limes_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_frontier_limes_system, []).

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
 *   constraint_id: roman_frontier_limes_system
 *   human_readable: Roman Frontier Limes System
 *   domain: political/military/economic
 *
 * SUMMARY:
 *   The Roman frontier limes system (68 CE - 476 CE) represents a
 *   multi-century institutional constraint coordinating military defense,
 *   territorial control, and resource extraction across the empire's vast
 *   boundaries. The system functioned simultaneously as a coordination
 *   mechanism for collective security, an extraction apparatus taxing
 *   frontier populations and controlling external tribes, and eventually as
 *   performative bureaucracy maintained through inertia after military
 *   effectiveness declined. The limes demonstrates how a single structural
 *   phenomenon can legitimately classify as six different constraint types
 *   depending on the observer's position: the empire's military hierarchy
 *   experiences rope (coordination); frontier populations experience snare
 *   (trapped extraction); auxiliary soldiers experience tangled rope (mixed
 *   coordination and coercion); merchants experience rope (coordinated
 *   trade); the frontier population's peasantry experience snare
 *   (extraction); the late imperial administration experiences piton
 *   (degraded ritual); and the analytical observer risks a false summit
 *   naturalizing what is actually a contingent institutional choice.
 *
 * KEY AGENTS:
 *   - Roman Imperial Center: Primary beneficiary (institutional/arbitrage) — coordinates military power and resource extraction across continental scope with ability to arbitrage enforcement mechanisms
 *   - Frontier Populations: Primary victim (powerless/trapped) — subject to taxation, forced labor, and military occupation with no exit options
 *   - External Tribes: Secondary victim (powerless/trapped) — pressured by military barriers and trade dependency; strategic mobility limited despite some negotiating capacity
 *   - Auxiliary Soldiers: Mixed role (moderate/constrained) — gain military training and citizenship prospects but constrained by discipline and deployment requirements
 *   - Imperial Military Hierarchy: Primary beneficiary (institutional/arbitrage) — benefits from concentrated command structure and resource flows
 *   - Merchant Networks: Secondary beneficiary (institutional/arbitrage) — gain access to regulated trade routes and standardized tariff systems
 *   - Imperial Administration: Institutional actor (institutional/constrained) — maintains system through bureaucratic inertia even as military effectiveness declines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_frontier_limes_system, 0.52).
domain_priors:suppression_score(roman_frontier_limes_system, 0.68).
domain_priors:theater_ratio(roman_frontier_limes_system, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_frontier_limes_system, extractiveness, 0.52).
narrative_ontology:constraint_metric(roman_frontier_limes_system, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(roman_frontier_limes_system, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_frontier_limes_system, tangled_rope).
narrative_ontology:human_readable(roman_frontier_limes_system, "Roman Frontier Limes System").
narrative_ontology:topic_domain(roman_frontier_limes_system, "political/military/economic").

domain_priors:requires_active_enforcement(roman_frontier_limes_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_frontier_limes_system, roman_imperial_center).
narrative_ontology:constraint_beneficiary(roman_frontier_limes_system, frontier_military_hierarchy).
narrative_ontology:constraint_beneficiary(roman_frontier_limes_system, merchant_networks).
narrative_ontology:constraint_victim(roman_frontier_limes_system, frontier_populations).
narrative_ontology:constraint_victim(roman_frontier_limes_system, external_tribes).
narrative_ontology:constraint_victim(roman_frontier_limes_system, provincial_peasantry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTIER POPULATION (SNARE) — Trapped by military occupation, taxation extraction, and forced labor obligations. Subject populations cannot exit the system without abandonment of livelihood. Maximum suppression through garrison presence and legal coercion.
constraint_indexing:constraint_classification(roman_frontier_limes_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: EXTERNAL TRIBES (SNARE) — Trapped by military barriers, trade dependency on Roman markets, and constant threat of raiding. Exit requires abandonment of territorial claims. Suppression through fortifications and military threat.
constraint_indexing:constraint_classification(roman_frontier_limes_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: AUXILIARY SOLDIERS (TANGLED ROPE) — Receive military training, steady pay, and citizenship prospects (after discharge), but subject to strict discipline and remote deployment. Constrained by career path dependence and long enlistment terms. System provides coordination benefits (organization, mutual defense) alongside significant extraction (coercion, mobility restriction).
constraint_indexing:constraint_classification(roman_frontier_limes_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: IMPERIAL MILITARY HIERARCHY (ROPE) — Primary beneficiary. Benefits from coordinated frontier defense, supply lines, and concentrated power. Experiences the limes as a coordination mechanism solving the collective action problem of managing vast borders. Minimal experienced extraction because benefits outweigh costs.
constraint_indexing:constraint_classification(roman_frontier_limes_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MERCHANT NETWORKS (ROPE) — Benefit from regulated trade routes, tariff standardization, and military protection against banditry. Experience the limes as coordination enabling long-distance commerce. Can arbitrage across empire through established toll and trade protocols.
constraint_indexing:constraint_classification(roman_frontier_limes_system, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: IMPERIAL ADMINISTRATION (PITON) — Maintains the limes through bureaucratic inertia long after its military efficiency declines. Theater ratio (0.55) reflects that significant administrative overhead is performative: census taking, fort rotation, and inspection protocols persist as ritual even as military effectiveness degrades. The system carries institutional momentum despite rising maintenance costs.
constraint_indexing:constraint_classification(roman_frontier_limes_system, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational view, the limes appears as an inevitable consequence of managing vast territorial scale: any empire containing diverse populations will develop boundary control mechanisms as a law-like feature of political organization. However, the structural data reveals this naturalization as problematic — the limes is a contingent institutional choice, not a necessity of scale.
constraint_indexing:constraint_classification(roman_frontier_limes_system, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_frontier_limes_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_frontier_limes_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_frontier_limes_system, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(roman_frontier_limes_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_frontier_limes_system, TR),
    TR >= 0.70.

:- end_tests(roman_frontier_limes_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The limes system extracted significant resources from frontier populations through taxation, forced labor, and supply requisitions. However, extraction was not maximal across all perspectives — merchants and military hierarchy benefited from coordination mechanisms that reduced transaction costs. The measurement trajectory shows increasing extractiveness from 0.38 (early coordination phase) to 0.62 (late bureaucratic phase), reflecting gradual shift from genuine defense coordination to rent-seeking administration. Suppression (0.68): High. Frontier populations faced severe barriers to exit: military occupation, legal restrictions, threat of violence, and economic dependency on tax/military systems. External tribes faced fortifications and organized military force. However, suppression was not absolute — some frontier populations migrated, some external tribes maintained trade relationships and negotiating capacity. Theater ratio (0.55): Moderate. Early limes emphasized functional military infrastructure (forts, roads, supply systems). By the late empire (200-300 years), theater increased substantially to 0.73 as administrative overhead proliferated without corresponding military effectiveness gains. Census-taking, inspection protocols, garrison rotations became performative — sustaining the system's image rather than enhancing defense.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap is between beneficiaries (rope) and victims (snare). The empire's perspective sees a coordination success — defense, trade, administration organized across continental scale. The frontier population's perspective sees pure extraction and control. The piton perspective reveals institutional degradation over time: theater rising from 0.25 to 0.73 while extractiveness also rose suggests the system was doing real work early but became increasingly performative. The mountain perspective (scale inevitably requires boundary control) naturalizes what appears in the structural data as contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial center and military hierarchy derive low d values from beneficiary status plus arbitrage exit options (institutional power, ability to reallocate resources and redefine strategy), yielding negative or near-zero effective extraction. Frontier populations derive high d from victim status plus trapped exit (powerless, no ability to leave without abandonment), yielding maximum f(d) and high chi. Auxiliary soldiers derive moderate d from mixed victim/beneficiary status plus constrained exit (get paid and trained but cannot freely leave), yielding moderate chi. Merchant networks derive low d from beneficiary status plus mobile/arbitrage options, but must be differentiated from local merchants who faced higher barriers — the perspective focuses on the networks that actually benefited (institutional actors), not those excluded from benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The limes system resolves mandatrophy by demonstrating that all six types are legitimately present depending on structural position. The system is not 'really' one type that other perspectives misidentify — it genuinely exhibits coordination (rope from military hierarchy), extraction (snare from frontier populations), mixed effects (tangled rope from auxiliary soldiers), degradation (piton in late empire), and a false natural-law appearance (mountain perspective that the analytical framework rejects). The measured increase in theater_ratio (0.25 → 0.73) and extractiveness (0.38 → 0.62) over 300 years reveals the system's trajectory: initially genuine coordination with collateral extraction, gradually shifting toward pure extraction with performative coordination claims. This trajectory is consistent with institutional capture — the system's beneficiaries gradually concentrated control while the coordination function atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_versus_control_boundary,
    'What portion of the limes system functions as genuine collective defense coordination versus pure extractive control?',
    'Archaeological analysis of fort density, troop movements, and infrastructure investment; comparison of defensive efficacy against barbarian incursions vs. internal tax collection efficiency; records of garrison redeployment patterns',
    'If coordination function dominates (>60%): classify as Tangled Rope from all perspectives. If extraction dominates (>70%): shift to Snare from all but beneficiary perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_versus_control_boundary, empirical, 'Partition of limes function between coordination and extraction').

omega_variable(
    external_tribe_autonomy,
    'Were external tribes truly trapped by the limes, or did they maintain strategic mobility and negotiating capacity despite the boundary?',
    'Historical evidence of tribal migration, trade negotiations, and military alliances formed across the limes; analysis of gift exchange and treaty protocols; settlement pattern analysis on both sides of the frontier',
    'If trapped: external tribes experienced maximum suppression (snare classification valid). If mobile: tribes had constrained but real exit options (tangled rope or rope from different perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_tribe_autonomy, empirical, 'Whether external tribes had strategic mobility beyond the limes').

omega_variable(
    civilian_extraction_mechanism,
    'Was civilian population extraction a primary function of the limes system, or a secondary effect of military organization?',
    'Comparative analysis of tax burdens in frontier vs. interior provinces; study of forced labor requisitions; analysis of grain shipments and their direction; assessment of whether frontier territories were net exporters or importers of resources',
    'If primary: extraction is structural (snare). If secondary: extraction is incidental to coordination (tangled rope). If frontier was subsidized: classification shifts to scaffold (investment in temporary security).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civilian_extraction_mechanism, empirical, 'Role of civilian extraction in limes maintenance').

omega_variable(
    piton_degradation_timeline,
    'When did the limes transition from functional military system to performative bureaucracy?',
    'Chronological analysis of fort occupation patterns, troop strength records, and infrastructure maintenance investments; comparison with declining military effectiveness against incursions; assessment of when garrison redeployments accelerated',
    'If degradation began early (2nd century): system was theater from near the start. If late (4th-5th century): system maintained functional effectiveness for 200+ years before becoming piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_degradation_timeline, empirical, 'Timeline of limes transition from military function to performative maintenance').

omega_variable(
    merchant_benefit_distribution,
    'Did merchant networks capturing benefits from the limes represent the majority of commercial operators, or a concentrated elite?',
    'Analysis of merchant records, tax documents, and trade good distributions; assessment of who could afford tariffs and who was excluded; study of whether long-distance trade was accessible to local merchants or restricted to metropolitan elites',
    'If distributed: rope classification for merchant networks is robust. If concentrated: limes enabled only elite merchant extraction (snare from local merchants'' perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(merchant_benefit_distribution, empirical, 'Distribution of merchant benefits from limes regulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_frontier_limes_system, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(limes_tr_t0, roman_frontier_limes_system, theater_ratio, 0, 0.25).
narrative_ontology:measurement(limes_tr_t100, roman_frontier_limes_system, theater_ratio, 100, 0.42).
narrative_ontology:measurement(limes_tr_t200, roman_frontier_limes_system, theater_ratio, 200, 0.61).
narrative_ontology:measurement(limes_tr_t300, roman_frontier_limes_system, theater_ratio, 300, 0.73).

% Extraction over time
narrative_ontology:measurement(limes_be_t0, roman_frontier_limes_system, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(limes_be_t100, roman_frontier_limes_system, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(limes_be_t200, roman_frontier_limes_system, base_extractiveness, 200, 0.56).
narrative_ontology:measurement(limes_be_t300, roman_frontier_limes_system, base_extractiveness, 300, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_frontier_limes_system, enforcement_mechanism).
narrative_ontology:affects_constraint(roman_frontier_limes_system, barbarian_tribe_organization).
narrative_ontology:affects_constraint(roman_frontier_limes_system, roman_provincial_taxation).
narrative_ontology:affects_constraint(roman_frontier_limes_system, military_supply_logistics).

% DUAL FORMULATION NOTE:
% The limes system decomposes into military defense (genuine collective action problem) and extractive taxation (pure redistribution). These might be modeled as separate constraints with different epsilon values: frontier_defense_coordination (lower epsilon, rope-dominant) vs frontier_extraction_apparatus (higher epsilon, snare-dominant). The integrated story treats both functions as inseparable in practice, though historically the balance shifted over time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(roman_frontier_limes_system, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
