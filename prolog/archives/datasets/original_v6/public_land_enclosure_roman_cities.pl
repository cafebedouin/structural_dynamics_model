% ============================================================================
% CONSTRAINT STORY: public_land_enclosure_roman_cities
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_land_enclosure_roman_cities, []).

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
 *   constraint_id: public_land_enclosure_roman_cities
 *   human_readable: Public Land Enclosure in Roman Cities
 *   domain: ancient_political_economy/urban_governance
 *
 * SUMMARY:
 *   Public land enclosure in Roman cities represents a structural transition
 *   from republican commons governance to imperial resource consolidation.
 *   The constraint exhibits genuine coordination functions (centralized grain
 *   distribution, infrastructure maintenance, tax collection) overlaid with
 *   asymmetric extraction that concentrates land and subsistence control in
 *   the patrician class and imperial administration. The enclosure operates
 *   through multiple mechanisms: legal prohibition of unauthorized commons
 *   access, military enforcement of boundary claims, and gradual
 *   internalization of the boundary as a natural feature of urbanism. The
 *   constraint intensified over the imperial period as extractive overlay
 *   accumulated (increased taxation, expanded private claims) while
 *   coordination functions remained constant. The plebeian experience is
 *   primarily one of trapped access to former subsistence mechanisms; their
 *   perceived extractiveness is highest. The patrician class and imperial
 *   administration experience the same constraint as pure coordination —
 *   boundary-setting that enables predictable governance. The analytical
 *   observer at the civilizational level risks naturalizing enclosure as
 *   inevitable state consolidation, missing the contingency of its extractive
 *   intensity on power asymmetry.
 *
 * KEY AGENTS:
 *   - Urban Plebeians: Primary victims (powerless/trapped) — lose access to subsistence commons (grain fields, pasture, wood collection); economically dependent on redistribution mechanisms controlled by patricians
 *   - Patrician Landholding Class: Primary beneficiaries (powerful/mobile) — expand private land claims, capture surplus value from consolidated estates, maintain political dominance through resource control
 *   - Imperial Administration: Secondary beneficiary (institutional/arbitrage) — coordinates centralized governance, stabilizes tax base, eliminates unauthorized commons claims that undermine authority
 *   - Municipal Authority: Constrained mediator (moderate/constrained) — must implement enclosure while coordinating genuine infrastructure (aqueducts, roads, grain storage); faces extraction pressure from both above (imperial mandate) and below (plebeian resistance)
 *   - Republican Legal Framework: Vestigial institution (institutional/arbitrage) — maintains nominal public ownership fiction while enforcement operates on private-property basis (piton classification)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as inevitable consequence of state scale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_land_enclosure_roman_cities, 0.58).
domain_priors:suppression_score(public_land_enclosure_roman_cities, 0.72).
domain_priors:theater_ratio(public_land_enclosure_roman_cities, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_land_enclosure_roman_cities, extractiveness, 0.58).
narrative_ontology:constraint_metric(public_land_enclosure_roman_cities, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(public_land_enclosure_roman_cities, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_land_enclosure_roman_cities, tangled_rope).
narrative_ontology:human_readable(public_land_enclosure_roman_cities, "Public Land Enclosure in Roman Cities").
narrative_ontology:topic_domain(public_land_enclosure_roman_cities, "ancient_political_economy/urban_governance").

domain_priors:requires_active_enforcement(public_land_enclosure_roman_cities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_land_enclosure_roman_cities, patrician_landholding_class).
narrative_ontology:constraint_beneficiary(public_land_enclosure_roman_cities, imperial_administration).
narrative_ontology:constraint_victim(public_land_enclosure_roman_cities, plebeian_access_to_subsistence).
narrative_ontology:constraint_victim(public_land_enclosure_roman_cities, urban_commons_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: URBAN PLEBEIAN (SNARE) — Trapped by legal prohibition on commons access and economic dependence on subsistence activities (grain distribution, pasturage, wood collection) formerly available on public land. No exit option: relocation to rural areas risks loss of urban food security network; remaining in city requires submission to patrician redistribution mechanisms. Maximum experienced extraction.
constraint_indexing:constraint_classification(public_land_enclosure_roman_cities, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MUNICIPAL AUTHORITY (TANGLED ROPE) — Constrained by imperial mandate but also genuinely coordinates city infrastructure (aqueducts, roads, grain storage). Faces extraction pressure from patrician land claims but also benefits from centralized authority. Coordinating function is real (water/sanitation) but overlaid with asymmetric extraction (patricians capture surplus value). Constrained exit: cannot withdraw from role without administrative collapse.
constraint_indexing:constraint_classification(public_land_enclosure_roman_cities, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: IMPERIAL ADMINISTRATION (ROPE) — Benefits from enclosure as a governance coordination mechanism: centralized control of subsistence redistribution, stable tax base from consolidated lands, elimination of unauthorized commons claims that undermine authority. Experiences the constraint as pure coordination: establishing public/private boundary enables predictable resource extraction and political control. Net beneficiary with exit options (can reallocate enclosure policy across provinces).
constraint_indexing:constraint_classification(public_land_enclosure_roman_cities, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PATRICIAN LANDHOLDING CLASS (TANGLED ROPE) — Primary beneficiary of enclosure (expanded private claims, reduced commons competition). Also coordinating genuine function: enclosure enables large-scale agricultural investment, infrastructure development, and urban planning. But coordination function is asymmetrically distributed — patricians capture benefits while plebeians bear costs. Mobile exit option (can relocate to other provinces or revert to commons claims if enforcement collapses) keeps extraction moderate rather than maximal.
constraint_indexing:constraint_classification(public_land_enclosure_roman_cities, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: REPUBLICAN LEGAL FICTION (PITON) — Ancient Roman law preserved nominal public ownership of enclosure land while imperial practice redistributed it to patricians. Theater ratio low (0.38) because enforcement is real and extraction mechanisms are transparent — not mystified by legal rhetoric. But the piton classification captures a degradation: the original republican commons logic has atrophied into institutional inertia (formal public ownership maintained while de facto private control operates). The constraint persists through coercive enforcement rather than legitimacy.
constraint_indexing:constraint_classification(public_land_enclosure_roman_cities, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal/civilizational view, enclosure of commons is seen as an inevitable consequence of state consolidation and resource scarcity: all large-scale societies require boundary-setting around public resources to prevent overuse (tragedy of the commons). This perspective risks naturalizing the constraint as a universal law of governance. However, the structural data reveals this as a false summit: the extraction is contingent on specific power asymmetries and legal mechanisms, not an inevitable natural law.
constraint_indexing:constraint_classification(public_land_enclosure_roman_cities, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_land_enclosure_roman_cities_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_land_enclosure_roman_cities, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_land_enclosure_roman_cities, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_land_enclosure_roman_cities, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(public_land_enclosure_roman_cities, TR),
    TR >= 0.70.

:- end_tests(public_land_enclosure_roman_cities_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts subsistence access from plebeians (trapped victims) while providing genuine coordination benefits to administration (infrastructure, taxation, governance predictability). The extraction is substantial but not maximal because coordination functions are real — enclosure is not pure rent-seeking but rather mixed extraction overlaid on genuine governance need. Suppression (0.72): High. Multiple suppression mechanisms: legal prohibition on commons access, military enforcement of boundaries, economic dependence created by subsistence redistribution, cultural narrative that urbanism requires hierarchy. Suppression increased over time (from 0.65 at T0 to 0.72 at T100) as enforcement mechanisms matured. Theater ratio (0.38): Moderate-low. The constraint exhibits relatively transparent extraction mechanisms — legal codes explicitly define boundaries, enforcement is visible (military patrol, judicial sanctions), and the redistribution mechanism is explicit (grain doles, tax collection). Not highly theatrical because the patrician-plebeian power asymmetry is openly acknowledged in law and practice. Theater increased slightly over time (from 0.22 to 0.38) as legal fiction of public ownership grew more elaborate, but the fundamental mechanisms remained transparent.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The plebeian sees a snare (no coordination function benefits them, pure extraction through subsistence denial). The patrician sees tangled rope (genuine coordination enabling agricultural investment, but asymmetric benefit capture). The imperial administration sees rope (pure coordination of governance apparatus). The municipal authority sees tangled rope from a different angle (must implement extraction while maintaining coordination). The republican legal fiction sees itself as piton (degraded institution maintained through coercive enforcement). The civilizational analyst risks seeing mountain (enclosure as inevitable governance requirement) but this is a false summit — the boundary itself is structural, but its asymmetric enforcement is contingent. The gap between plebeian-snare and patrician-tangled_rope is the diagnostic core: same constraint, opposite classifications, revealing the extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Plebeian directionality (d) is derived from trapped exit + victim status → high d → high f(d) → high experienced extraction chi. The agent cannot exit the constraint and bears all costs. Patrician directionality is derived from beneficiary status + mobile exit → low d → moderate f(d) → negative experienced extraction (benefit). Imperial administration is institutional/arbitrage → arbitrage exit + beneficiary status → very low d → institutional beneficiary level extraction (pure coordination benefit). Municipal authority is moderate/constrained + dual role (implementer and mediator) → mid-range d reflecting split benefits/costs. The perspectival gap reflects real differences in structural position: the plebeian's experienced extraction is highest (trapped, bearing costs); the patrician's is lowest (mobile, capturing benefits); the imperial administration's is least felt at all (arbitrage exit, pure coordination function). The municipal authority's moderate classification reflects their squeezed position between imperial mandate and plebeian resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   NOT RESOLVED. The constraint exhibits tangled rope classification at the analytical level (genuine coordination functions + asymmetric extraction) but snare classification from plebeian perspective reveals the tension. Mandatrophy would be resolved by establishing: (1) whether the coordination functions (grain distribution, infrastructure, taxation) could be delivered through non-extractive mechanisms (commons management, citizen assemblies), or (2) whether the extraction is necessary overhead for coordination at Roman urban scale. Current data suggests the extraction intensity exceeds coordination necessity — a smaller boundary (partial enclosure) could deliver infrastructure benefits while preserving subsistence commons access. This gap indicates tangled rope classification is appropriate but underestimates extraction contingency. A future analysis establishing whether plebeian subsistence access could coexist with imperial taxation would either confirm tangled rope (extraction is necessary) or degrade to snare (extraction is opportunistic).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_governance_necessity,
    'How much of the extraction in public land enclosure is necessary for legitimate urban governance vs. contingent on patrician power asymmetry?',
    'Comparative analysis of enclosure patterns across provinces with different patrician power concentrations; historical reconstruction of commons management capacity pre-enclosure',
    'If boundary is close to zero (governance necessity low): snare classification dominates, extractive intent is clear. If boundary is near current extraction level (governance necessity high): tangled rope classification is appropriate, coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_governance_necessity, empirical, 'Proportion of extraction necessary for governance vs. contingent on power asymmetry').

omega_variable(
    commons_degradation_causation,
    'Did pre-enclosure commons suffer from tragedy-of-the-commons degradation that enclosure legitimately solved, or did enclosure create artificial scarcity to justify extraction?',
    'Archaeological evidence of pre-enclosure land use patterns; historical records of commons management institutions; analysis of whether enclosure was preceded by measurable resource degradation or initiated degradation',
    'If commons were degrading pre-enclosure: mountain perspective gains force (enclosure was structural necessity). If enclosure preceded degradation: snare classification confirmed (extraction mechanism created problem it claims to solve).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_degradation_causation, empirical, 'Whether commons degradation preceded or followed enclosure').

omega_variable(
    plebeian_identity_lock_mechanism,
    'Is plebeian acceptance of enclosure enforced by external barriers (legal prohibition, military enforcement) or internalized through cultural narrative (urbanism requires hierarchy)?',
    'Historical analysis of resistance patterns, flight to rural communes, legal sanctions for commons trespass; cultural texts on urban legitimacy and plebeian self-concept',
    'If enforcement is primarily external (trapped): snare dominates. If internalized (identity_locked): plebeian perspective becomes rope or tangled_rope — agents perceive constraint as changeable in principle but cannot change it because identity is fused with submission.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plebeian_identity_lock_mechanism, empirical, 'Whether suppression is structural or internalized in plebeian identity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_land_enclosure_roman_cities, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ple_tr_t0, public_land_enclosure_roman_cities, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ple_tr_t50, public_land_enclosure_roman_cities, theater_ratio, 50, 0.3).
narrative_ontology:measurement(ple_tr_t100, public_land_enclosure_roman_cities, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(ple_be_t0, public_land_enclosure_roman_cities, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ple_be_t50, public_land_enclosure_roman_cities, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(ple_be_t100, public_land_enclosure_roman_cities, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_land_enclosure_roman_cities, resource_allocation).
narrative_ontology:affects_constraint(public_land_enclosure_roman_cities, roman_grain_supply_monopoly).
narrative_ontology:affects_constraint(public_land_enclosure_roman_cities, plebeian_client_patronage_network).

% DUAL FORMULATION NOTE:
% Public land enclosure is downstream of imperial consolidation (which centralizes governance) and upstream of grain supply monopoly (which depends on consolidated land control). The constraint family includes three stories: enclosure mechanism itself (this story), grain distribution coordination (separate ε ≈ 0.25, rope from most perspectives), and plebeian subsistence dependence (separate ε ≈ 0.75, snare from plebeian perspective). Each has distinct extraction/coordination balance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_land_enclosure_roman_cities, moderate, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
