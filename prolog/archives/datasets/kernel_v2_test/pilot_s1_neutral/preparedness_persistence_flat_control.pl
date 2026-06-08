% ============================================================================
% CONSTRAINT STORY: preparedness_persistence_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence_flat_control, []).

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
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_persistence_flat_control
 *   human_readable: Post-1953 Flood Preparedness as Institutionalized Practice
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   Post-1953 flood preparedness in the United States consolidated into an
 *   institutionalized practice of recurring drills, inspections, and
 *   hierarchical certification across federal, state, and local emergency
 *   management. The 1953 Kansas City flood and subsequent catastrophes
 *   established the doctrine that institutional memory of preparedness
 *   requires continuous activation — drills become the mechanism for keeping
 *   knowledge alive across generational discontinuity. The constraint
 *   exhibits a persistent tension: it is genuinely coordinative (pooling
 *   information, standardizing protocols, maintaining vigilance) while
 *   simultaneously extractive (imposing compliance burdens, concentrating
 *   authority, creating unequal risk exposure for newcomers and
 *   underrepresented populations). The constraint's evolution shows
 *   increasing theater ratio (0.35→0.71 over 70 years) as drills became
 *   institutionalized rituals, yet extractiveness remains moderate
 *   (0.28→0.41) because the coordination function retains measurable value.
 *   The constraint is most accurately classified as Tangled Rope from the
 *   primary analytical perspective: genuine coordination coexists with
 *   asymmetric extraction. However, seven distinct agent positions produce
 *   disparate classifications, revealing that the regime's coherence masks
 *   distributional inequity and performative degradation.
 *
 * KEY AGENTS:
 *   - Emergency Management Bureaucracy (institutional/arbitrage): Federal and state agencies that set drill mandates, allocate resources, and define compliance standards. Benefits from institutional continuity, budgeting authority, and coordinative control.
 *   - Frontline Responders (moderate/constrained): Fire, police, EMS, emergency management personnel who execute drills and response. Trapped by employment and local community commitment; bear uncompensated labor costs during drills.
 *   - Established Resident Communities (moderate/constrained): Long-term residents, native populations, and those with multigenerational participation in drill networks. Benefit from institutional knowledge transfer and integrated preparedness; pay the cost of compliance time.
 *   - Newcomers and Displaced Populations (powerless/trapped): Recent arrivals, climate migrants, communities outside traditional disaster management networks. Trapped by geography and institutional exclusion; bear maximum risk exposure from preparedness regime designed for stable populations.
 *   - Flood-Prone Municipalities (powerful/constrained): Local government bodies balancing federal mandate compliance against resource constraints and local priorities. Genuinely benefit from coordinative resource-sharing but constrained by federal requirements and liability exposure.
 *   - Climate-Adaptive Networks (organized/mobile): Climate scientists, real-time modeling initiatives, adaptive capacity researchers building alternative preparedness mechanisms. Mobile exit available through adoption of climate-responsive pathways that bypass historical drill regimes.
 *   - Analytical Observer (analytical/analytical): Civilizational-level perspective risking naturalization of contingent institutional choices as immutable laws of disaster management.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence_flat_control, 0.38).
domain_priors:suppression_score(preparedness_persistence_flat_control, 0.42).
domain_priors:theater_ratio(preparedness_persistence_flat_control, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence_flat_control, extractiveness, 0.38).
narrative_ontology:constraint_metric(preparedness_persistence_flat_control, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(preparedness_persistence_flat_control, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence_flat_control, tangled_rope).
narrative_ontology:human_readable(preparedness_persistence_flat_control, "Post-1953 Flood Preparedness as Institutionalized Practice").
narrative_ontology:topic_domain(preparedness_persistence_flat_control, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_persistence_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(preparedness_persistence_flat_control, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence_flat_control, emergency_management_bureaucracy).
narrative_ontology:constraint_beneficiary(preparedness_persistence_flat_control, flood_control_agencies).
narrative_ontology:constraint_beneficiary(preparedness_persistence_flat_control, institutional_memory_continuity).
narrative_ontology:constraint_victim(preparedness_persistence_flat_control, frontline_responders).
narrative_ontology:constraint_victim(preparedness_persistence_flat_control, vulnerable_communities).
narrative_ontology:constraint_victim(preparedness_persistence_flat_control, unprepared_newcomers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINTEGRATED NEWCOMER (SNARE) — New residents, migrants, displaced persons, or communities outside the drill cycle bear maximum cost when flooding occurs. Trapped by geography and lack of institutional connection. The preparedness regime assumes stable resident populations who participate in multigenerational drills; newcomers inherit zero institutional memory and must discover preparedness through catastrophe. No exit from exposure; extraction takes the form of unequal risk.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FRONTLINE RESPONDER (SNARE) — Fire, police, EMS, and local emergency management personnel are structurally embedded in the preparedness machinery. They invest training time and personal capacity in drill participation with no guarantee of adequate resource allocation when actual floods occur. Constrained by employment and local commitment, not entirely trapped. However, the constraint extracts uncompensated labor during drills and crisis response while the benefits (reduced population losses, institutional continuity) accrue to the broader system. Drills maintain their labor without scaling resources.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: FLOOD-PRONE MUNICIPALITY (TANGLED ROPE) — Local government bodies in flood-prone regions genuinely benefit from preparedness coordination (reduced disaster response costs, federal reimbursement eligibility, insurance discounts tied to drill compliance, shared resource networks with neighboring municipalities). But they also bear extraction: federal mandates for drill frequency, inspection costs, infrastructure hardening requirements, and liability exposure for preparedness failures. The coordination function is real (distributed risk reduction); the extraction is embedded in the same mechanism (federal control of resource allocation, insurance cost transfers).
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EMERGENCY MANAGEMENT BUREAUCRACY (ROPE) — Federal and state emergency management agencies genuinely coordinate collective preparedness. They pool information, standardize protocols, allocate resources, and reduce duplication. These agencies also benefit from the constraint: budgets, staffing, and institutional mandate flow from the preparedness regime. But experienced extraction is low because the beneficiary position is stable and the agencies have full arbitrage (they can reshape the system). The constraint is genuinely coordinative from their perspective — the coordination benefits exceed costs.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DRILL RITUAL INFRASTRUCTURE (PITON) — The post-1953 practice of recurring drills and inspections has degraded from a functional verification mechanism (1953-1985: drills discovered actual gaps in preparedness, leading to concrete infrastructure improvements) into a performative ritual. Modern drills often follow scripted scenarios, report rubber-stamped completion, and occupy responder time without revealing or fixing new vulnerabilities. Theater ratio (0.68) reflects this: drills produce compliance documentation and institutional legitimacy rather than adaptive knowledge. The infrastructure persists through inertia — dismantling it would expose institutional unpreparedness, so performance continues.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CLIMATE-ADAPTIVE PREPAREDNESS MOVEMENT (SCAFFOLD) — Organized networks (climate science communities, adaptive capacity researchers, participatory risk mapping initiatives, real-time inundation modeling) are building alternative preparedness mechanisms that do not depend on static historical drill cycles. These movements see post-1953 drills as an outdated coordination mechanism increasingly misaligned with shifting flood patterns. The scaffold perspective treats the constraint as temporary: as climate-responsive, data-driven preparedness pathways mature (5-15 years), the historical drill regime loses its coordination function and becomes vestigial. Mobile exit available for organizations adopting adaptive pathways.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL RECURRENCE VIEW (MOUNTAIN) — From a civilizational/universal perspective, flood recurrence is a geophysical constant independent of human institutions. Every region with episodic floods must maintain some form of preparedness discipline across generations, or institutional memory decays and risk compounds. The post-1953 regime appears as an inevitable structural response to an immutable constraint: the need to transmit survival knowledge across generational discontinuity. From this view, drills and inspections are as necessary as weather itself. However, this perspective risks naturalizing a contingent institutional arrangement — the specific mechanisms (federal drill mandates, liability-based enforcement, scripted scenarios) are contingent choices, not natural laws. The engine will likely compute this as a false summit.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(preparedness_persistence_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(preparedness_persistence_flat_control, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(preparedness_persistence_flat_control, TR),
    TR >= 0.70.

:- end_tests(preparedness_persistence_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint imposes real coordination value (shared risk reduction, standardized response, information pooling, federal resource allocation frameworks) estimated at 0.50-0.60 of the system's output. The extraction component (0.38) arises from: (1) uncompensated responder labor during drills (estimated 15% of responder time); (2) municipal compliance costs not fully reimbursed (estimated 8-12% of municipal emergency management budgets); (3) unequal risk exposure for newcomers excluded from multigenerational networks (estimated 10-15% population-level mortality risk differential); (4) resource concentration toward institutionally sophisticated municipalities (estimated 5% allocation inefficiency). The constraint's extractiveness has risen over 70 years as drills became routine bureaucratic obligations rather than evidence-gathering exercises, increasing the ratio of ritual to adaptive learning. Suppression (0.42): Moderate. Barriers to exit include federal mandate enforceability (loss of FEMA reimbursement eligibility, insurance penalties), social pressure within disaster-prone communities (norm-based compliance), and geographic constraint (one cannot relocate the disaster risk). However, suppression is not total: some communities have reduced drill intensity, and resource-rich jurisdictions have exit options through privatized preparedness. Theater ratio (0.68): High and rising. Historical drills (1953-1970s) had functional content — they discovered actual gaps (missing communications infrastructure, inadequate evacuation routes, responder training deficiencies) that led to concrete improvements. Modern drills (2000s-present) increasingly follow scripted scenarios and produce compliance documentation with minimal discovery of new vulnerabilities. The rising trajectory (0.35→0.71) reflects this degradation: drills now occupy responder time and institutional attention without proportional learning return.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the full span of DR classifications across agent positions. The unintegrated newcomer experiences extraction approaching snare levels because preparedness institutions were designed assuming stable generational cohorts. The frontline responder experiences snare dynamics — labor extraction without commensurate benefit, constrained exit despite awareness of the unfairness. The flood-prone municipality experiences tangled rope: genuine coordination benefits (shared risk reduction, federal resource access, insurance discounts tied to preparedness compliance) coexist with extraction (federal mandate compliance, infrastructure hardening requirements, liability exposure). The emergency management bureaucracy experiences rope: the constraint is coordinative from their vantage, and they hold sufficient power (arbitrage options, resource control) that extraction runs toward them rather than away. The drill infrastructure itself has become piton: the machinery persists through institutional inertia despite degraded functional content. The climate-adaptive movement experiences the constraint as a temporary structure (scaffold) — drills are an intermediate-term coordination mechanism being superseded by adaptive pathways. The civilizational analytical observer risks classifying the constraint as mountain (immutable flood recurrence requires continuous institutional memory), but this naturalizes contingent political choices (federal drill mandates, insurance-based enforcement, centralized certification) as natural law. The false summit detector should flag this: the constraint's persistence is institutional, not physical.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position: power level determines capacity for action; exit options determine flexibility and mobility; beneficiary/victim status determines extraction direction. Beneficiaries of the regime (emergency management bureaucracy: d ≈ 0.1, institutional/arbitrage) experience low effective extraction because they control resource allocation and can reshape the system. Agents with constrained exit and victim status (frontline responders: d ≈ 0.75, moderate/constrained) experience high extraction through uncompensated labor and mandatory participation. Powerless trapped newcomers (d ≈ 1.0, powerless/trapped) experience maximal extraction as they bear risk without benefit of institutional knowledge. Organized agents with mobile exit (climate networks: d ≈ 0.4, organized/mobile) experience moderate extraction because they have pathways to exit the regime. The magnitude of effective extraction is modulated by spatial scope: national scope means verification is difficult and enforcement costs are distributed, slightly amplifying extraction for all agents. Temporal scope (generational) means some extraction (uncompensated responder labor) accumulates silently, undetected by individual biographical experience.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy — the violation of mandate by operation — appears in this constraint as the drift from the founding mandate ('maintain institutional memory of preparedness across generations') toward the actual operation ('maintain institutional legitimacy and bureaucratic continuity through compliance documentation'). The foundational mandate is not formally obsolete: flood recurrence remains, generational discontinuity persists, institutional memory does decay. However, the mechanism has drifted: drills now verify compliance more than readiness, and the regime serves its own perpetuation more than adaptive preparedness. The mandatrophy is not total (coordination value remains non-negligible), which is why the constraint remains Tangled Rope rather than collapsing to Snare. However, the rising theater ratio (0.35→0.71) and the scaffold perspective (climate-adaptive alternatives maturing) both indicate that the constraint's mandate-to-operation gap is widening. A mandatrophy_resolved declaration is not yet warranted because the underlying coordination problem (maintaining preparedness across generational discontinuity in flood-prone regions) remains live and unsolved. The constraint has degraded from a rope-like coordination mechanism toward a piton-like ritual, but the problem it was designed for has not been resolved by alternatives — it has merely been displaced toward climate-adaptive modeling efforts that remain institutional infants (5-15 years to maturity). The mandatrophy is in process: the old mechanism is dying while the alternative is being born.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generational_knowledge_transfer_empirics,
    'Do multigenerational drill cycles actually transmit preparedness knowledge effectively, or do they primarily transmit institutional legitimacy and compliance documentation?',
    'Longitudinal analysis of responder knowledge retention across generational boundaries; correlation between drill participation and actual adaptive capacity in novel flood scenarios; comparison of prepared communities with high drill participation vs. communities relying on distributed knowledge without centralized drills',
    'If effective: constraint is genuine Rope from analytical perspective. If largely performative: constraint is Piton with false-summit coverage — institutional inertia wearing the mask of natural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_knowledge_transfer_empirics, empirical, 'Whether drill cycles effectively transmit preparedness knowledge across generations').

omega_variable(
    newcomer_integration_mechanism,
    'What is the actual mechanism by which newcomers and displaced populations become integrated into the preparedness regime, and at what time cost?',
    'Cohort analysis of newcomer participation in drills within first 1, 3, and 5 years of arrival; documentation of informal knowledge transfer pathways vs. formal drill enrollment; survey of preparedness knowledge among long-term residents vs. residents of <5 years in flood-prone regions',
    'If integration is rapid and effective: snare classification overstates the extraction. If integration requires years or relies on unmandated informal networks: snare classification is correct — the regime benefits established residents and the bureaucracy while imposing unequal risk on newcomers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(newcomer_integration_mechanism, empirical, 'Integration mechanism for newcomers into preparedness regime').

omega_variable(
    resource_allocation_asymmetry,
    'Are federal/state resources allocated proportionally to flood risk and responder need, or do they concentrate in regions with higher institutional capacity to request and document preparedness activities?',
    'Analysis of FEMA/state emergency management resource allocation vs. historical flood frequency; comparison of drill-intensive vs. drill-minimal regions; tracking of post-flood disaster assistance amounts and compliance timeline',
    'If proportional: tangled rope classification accurate — asymmetry is built into the mechanism. If concentrated in high-capacity regions: extraction component is larger than (0.38) suggests; constraint may classify as closer to Snare from the low-capacity municipality perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_asymmetry, empirical, 'Distribution of resources across municipal capacity and flood risk').

omega_variable(
    theater_ratio_degradation_trajectory,
    'What is the historical trajectory of theater ratio in flood preparedness drills — has performativity increased relative to functional verification, or has the ratio remained stable as technology and methodology improved?',
    'Document analysis of drill reports 1953-present; coding of drill content for verification vs. performance characteristics; interview data from responders across decades on drill realism and adaptive value; comparison of drill-identified gaps vs. actual vulnerability discoveries in post-flood assessments',
    'If theater ratio has risen above 0.60: Piton classification is correct and temporal measurement trajectory should show increasing ratio. If ratio has remained low: the regime may retain coordinative function across generations, shifting classification toward Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_degradation_trajectory, empirical, 'Historical degradation or stability of performative vs. functional content in drills').

omega_variable(
    climate_shifting_baseline_problem,
    'Are post-1953 flood preparedness drills designed to adapt to shifting flood patterns under climate change, or do they assume stationary hydrology from historical records?',
    'Review of national and state drill protocols for climate scenario incorporation; analysis of federal flood maps and their update frequency relative to climate projections; assessment of whether new drills include novel flood magnitudes/locations or repeat historical patterns',
    'If drills remain stationary-hydrology-based: scaffold perspective is correct — the constraint''s coordination function is eroding as physical reality diverges from institutional assumptions. If drills incorporate climate adaptation: constraint retains coordination function and may shift toward Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_shifting_baseline_problem, empirical, 'Adaptation of preparedness drills to non-stationary flood hydrology').

omega_variable(
    false_summit_natural_necessity,
    'Does the post-1953 preparedness regime represent a necessary institutional response to immutable flood recurrence, or does it naturalize contingent political choices (federal mandates, insurance-based enforcement, liability frameworks) as inescapable law?',
    'Comparative analysis of preparedness regimes across democracies and non-democracies; examination of alternative coordination mechanisms (decentralized community-based, market-driven, volunteer networks) and their empirical outcomes; documentation of policy choices made post-1953 that were framed as natural but were explicitly selected over alternatives',
    'If regime is truly natural: mountain classification is justified. If alternatives exist with comparable or superior outcomes: constraint is false-summit candidate — analytically positioned as natural law but actually contingent institutional arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_necessity, conceptual, 'Whether the constraint represents natural necessity or naturalized contingent choices').

omega_variable(
    catastrophic_memory_decay,
    'What is the actual timeline for institutional memory decay in flood preparedness across generational discontinuity, and does the post-1953 drill regime prevent it?',
    'Historical analysis of preparedness following major floods: comparison of communities that implemented post-1953 drills vs. those that relied on oral tradition or written records; examination of preparation quality before and after generational transitions in leadership; study of communities after >20 years without major flooding to assess whether preparedness persists',
    'If drills prevent decay: tangled rope classification is correct. If preparedness degrades despite drills or if non-drill-based communities maintain equivalent preparedness: extraction component may be higher than estimated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophic_memory_decay, empirical, 'Timeline and mechanisms of institutional memory decay in preparedness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence_flat_control, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence_flat_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(prep_tr_t15, preparedness_persistence_flat_control, theater_ratio, 15, 0.52).
narrative_ontology:measurement(prep_tr_t30, preparedness_persistence_flat_control, theater_ratio, 30, 0.68).
narrative_ontology:measurement(prep_tr_t70, preparedness_persistence_flat_control, theater_ratio, 70, 0.71).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence_flat_control, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(prep_be_t15, preparedness_persistence_flat_control, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(prep_be_t30, preparedness_persistence_flat_control, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(prep_be_t70, preparedness_persistence_flat_control, base_extractiveness, 70, 0.41).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence_flat_control, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(prep_su_t15, preparedness_persistence_flat_control, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(prep_su_t30, preparedness_persistence_flat_control, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(prep_su_t70, preparedness_persistence_flat_control, suppression_requirement, 70, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence_flat_control, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence_flat_control, 0.12).
narrative_ontology:affects_constraint(preparedness_persistence_flat_control, community_disaster_collective_action).
narrative_ontology:affects_constraint(preparedness_persistence_flat_control, climate_adaptive_preparedness_systems).
narrative_ontology:affects_constraint(preparedness_persistence_flat_control, federal_mandates_local_capacity_mismatch).

% DUAL FORMULATION NOTE:
% Post-1953 flood preparedness is the institutionalized coordination mechanism for a family of related constraints: community collective action for disaster response (who organizes, who benefits, who bears costs), local-federal capacity mismatch (resource allocation, mandate compliance), and climate-adaptive alternatives (whether drills remain adequate as hydrological baselines shift). Each story in the family has distinct ε values reflecting different aspects of preparedness: drill theater (high ε, performance), emergency response coordination (moderate ε, mixed), resource allocation asymmetry (moderate ε, extraction), and climate adaptation obsolescence (high ε, constraint becoming misaligned). The stories are linked by institutional dependency: federal drill mandates drive local response infrastructure, which shapes community preparedness culture, which affects climate adaptation adoption. This story captures the broad institutionalized constraint; the siblings capture specific mechanism breakdowns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_persistence_flat_control, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
