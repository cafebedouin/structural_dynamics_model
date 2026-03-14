% ============================================================================
% CONSTRAINT STORY: irrigation_infrastructure_path_dependence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irrigation_infrastructure_path_dependence, []).

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
 *   constraint_id: irrigation_infrastructure_path_dependence
 *   human_readable: Irrigation Infrastructure Path Dependence
 *   domain: water/agriculture/economics/infrastructure
 *
 * SUMMARY:
 *   Irrigation infrastructure creates structural path dependence when
 *   historical investment, institutional arrangements, and legal water rights
 *   allocations lock systems into specific configurations even as ecological
 *   conditions, population distributions, and technological capabilities
 *   change. The constraint exhibits a tangled rope character: the
 *   infrastructure genuinely coordinates water distribution (enabling
 *   agriculture that would be impossible under rainfall alone), yet
 *   simultaneously extracts through monopolistic control of allocation,
 *   rent-seeking by large landholders, suppression of alternative water
 *   systems, and intergenerational cost transfer to downstream communities
 *   and future water users. The extractiveness has grown from 0.32 to 0.58
 *   over 40 years as groundwater depletion has reduced system flexibility and
 *   intensified allocation conflicts. Theater has risen from 0.38 to 0.58 as
 *   administrative processes increasingly serve legitimacy functions rather
 *   than actual coordination. The constraint demonstrates how historical
 *   infrastructure investments create institutional dependencies that persist
 *   long after the underlying conditions have changed.
 *
 * KEY AGENTS:
 *   - Smallholder farmers: Primary victims (powerless/trapped) — dependent on canal access for subsistence, face water cutoffs and bureaucratic rationing with no alternatives
 *   - Downstream communities: Secondary victims (powerless/trapped) — geographic position locks them into receiving residual water after upstream users; generations of reduced flow and ecosystem damage
 *   - Canal bureaucracy: Institutional actor (institutional/constrained) — maintains allocation system; benefits from continuous budgets and institutional continuity; constrained by maintenance costs and political pressure
 *   - Large landholders: Primary beneficiaries (organized/arbitrage) — hold historical water rights and preferential allocation; can invest in alternatives and negotiate directly; captured early benefits of infrastructure
 *   - Reform coalition: Organized actors (organized/mobile) — see institutional path dependence as temporary problem with solution pathways through technology and policy
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing institutional path dependence as irreversible law of infrastructure economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irrigation_infrastructure_path_dependence, 0.58).
domain_priors:suppression_score(irrigation_infrastructure_path_dependence, 0.62).
domain_priors:theater_ratio(irrigation_infrastructure_path_dependence, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irrigation_infrastructure_path_dependence, extractiveness, 0.58).
narrative_ontology:constraint_metric(irrigation_infrastructure_path_dependence, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(irrigation_infrastructure_path_dependence, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irrigation_infrastructure_path_dependence, tangled_rope).
narrative_ontology:human_readable(irrigation_infrastructure_path_dependence, "Irrigation Infrastructure Path Dependence").
narrative_ontology:topic_domain(irrigation_infrastructure_path_dependence, "water/agriculture/economics/infrastructure").

domain_priors:requires_active_enforcement(irrigation_infrastructure_path_dependence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irrigation_infrastructure_path_dependence, large_landholders).
narrative_ontology:constraint_beneficiary(irrigation_infrastructure_path_dependence, canal_bureaucracy).
narrative_ontology:constraint_beneficiary(irrigation_infrastructure_path_dependence, established_agricultural_interests).
narrative_ontology:constraint_victim(irrigation_infrastructure_path_dependence, smallholder_farmers).
narrative_ontology:constraint_victim(irrigation_infrastructure_path_dependence, downstream_communities).
narrative_ontology:constraint_victim(irrigation_infrastructure_path_dependence, future_water_users).
narrative_ontology:constraint_victim(irrigation_infrastructure_path_dependence, alternative_agricultural_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLHOLDER FARMER (SNARE) — Structurally dependent on canal water for subsistence; trapped by land tenure arrangements, debt cycles, and lack of alternative water sources. Cannot exit the irrigation system without abandoning livelihood. The canal bureaucracy controls water allocation and timing, creating maximum extraction with minimal coordination benefit. No alternative technologies or resources available.
constraint_indexing:constraint_classification(irrigation_infrastructure_path_dependence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DOWNSTREAM COMMUNITY (SNARE) — Trapped by geographic position and historical water rights allocation that prioritizes upstream users. Bears costs of reduced water flow, seasonal scarcity, and ecosystem degradation. No meaningful exit option or voice in allocation decisions. Extraction is multigenerational — water scarcity compounds across time.
constraint_indexing:constraint_classification(irrigation_infrastructure_path_dependence, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: CANAL BUREAUCRACY (TANGLED ROPE) — Coordinates water distribution, maintenance, and conflict resolution (genuine coordination function). Simultaneously extracts through fees, water rationing, and institutional dependency. Requires active enforcement to maintain allocation rules. Benefits from the system's persistence through budgets and institutional continuity; constrained by political pressure and maintenance costs.
constraint_indexing:constraint_classification(irrigation_infrastructure_path_dependence, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE LANDHOLDER (ROPE) — Primary beneficiary with historical water rights and preferential allocation. Experiences the constraint as pure coordination — infrastructure enables their productivity. Maintains arbitrage options (can invest in alternative technologies, switch crops, negotiate directly with authorities). Faces minimal suppression; benefits are clear and continuous.
constraint_indexing:constraint_classification(irrigation_infrastructure_path_dependence, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Environmental NGOs, water rights advocates, and progressive agricultural economists see the irrigation path dependence as a temporary institutional arrangement with a sunset clause. Groundwater depletion, climate change, and alternative water technologies (drip irrigation, rainwater harvesting) create pressure for system redesign. Emergence of water markets and efficiency standards offers exit pathways. Theater is moderate — reform rhetoric persists even when implementation lags.
constraint_indexing:constraint_classification(irrigation_infrastructure_path_dependence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: COLONIAL WATER ALLOCATION LEGACY (PITON) — Historical water rights hierarchies established during colonial periods persist through institutional inertia despite changed ecological conditions and population distributions. The formal water allocation rules are maintained through ritual and legal precedent, but actual water availability has degraded. Theater is high — the system performs legitimacy through administrative processes while material coordination has largely failed. Maintenance is driven by elite institutional interests with vested power, not by functional necessity.
constraint_indexing:constraint_classification(irrigation_infrastructure_path_dependence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / IRREVERSIBILITY (MOUNTAIN) — From a civilizational view, once large-scale irrigation infrastructure is constructed, the economic sunk costs and institutional coordination create path dependence that appears nearly irreversible at the civilizational timescale. Reallocation of water rights, infrastructure redesign, or technological transition all face massive institutional resistance. The constraint appears as a natural law of infrastructure economics — sunk costs lock systems into trajectories. However, this masks the contingency: the path dependence is institutional and political, not thermodynamic.
constraint_indexing:constraint_classification(irrigation_infrastructure_path_dependence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irrigation_infrastructure_path_dependence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(irrigation_infrastructure_path_dependence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(irrigation_infrastructure_path_dependence, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(irrigation_infrastructure_path_dependence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(irrigation_infrastructure_path_dependence, TR),
    TR >= 0.70.

:- end_tests(irrigation_infrastructure_path_dependence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The infrastructure provides genuine coordination value (enabling irrigation on a large scale) but the allocation mechanism extracts through monopolistic control. Large landholders captured the highest-value water rights during construction and maintain these through institutional hierarchy. Smallholders and downstream communities bear costs of scarcity and rationing. The rising trajectory reflects that as water becomes scarcer, the same allocation rules produce higher extraction — scarcity increases the value of controlled access. Suppression (0.62): Moderate-high. Legal water rights frameworks entrench allocation patterns; lack of alternative water sources for smallholders; information asymmetries about hydrological conditions favor bureaucracy and large landholders; debt and land tenure trap smallholders into dependence. Yet suppression is not total — some smallholders have organized for reform, some communities have invested in boreholes, some states have begun reallocating rights. Theater (0.58): Moderate and rising. Canal maintenance and allocation management have real coordination function, but as system degrades, performance increasingly depends on administrative ritual rather than actual water delivery. Administrative fees and licensing requirements serve more to maintain bureaucratic power than to coordinate allocation efficiently. The theater rise from 0.38 to 0.58 tracks the growing gap between the system's formal legitimacy and its actual performance.
 *
 * PERSPECTIVAL GAP:
 *   The most dramatic perspectival gap is between the large landholder's Rope (coordination mechanism enabling productivity) and the smallholder farmer's Snare (pure extraction with no benefit). Both perspectives measure the same infrastructure from radically different structural positions. The canal bureaucracy's Tangled Rope view legitimates continued control by emphasizing coordination function — a partially true claim that obscures the asymmetric extraction. The reform coalition's Scaffold view is prospective: it sees the path dependence eroding through technology and climate pressure, but this sunset clause depends on institutional choices not yet made. The piton view of colonial water rights reveals how historical allocations persist as institutional theater long after material conditions have changed. The mountain view risks naturalizing contingent institutional arrangements as irreversible laws of infrastructure economics.
 *
 * DIRECTIONALITY LOGIC:
 *   Large landholders have high historical water rights, minimal resource barriers to exit (can shift crops, invest in alternatives, negotiate directly), and derive clear benefits — their d value is low (~0.10-0.20), yielding negative or minimal effective extraction (χ). The canal bureaucracy has constrained exit (embedded in institutional structure, dependent on state budgets) and mixed benefits (institutional continuity and budgets, but facing political pressure) — d value moderate (~0.45-0.55), yielding moderate χ. Smallholder farmers have trapped exit (no alternative water sources, debt-dependent, land tenure constraints) and are victims — d value high (~0.85-0.95), yielding high χ. Downstream communities similarly trapped — d value high (~0.88), yielding high χ. The directionality calculation shows that extraction flows from those with trapped exit options to those with arbitrage options, mediated through institutional hierarchy.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the tangled rope classification is diagnostically correct: the system performs genuine coordination (enables large-scale irrigation) while simultaneously extracting (asymmetric allocation of benefits and costs). Distinguishing the two functions is essential to understanding the constraint's durability. The coordination function is real — rapid transition away from the system would create agricultural crisis and threaten food security for the regions dependent on it. Yet the extraction is also real — the allocation mechanism systematically favors large landholders and penalizes smallholders and downstream users. The constraint cannot be analyzed as pure coordination (Rope) because the extraction is structural and intergenerational. It cannot be analyzed as pure extraction (Snare) because the infrastructure does enable productivity that would be impossible under rainfall alone. The tangled rope classification holds both truths: the system is both necessary and exploitative. Reform requires not dismantling the coordination function but restructuring the allocation mechanism to distribute its benefits more equitably — exactly the mandate that path dependence prevents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    groundwater_depletion_rate_threshold,
    'At what rate of groundwater depletion does the irrigation system transition from path-dependent (hard to change) to irreversible (impossible to maintain)?',
    'Hydrogeological modeling of aquifer recharge rates vs extraction rates; historical precedents of irrigation system collapse or forced transformation',
    'If threshold < 20 years: systems are already in crisis phase and must transform rapidly, making extraction mechanisms less stable. If threshold > 50 years: path dependence remains effective and extraction mechanisms persist through the measurement interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(groundwater_depletion_rate_threshold, empirical, 'Critical timeline for irrigation system viability given groundwater depletion').

omega_variable(
    alternative_technology_adoption_barriers,
    'Are barriers to drip irrigation, rainwater harvesting, or other water-efficient technologies primarily technical, economic, institutional, or identity-based?',
    'Comparative analysis of adoption rates across regions with similar water scarcity but different institutional/cultural contexts; controlled trials of technology adoption with varying support mechanisms',
    'If primarily technical/economic: path dependence can be overcome through investment and policy. If primarily institutional/identity: breaking path dependence requires cultural and political transformation, making constraints more durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_technology_adoption_barriers, empirical, 'Nature of barriers to alternative agricultural technologies').

omega_variable(
    water_rights_reallocation_feasibility,
    'Can historical water rights be reallocated without triggering acute conflict or institutional collapse?',
    'Case studies of water rights redistribution (India''s inter-state water disputes, California water markets, Middle Eastern agreements); analysis of legal mechanisms for reallocation',
    'If feasible: scaffold sunset clause is real, and system can transform without catastrophic extraction. If infeasible: path dependence is reinforced and extraction mechanisms remain stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(water_rights_reallocation_feasibility, preference, 'Political feasibility of reallocating historical water rights').

omega_variable(
    institutional_inertia_vs_ecological_necessity,
    'Does the canal bureaucracy maintain the current allocation system primarily to preserve institutional power or because genuine coordination problems would emerge from rapid change?',
    'Comparative analysis of system performance with and without bureaucratic control; study of community-managed irrigation systems; examination of whether bureaucratic interests align with or diverge from coordination requirements',
    'If primarily institutional inertia: the piton classification is accurate and theater ratio is high. If partially genuine coordination: tangled rope classification is more accurate and some extraction is legitimately coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_inertia_vs_ecological_necessity, empirical, 'Whether bureaucratic maintenance reflects genuine coordination needs or pure institutional inertia').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irrigation_infrastructure_path_dependence, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irrig_tr_t0, irrigation_infrastructure_path_dependence, theater_ratio, 0, 0.38).
narrative_ontology:measurement(irrig_tr_t20, irrigation_infrastructure_path_dependence, theater_ratio, 20, 0.5).
narrative_ontology:measurement(irrig_tr_t40, irrigation_infrastructure_path_dependence, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(irrig_be_t0, irrigation_infrastructure_path_dependence, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(irrig_be_t20, irrigation_infrastructure_path_dependence, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(irrig_be_t40, irrigation_infrastructure_path_dependence, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irrigation_infrastructure_path_dependence, resource_allocation).
narrative_ontology:affects_constraint(irrigation_infrastructure_path_dependence, groundwater_depletion_crisis).
narrative_ontology:affects_constraint(irrigation_infrastructure_path_dependence, water_rights_legal_hierarchy).
narrative_ontology:affects_constraint(irrigation_infrastructure_path_dependence, agricultural_technology_lock_in).

% DUAL FORMULATION NOTE:
% The irrigation infrastructure path dependence is upstream of specific water scarcity crises and technology adoption barriers. The constraint represents the meta-institutional structure that coordinates and extracts through allocation rules, while specific physical constraints (aquifer depletion) and technology constraints (adoption barriers) represent downstream effects and potential escape routes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irrigation_infrastructure_path_dependence, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
