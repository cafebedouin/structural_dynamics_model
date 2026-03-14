% ============================================================================
% CONSTRAINT STORY: emergency_response_reach
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emergency_response_reach, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: emergency_response_reach
 *   human_readable: Emergency Response Reach Constraint
 *   domain: public_safety/infrastructure
 *
 * SUMMARY:
 *   The emergency response reach constraint describes the structural inequity
 *   in access to emergency medical, fire, and law enforcement services
 *   distributed geographically. Urban and affluent suburban populations
 *   benefit from dense station networks and short response times; rural
 *   populations and marginalized urban communities face significantly longer
 *   response intervals and lower resource availability. The constraint
 *   exhibits tangled coordination and extraction: genuine coordination
 *   function exists (optimized dispatch, mutual aid, resource sharing)
 *   alongside systematic extraction (geography-based inequity, concentration
 *   of resources in profitable areas, structural disadvantage for populations
 *   with least bargaining power). The reach disparity increases over time as
 *   urban density concentrates population and funding, widening the gap.
 *   Theater ratio reflects performative aspects of coverage metrics that do
 *   not capture actual reach effectiveness for underserved populations.
 *
 * KEY AGENTS:
 *   - Rural populations: Primary victims (powerless/trapped) — bear full cost of geographic isolation with no exit option; response times routinely exceed 45+ minutes
 *   - Marginalized urban communities: Primary victims (powerless/trapped) — systematically under-served despite density; response bias toward affluent districts; trapped by socioeconomic and spatial barriers
 *   - Suburban communities: Mixed victims/beneficiaries (moderate/constrained) — benefit from proximity coordination but face longer waits than affluent areas; constrained by limited ability to enhance local resources independently
 *   - Urban affluent areas: Primary beneficiaries (institutional/arbitrage) — receive concentrated resources, shortest response times, highest service quality; can arbitrage between public and private providers
 *   - Emergency services institution: Institutional beneficiary (institutional/arbitrage) — coordinates system; benefits from control over resource allocation and deployment decisions
 *   - Reform coalition: Organized agents (organized/constrained) — equity advocates, emergency management modernizers building alternative pathways (telemedicine, community paramedicine, remote monitoring) with implicit sunset logic
 *   - Analytical observer: Civilizational view (analytical/analytical) — sees both genuine coordination function (dispatch optimization, mutual aid) and embedded extraction mechanism (geography-based inequity in resource concentration)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emergency_response_reach, 0.52).
domain_priors:suppression_score(emergency_response_reach, 0.58).
domain_priors:theater_ratio(emergency_response_reach, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emergency_response_reach, extractiveness, 0.52).
narrative_ontology:constraint_metric(emergency_response_reach, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(emergency_response_reach, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emergency_response_reach, tangled_rope).
narrative_ontology:human_readable(emergency_response_reach, "Emergency Response Reach Constraint").
narrative_ontology:topic_domain(emergency_response_reach, "public_safety/infrastructure").

domain_priors:requires_active_enforcement(emergency_response_reach).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emergency_response_reach, urban_populations).
narrative_ontology:constraint_beneficiary(emergency_response_reach, affluent_suburban_areas).
narrative_ontology:constraint_beneficiary(emergency_response_reach, emergency_service_institutions).
narrative_ontology:constraint_victim(emergency_response_reach, rural_populations).
narrative_ontology:constraint_victim(emergency_response_reach, marginalized_urban_communities).
narrative_ontology:constraint_victim(emergency_response_reach, remote_areas).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL RESIDENT (SNARE) — Faces geographic isolation from emergency response infrastructure with no meaningful exit option. Response times routinely exceed 45+ minutes; trapped in geography and constrained by limited alternative resources. Bears full cost of slow ambulance arrival, limited firefighting capacity, and sparse coverage.
constraint_indexing:constraint_classification(emergency_response_reach, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MARGINALIZED URBAN COMMUNITY (SNARE) — Systematically underserved by emergency response despite urban density. Response bias toward affluent districts, longer wait times in high-crime areas, police targeting patterns. Trapped by socioeconomic barriers and neighborhood discrimination. No exit option from geography without significant relocation cost.
constraint_indexing:constraint_classification(emergency_response_reach, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: SUBURBAN COMMUNITY (TANGLED ROPE) — Benefits from proximity-based service concentration but also benefits from genuine coordination of shared emergency resources (mutual aid agreements, regional dispatch). Experiences modest extraction through longer response times than affluent areas, but also genuine coordination function that reduces individual household costs.
constraint_indexing:constraint_classification(emergency_response_reach, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: EMERGENCY SERVICES INSTITUTION (ROPE) — Experiences the reach constraint as a coordination problem: optimizing coverage patterns, resource allocation, and dispatch efficiency. Benefits from institutional control over resource deployment; can arbitrage between jurisdictions and funding streams. The constraint enables their function.
constraint_indexing:constraint_classification(emergency_response_reach, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized actors (healthcare reformers, emergency management modernizers, equity advocates) see the reach gap as a temporary coordination failure with a sunset: remote station placement optimization, telemedicine triage protocols, and community paramedicine are building alternative verification pathways that bypass geography. The sunset clause is not yet explicit in policy but structurally implicit in the technology trajectory.
constraint_indexing:constraint_classification(emergency_response_reach, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY COVERAGE MODEL (PITON) — The dispatch-station model optimized for 20th-century conditions persists through institutional inertia despite being increasingly misaligned with actual settlement patterns. The model is maintained because alternatives haven't fully replaced it, not because it works. Theater ratio reflects the performative aspects of coverage metrics that don't track actual reach effectiveness.
constraint_indexing:constraint_classification(emergency_response_reach, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, emergency response reach exhibits genuine coordination (optimal placement, dispatch efficiency, mutual aid) alongside asymmetric extraction (geography-based inequity, resource concentration in profitable urban areas, structural disadvantage for marginalized populations). The constraint has both coordination function and embedded extraction mechanism.
constraint_indexing:constraint_classification(emergency_response_reach, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_response_reach_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emergency_response_reach, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergency_response_reach, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(emergency_response_reach, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(emergency_response_reach, TR),
    TR >= 0.70.

:- end_tests(emergency_response_reach_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The original research assessment (1990s-2000s) showed lower extractiveness (0.38) reflecting that geographic reach gaps were accepted as natural economic constraints. Rising to 0.52 reflects the constraint shifting from accepted natural limit to recognized extractive choice as technology enables alternatives (telemedicine, remote monitoring, distributed paramedicine). The extraction is partly embedded in institutional design (concentration of resources in urban areas despite stated equity goals) and partly in acceptance of that design as inevitable. Suppression (0.58): Moderate-high. Significant barriers include geographic barriers (isolation), economic barriers (funding concentration in profitable areas), institutional barriers (dispatch system optimization for density), and informational barriers (lack of visibility into disparities). Suppression is not total — rural residents have some exit options (relocation, private ambulance services, helicopter transport for critical cases) but at prohibitive cost. Theater ratio (0.48): Moderate and rising. The metric reflects growing performative content in coverage assessments: response time statistics that don't account for call-to-clinical-treatment latency, coverage maps that show geographic proximity without accounting for seasonal accessibility or staffing reality, and 'best possible time' standards that diverge from actual performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates profound perspectival divergence. Rural residents see a snare: trapped in geography with no exit and full extraction. Marginalized urban populations see a snare: systematic deprioritization and response bias despite density. Suburban communities see tangled rope: genuine coordination mixed with asymmetric distribution. Emergency services see rope: solving the coordination problem of optimal dispatch. Reform coalition sees scaffold: temporary problem with a sunset via technology and policy change. The legacy model sees piton: performative continuation of an outdated system. The analytical observer sees tangled rope: genuine coordination alongside embedded extraction. The perspectival gaps reveal the constraint's true structure: coordination function is real and necessary (optimal dispatch, mutual aid, resource sharing) but extraction mechanisms are also real and contingent (deliberate concentration of resources in profitable areas, institutional inertia preserving inefficient models, acceptance of disparity as natural).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the agent's structural position relative to the extraction flow. Rural and marginalized populations are structurally trapped (maximum d) — they cannot exit the geographic constraint and bear full cost of response delays. Suburban communities are constrained but mixed (moderate d) — they benefit from some coordination but experience worse outcomes than affluent areas. Urban affluent areas are beneficiaries with arbitrage options (low d) — they benefit from resource concentration and can supplement with private alternatives. Emergency services institution is beneficiary with arbitrage (low d) — coordinates system deployment and controls resource distribution. The piton classification reflects institutional inertia: the legacy dispatch-station model persists despite being misaligned with settlement patterns, maintained because alternatives haven't fully replaced it. The scaffold classification reflects that telemedicine, remote monitoring, and community paramedicine are building alternative reach mechanisms that could sunset the geographic constraint by making physical proximity less functionally necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that emergency response reach contains both genuine coordination and real extraction. The coordination component (dispatch optimization, mutual aid, regional resource sharing) would exist even in an equitable system — it solves a real problem of coordinating scarce emergency resources across space and time. The extraction component (geographic inequity, concentration in profitable areas, structural disadvantage for powerless populations) is a separate mechanism layered onto coordination. The constraint is tangled rope, not rope, because the extraction is not incidental but structural: the system's optimization function implicitly weights urban/affluent areas more heavily in resource allocation, and this weighting is actively maintained despite stated equity commitments. The reform coalition's scaffold perspective reveals that alternatives exist (telemedicine, remote monitoring, distributed paramedicine) that could provide coordination with lower extraction, suggesting the current extraction is not necessary to the coordination function. The mandatrophy is resolved not by eliminating either component but by making visible which component is coordination and which is extraction, enabling policy choices about whether the extraction is justified by coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    response_time_threshold,
    'What response time threshold distinguishes adequate emergency care from extractive under-resourcing?',
    'Epidemiological analysis of outcome correlation with response time across different emergency types; identification of time windows where intervention effectiveness drops sharply',
    'If threshold < 10 minutes for all emergencies: current rural system is catastrophically under-resourced. If threshold > 45 minutes for stable conditions: rural response times are adequate, reframing reach gap as theoretical rather than material.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(response_time_threshold, empirical, 'Time threshold for adequate emergency response').

omega_variable(
    geographic_necessity_of_extraction,
    'Is the disparity in response reach an inevitable consequence of population density economics, or is it a contingent result of funding allocation choices?',
    'International comparative analysis of emergency response reach across countries with similar geography but different funding models; assessment of alternative deployment strategies (regional hubs, air transport, distributed community paramedicine) in sparse-population contexts',
    'If inevitable: reach constraint is a quasi-mountain (natural economic law). If contingent: reach constraint is pure tangled rope with extractive component driven by resource allocation choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_necessity_of_extraction, conceptual, 'Whether response reach disparity is inevitable or contingent').

omega_variable(
    technology_substitution_feasibility,
    'Can telemedicine triage, remote monitoring, and autonomous dispatch substantially reduce the functional importance of physical station proximity, or are hands-on interventions too dependent on local presence?',
    'Longitudinal tracking of outcomes under telemedicine-enhanced dispatch in rural pilot programs; analysis of intervention types that can vs cannot be delegated to remote clinical guidance',
    'If feasible: scaffold sunset is real — technology will enable reach equalization. If limited: current geographic reach disparity is enduring, reframing reform as mitigation rather than resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_substitution_feasibility, empirical, 'Whether technology can substitute for geographic proximity in emergency response').

omega_variable(
    extraction_mechanism_internalization,
    'To what extent do residents in under-served areas perceive the reach gap as a natural consequence of geography versus an extractive institutional choice to concentrate resources in profitable urban areas?',
    'Qualitative research on how residents frame the reach constraint; analysis of whether perception patterns correlate with actual policy choices vs geographic necessity; comparison with resident perceptions in countries with more equitable reach',
    'If largely naturalized: residents are identity-locked into accepting the constraint, increasing measured suppression. If widely recognized as extractive: suppression is lower and organizing potential is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_internalization, conceptual, 'Whether reach disparity is perceived as natural or extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emergency_response_reach, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emr_tr_t0, emergency_response_reach, theater_ratio, 0, 0.32).
narrative_ontology:measurement(emr_tr_t5, emergency_response_reach, theater_ratio, 5, 0.4).
narrative_ontology:measurement(emr_tr_t10, emergency_response_reach, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(emr_be_t0, emergency_response_reach, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(emr_be_t5, emergency_response_reach, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(emr_be_t10, emergency_response_reach, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emergency_response_reach, resource_allocation).
narrative_ontology:affects_constraint(emergency_response_reach, healthcare_access_inequality).
narrative_ontology:affects_constraint(emergency_response_reach, disaster_response_capacity).
narrative_ontology:affects_constraint(emergency_response_reach, public_safety_inequity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
