% ============================================================================
% CONSTRAINT STORY: infrastructure_confidence_displacement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_infrastructure_confidence_displacement, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: infrastructure_confidence_displacement
 *   human_readable: Infrastructure Confidence Displacement in Disaster Preparedness Systems
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   Infrastructure confidence displacement describes the systematic process
 *   by which disaster preparedness institutions maintain public confidence in
 *   infrastructure resilience while deferring maintenance, suppressing
 *   failure documentation, and preventing community-level adaptive capacity
 *   development. The constraint operates through a coordination mechanism
 *   (standardized emergency response requires public compliance with
 *   institutional directives) that embeds extraction (vulnerable populations
 *   bear disproportionate disaster costs while maintenance budgets are
 *   reallocated to visible projects that sustain confidence narratives). The
 *   displacement is multi-layered: it displaces honest risk communication
 *   with reassurance, displaces institutional memory of past failures with
 *   optimism about current readiness, and displaces distributed community
 *   resilience investments with centralized infrastructure dependence. The
 *   theater_ratio (0.58) reflects that much disaster preparedness activity is
 *   performative: readiness certifications, compliance audits, and tabletop
 *   exercises that maintain confidence without improving actual protective
 *   capacity. The extractiveness (0.62) has increased over the measurement
 *   interval as climate-driven disaster frequency intensifies the gap between
 *   infrastructure confidence narratives and actual system performance, while
 *   suppression (0.48) has increased as institutional mechanisms to prevent
 *   honest failure documentation have been formalized.
 *
 * KEY AGENTS:
 *   - Vulnerable Populations: Primary victim (powerless/trapped) — bear maximum disaster costs from infrastructure failures that confidence narratives prevented them from preparing for; no exit option during crisis events
 *   - Municipal Emergency Response Staff: Secondary victim and beneficiary (moderate/constrained) — benefit from coordination function (protocols, equipment channels) but bear extraction costs when confidence narratives prevent honest risk communication with communities they serve
 *   - Infrastructure Maintenance Institutions: Primary beneficiary (institutional/arbitrage) — capture budget allocations and public legitimacy by maintaining infrastructure confidence; can exit to private sector or different jurisdictions
 *   - Emergency Management Agencies: Primary beneficiary (institutional/arbitrage) — benefit from centralized disaster response authority justified by infrastructure confidence narratives
 *   - Community Resilience Networks: Organized agents (organized/mobile) — building alternative preparedness pathways with implicit sunset logic as infrastructure failures accumulate
 *   - Federal Emergency Management Framework: Institutional actor (institutional/constrained) — maintains performative compliance rituals as actual coordination function atrophies (piton perspective)
 *   - Institutional Memory Commons: Abstract victim (powerless/trapped) — collective epistemic good with no advocate; extraction occurs through systematic suppression of failure documentation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(infrastructure_confidence_displacement, 0.62).
domain_priors:suppression_score(infrastructure_confidence_displacement, 0.48).
domain_priors:theater_ratio(infrastructure_confidence_displacement, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(infrastructure_confidence_displacement, extractiveness, 0.62).
narrative_ontology:constraint_metric(infrastructure_confidence_displacement, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(infrastructure_confidence_displacement, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(infrastructure_confidence_displacement, tangled_rope).
narrative_ontology:human_readable(infrastructure_confidence_displacement, "Infrastructure Confidence Displacement in Disaster Preparedness Systems").
narrative_ontology:topic_domain(infrastructure_confidence_displacement, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(infrastructure_confidence_displacement).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(infrastructure_confidence_displacement, '48560c29-7cae-479e-b743-c3e16f76dc07').
narrative_ontology:cs_kernel_codification('48560c29-7cae-479e-b743-c3e16f76dc07', implicit).
narrative_ontology:cs_authority_grounding('48560c29-7cae-479e-b743-c3e16f76dc07', practice).
narrative_ontology:cs_created_at('48560c29-7cae-479e-b743-c3e16f76dc07', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(infrastructure_confidence_displacement, infrastructure_maintenance_institutions).
narrative_ontology:constraint_beneficiary(infrastructure_confidence_displacement, emergency_management_agencies).
narrative_ontology:constraint_victim(infrastructure_confidence_displacement, vulnerable_populations).
narrative_ontology:constraint_victim(infrastructure_confidence_displacement, institutional_memory_commons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(infrastructure_confidence_displacement, municipal_emergency_staff).
narrative_ontology:constraint_victim(infrastructure_confidence_displacement, municipal_emergency_staff).
narrative_ontology:constraint_victim(infrastructure_confidence_displacement, community_resilience_networks).
narrative_ontology:constraint_vindicates(infrastructure_confidence_displacement, technical_solutionism_doctrine).
narrative_ontology:constraint_vindicates(infrastructure_confidence_displacement, infrastructure_sufficiency_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Geographic and economic immobility during disaster events. Told infrastructure will protect them, experience system failure during crisis, bear maximum costs (displacement, property loss, health impacts) from failures that confidence narratives prevented them from preparing for. No exit option during crisis; relocation before crisis often economically infeasible.
narrative_ontology:constraint_stakeholder(infrastructure_confidence_displacement, vulnerable_populations, payer,
    powerless, immediate, trapped, local).

% Implement disaster response protocols and communicate with public during emergencies. Benefit from standardized procedures, equipment procurement channels, and career stability within emergency management systems. Bear costs when institutional confidence narratives prevent honest risk communication with communities they serve. Face career penalties for public dissent about system limitations. Constrained exit within emergency management career tracks.
narrative_ontology:constraint_stakeholder(infrastructure_confidence_displacement, municipal_emergency_staff, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(infrastructure_confidence_displacement, municipal_emergency_staff, beneficiary).

% Manage and maintain disaster-critical infrastructure (levees, power grids, water systems, evacuation routes). Capture budget allocations, public legitimacy, and career stability by maintaining infrastructure confidence narratives. Benefit from coordination function: public trust enables compliance with emergency directives. Can exit to private sector consulting, different jurisdictions, or infrastructure-adjacent industries when institutional pressures intensify. Net beneficiary position.
narrative_ontology:constraint_stakeholder(infrastructure_confidence_displacement, infrastructure_maintenance_institutions, beneficiary,
    institutional, biographical, arbitrage, national).

% Set disaster preparedness policies, allocate emergency resources, coordinate multi-jurisdictional responses. Benefit from centralized authority justified by infrastructure confidence narratives. Capture federal funding flows, regulatory authority, and institutional legitimacy. Control disaster readiness certification processes and post-disaster review mechanisms. Arbitrage-level exit to homeland security consulting, private emergency management, or political appointments.
narrative_ontology:constraint_stakeholder(infrastructure_confidence_displacement, emergency_management_agencies, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(infrastructure_confidence_displacement, emergency_management_agencies, beneficiary).

% Grassroots organizations building alternative disaster preparedness: mutual aid networks, distributed supply caches, neighborhood communication systems, skill-sharing for emergency response. Organized through collective action but initially resource-limited. Bear costs of infrastructure confidence displacement when centralized narratives crowd out community-level investment. Mobile exit through network participation — can build resilience capacity independent of institutional frameworks. See implicit sunset as disaster failures accumulate and validate distributed approaches.
narrative_ontology:constraint_stakeholder(infrastructure_confidence_displacement, community_resilience_networks, payer,
    organized, generational, mobile, national).

% Formalized disaster response protocols, resource allocation formulas, readiness certification standards operating at federal scale. The coordination function (standardized response procedures enabling multi-state resource sharing) has atrophied into performance (compliance audits, tabletop exercises, post-disaster reviews that produce unimplemented recommendations). Maintains theater of preparedness through institutional inertia despite repeated failures to protect vulnerable populations. Constrained exit — cannot simply abandon federal disaster authority.
narrative_ontology:constraint_stakeholder(infrastructure_confidence_displacement, federal_emergency_framework, agenda_setter,
    institutional, civilizational, constrained, continental).

% Abstract collective epistemic good with no advocate. Extraction occurs through systematic suppression of infrastructure failure documentation: post-disaster reviews that minimize institutional responsibility, readiness certifications that ignore known vulnerabilities, budget processes that reward confidence-maintaining narratives over honest risk assessment. The commons bears the cost of repeated rediscovery of known failure modes. Cannot exit, cannot organize, cannot speak. Kept in stakeholder array for narrative completeness but excluded from directionality computation via agent=false.
narrative_ontology:constraint_stakeholder(infrastructure_confidence_displacement, institutional_memory_commons, payer,
    powerless, civilizational, trapped, continental).
narrative_ontology:stakeholder_non_agent(infrastructure_confidence_displacement, institutional_memory_commons).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Disaster response coordination: centralized infrastructure (levees, power grids, evacuation routes) and standardized emergency protocols enable resource pooling, multi-jurisdictional cooperation, and public compliance with evacuation orders. Some level of public confidence in infrastructure is functionally necessary for coordination during time-critical emergencies.
% TRANSFER_FUNCTION: Budget allocations, public legitimacy, and career stability flow from vulnerable populations and institutional memory commons to infrastructure maintenance institutions and emergency management agencies. The confidence narrative justifies resource concentration in centralized institutional structures while preventing investment in distributed community resilience capacity. Disaster costs (displacement, property loss, health impacts) flow disproportionately to vulnerable populations when infrastructure fails.
% ABSENT_VOICES: Vulnerable populations whose geographic and economic immobility prevent participation in infrastructure planning processes. Communities displaced by past disasters whose experiences are not systematically incorporated into readiness assessments. Emergency response staff whose ground-level knowledge of system limitations is suppressed by institutional confidence-maintenance pressures. These voices would object to confidence narratives that prevent honest risk communication and community-level adaptation.
% DISAPPEARANCE_RATIONALE: If infrastructure confidence displacement disappeared overnight — if institutions communicated honest risk assessments, documented failures systematically, and supported community adaptive capacity — arrangements would rearrange substantially: vulnerable populations would invest in distributed preparedness rather than trusting institutional assurances; budget allocations would shift from confidence-maintaining projects to honest vulnerability reduction; community resilience networks would receive legitimacy and resources currently concentrated in centralized frameworks; emergency response protocols would incorporate realistic failure scenarios rather than ideal-case assumptions. The world would not stay the same — stakeholders' arrangements depend on the confidence narrative's persistence.
% FOUNDING_PROBLEM: The founding problem was genuine coordination failure in disaster response: historically, communities lacked standardized evacuation procedures, resource-sharing mechanisms across jurisdictions, or centralized infrastructure for rapid emergency response. Early disaster preparedness institutions were built to solve this real coordination problem by creating shared infrastructure and protocols that enabled effective multi-jurisdictional response.
% FOUNDING_PROBLEM_CORROBORATION: Infrastructure maintenance institutions and emergency management agencies attest that the founding coordination problem remains live: disasters still require centralized response mechanisms and public compliance with institutional directives. Community resilience networks and post-disaster researchers attest that the founding problem's nature has shifted: modern disasters are characterized by infrastructure failures rather than coordination absence, and centralized dependence prevents adaptive capacity development. Municipal emergency staff (moderate power, constrained exit) occupy an intermediate position: they attest that some centralized coordination is still necessary but that institutional confidence narratives now prevent rather than enable effective response. The contested status reflects genuine disagreement about whether current institutional structures are solving or have become the coordination problem.
narrative_ontology:disappearance_verdict(infrastructure_confidence_displacement, world_rearranges).
narrative_ontology:founding_problem_status(infrastructure_confidence_displacement, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE POPULATIONS (SNARE) — Trapped by geographic and economic immobility during disaster events. The infrastructure confidence displacement extracts maximum cost from this group: they are told the infrastructure will protect them, experience system failure during crisis, and have no exit option. The coordination story (infrastructure provides safety) is cover for extraction (deferred maintenance redirected to visible projects).
constraint_indexing:constraint_classification(infrastructure_confidence_displacement, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MUNICIPAL STAFF (TANGLED ROPE) — Constrained by institutional protocols and resource allocation. They benefit from the coordination function (standardized response procedures, equipment procurement channels) but bear extraction costs when the infrastructure-confidence narrative prevents honest risk communication. They see both coordination and extraction in the same mechanism.
constraint_indexing:constraint_classification(infrastructure_confidence_displacement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INFRASTRUCTURE INSTITUTIONS (ROPE) — Primary beneficiaries. They capture budget allocations, public legitimacy, and career stability by maintaining infrastructure confidence narratives. Experience the constraint as coordination: communicating system readiness enables public compliance with evacuation orders and emergency protocols. Net beneficiary position with arbitrage-level exit — can shift to consulting, private sector, or different jurisdictions.
constraint_indexing:constraint_classification(infrastructure_confidence_displacement, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMMUNITY NETWORKS (SCAFFOLD) — Organized grassroots groups building alternative preparedness pathways (mutual aid networks, distributed supply caches, neighborhood communication systems). See infrastructure confidence displacement as a temporary problem with an implicit sunset: as climate disasters intensify and infrastructure failures accumulate, communities are developing parallel resilience structures that bypass centralized institutional narratives. Mobile exit options through network participation.
constraint_indexing:constraint_classification(infrastructure_confidence_displacement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL FRAMEWORK (PITON) — The coordination function (standardized disaster response protocols, resource allocation formulas) has atrophied into performance. Post-disaster reviews, readiness certifications, and compliance audits persist through institutional inertia despite repeated failures to protect vulnerable populations. The framework maintains the theater of preparedness while the actual protective function degrades. Theater ratio drives this classification.
constraint_indexing:constraint_classification(infrastructure_confidence_displacement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the constraint exhibits both genuine coordination (disaster response does require centralized infrastructure and public compliance mechanisms) and substantial extraction (the confidence narrative systematically displaces honest risk assessment, institutional memory of failure, and community-level adaptive capacity). The analytical classification matches the claimed type.
constraint_indexing:constraint_classification(infrastructure_confidence_displacement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(infrastructure_confidence_displacement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(infrastructure_confidence_displacement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(infrastructure_confidence_displacement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(infrastructure_confidence_displacement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(infrastructure_confidence_displacement, TR),
    TR >= 0.70.

:- end_tests(infrastructure_confidence_displacement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Substantial. The primary extraction mechanism is the displacement of honest risk assessment and community adaptive capacity by infrastructure confidence narratives that benefit maintenance institutions while leaving vulnerable populations exposed. The value reflects that this is not maximum extraction (some protective function exists) but significantly more than legitimate coordination cost. The increase over the interval (0.48 → 0.62) tracks the growing gap between infrastructure confidence claims and actual system performance under climate-intensified disaster frequency. Suppression (0.48): Moderate. Suppression operates through multiple mechanisms: budget allocation processes that reward confidence-maintaining projects over honest vulnerability assessment; institutional incentives against documenting infrastructure failures; career risks for emergency managers who communicate limitations publicly; geographic and economic barriers preventing vulnerable populations from relocating. The increase over the interval (0.38 → 0.48) reflects formalization of failure-suppression mechanisms. Theater ratio (0.58): Moderate-high. Much disaster preparedness activity is performative rather than functional: compliance certifications that audit paperwork rather than actual capacity; tabletop exercises that rehearse ideal scenarios rather than realistic failures; post-disaster reviews that produce recommendations which are not implemented. The increase over the interval (0.35 → 0.58) reflects the growing divergence between the ritual of preparedness and the reality of repeated infrastructure failures.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a characteristic tangled_rope perspectival structure: beneficiaries see coordination (rope), trapped victims see pure extraction (snare), analytical observers see both functions in the same mechanism (tangled_rope). The infrastructure maintenance institutions experience the confidence narrative as a coordination tool — it enables public compliance with evacuation orders and emergency protocols. Vulnerable populations experience it as extraction — the narrative prevents them from developing adaptive capacity while they bear maximum costs during infrastructure failures. Municipal staff experience both: they benefit from standardized response protocols while bearing the cost of being unable to communicate honestly about system limitations. The community resilience networks see a sunset through the accumulation of failures: each disaster that contradicts institutional confidence claims strengthens the case for distributed preparedness alternatives. The federal framework sees its own degradation (piton): the coordination function has atrophied into performance but the institutional structure persists. The analytical observer classifies this as tangled_rope because both the coordination function (disaster response does require some centralized infrastructure and public compliance) and the extraction function (confidence displacement systematically harms vulnerable populations and institutional memory) are real and operate through the same structural mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to the constraint. Infrastructure maintenance institutions and emergency management agencies are primary beneficiaries: they collect budget allocations, public legitimacy, and career stability from maintaining infrastructure confidence, and they have arbitrage-level exit options (can move to private sector, consulting, or different jurisdictions). This positions them toward the low-d (beneficiary) end of the spectrum. Vulnerable populations are primary victims: they bear disproportionate disaster costs from infrastructure failures that confidence narratives prevented them from preparing for, and they are trapped by geographic and economic immobility during crisis events. This positions them toward the high-d (target) end of the spectrum. Municipal emergency response staff occupy a mixed position: they benefit from the coordination function (standardized protocols, equipment procurement) but bear costs when confidence narratives prevent honest risk communication. Their constrained exit options (career penalties for institutional dissent, limited mobility within emergency management career tracks) place them at moderate d. Community resilience networks have mobile exit options through network participation and organized power through collective action, positioning them toward low d despite being nominally victims of the displacement. The institutional memory commons is an abstract victim with no advocate and no exit — conceptually at maximum d, though difficult to operationalize in the directionality computation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the tangled_rope classification requires BOTH coordination and extraction to be real and to operate through the same mechanism — not merely coordination with side effects, and not merely extraction with a coordination cover story. The coordination function is genuine: disaster response does require centralized infrastructure, standardized protocols, and public compliance with institutional directives. Honest communication of every infrastructure limitation during a crisis could produce coordination failures (resource hoarding, evacuation route overload, panic-driven poor decisions). But the extraction function is also genuine: the infrastructure confidence narrative systematically displaces honest risk assessment, suppresses institutional memory of failures, prevents community-level adaptive capacity development, and leaves vulnerable populations disproportionately exposed during infrastructure failures. The beneficiaries (maintenance institutions, emergency management agencies) capture rents through this displacement. The victims (vulnerable populations, institutional memory commons) bear costs that exceed the necessary coordination overhead. The analytical classification as tangled_rope is confirmed by the structural data: requires_active_enforcement (true), beneficiaries declared, victims declared, extractiveness substantially above the Boltzmann floor for information_standard coordination type. The perspectival gap (beneficiaries see rope, victims see snare, analytical sees both) is the diagnostic signature of tangled_rope rather than an error to be resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_confidence_naturalization,
    'Is infrastructure confidence displacement a natural consequence of disaster psychology (people need reassurance to function), or a constructed institutional arrangement that benefits maintenance bureaucracies at the expense of adaptive capacity?',
    'Cross-national comparison of disaster preparedness systems with different institutional structures; correlation analysis between infrastructure confidence narratives and post-disaster outcome disparities by vulnerability class',
    'If natural: the constraint is closer to mountain (inevitable psychological requirement). If constructed: the constraint is confirmed as tangled_rope with identifiable beneficiaries extracting from institutional memory displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_confidence_naturalization, conceptual, 'Whether infrastructure confidence is natural psychological requirement or institutional construction').

omega_variable(
    community_resilience_sunset_timeline,
    'At what threshold of repeated infrastructure failure do community resilience networks achieve sufficient scale and legitimacy to displace centralized institutional narratives?',
    'Longitudinal tracking of mutual aid network formation, resource autonomy, and public trust metrics in regions with repeated disaster experiences; identification of tipping points where communities stop deferring to institutional readiness claims',
    'If threshold is low (2-3 major failures): scaffold perspective confirmed with near-term sunset. If threshold is high (decade+ of failures): communities remain captured by institutional confidence narratives despite evidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(community_resilience_sunset_timeline, empirical, 'Timeline threshold for community network displacement of institutional confidence').

omega_variable(
    honest_risk_communication_coordination_cost,
    'Does honest communication of infrastructure limitations increase or decrease coordination efficiency during disasters?',
    'Experimental comparison of evacuation compliance, resource hoarding behavior, and mutual aid activation under confidence-maintaining vs honest-risk messaging; analysis of panic vs preparation responses',
    'If honest communication decreases coordination: the confidence displacement has genuine coordination function (justifies tangled_rope rather than snare). If honest communication increases coordination: the displacement is pure extraction masked as coordination necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(honest_risk_communication_coordination_cost, empirical, 'Whether honest risk communication improves or degrades disaster coordination').

omega_variable(
    institutional_memory_commons_measurement,
    'How do you measure extraction from an abstract collective good (institutional memory of infrastructure failure) that has no advocate and no voice?',
    'Development of proxy metrics: frequency of repeated failures in same infrastructure subsystems, time-to-rediscovery of known vulnerabilities in post-disaster reviews, correlation between disaster severity and institutional surprise level',
    'If measurable: institutional memory displacement can be quantified and attributed. If not measurable: the victim remains conceptual rather than empirical, weakening the extraction claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_memory_commons_measurement, empirical, 'Operationalization of institutional memory commons as measurable victim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(infrastructure_confidence_displacement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infra_conf_theater_t0, infrastructure_confidence_displacement, theater_ratio, 0, 0.35).
narrative_ontology:measurement(infra_conf_theater_t3, infrastructure_confidence_displacement, theater_ratio, 3, 0.42).
narrative_ontology:measurement(infra_conf_theater_t6, infrastructure_confidence_displacement, theater_ratio, 6, 0.51).
narrative_ontology:measurement(infra_conf_theater_t9, infrastructure_confidence_displacement, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(infra_conf_extract_t0, infrastructure_confidence_displacement, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(infra_conf_extract_t3, infrastructure_confidence_displacement, base_extractiveness, 3, 0.53).
narrative_ontology:measurement(infra_conf_extract_t6, infrastructure_confidence_displacement, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(infra_conf_extract_t9, infrastructure_confidence_displacement, base_extractiveness, 9, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(infra_conf_suppress_t0, infrastructure_confidence_displacement, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(infra_conf_suppress_t3, infrastructure_confidence_displacement, suppression_requirement, 3, 0.42).
narrative_ontology:measurement(infra_conf_suppress_t6, infrastructure_confidence_displacement, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(infra_conf_suppress_t9, infrastructure_confidence_displacement, suppression_requirement, 9, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(infrastructure_confidence_displacement, information_standard).

% DUAL FORMULATION NOTE:
% Infrastructure confidence displacement is a constraint family candidate: the general pattern (confidence narrative displacing honest assessment) could decompose into domain-specific constraints (levee confidence in flood zones, grid confidence in hurricane regions, water supply confidence in drought regions) with different ε values reflecting different institutional capture levels and failure frequencies. This story models the general pattern; domain-specific decompositions would be separate stories linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
