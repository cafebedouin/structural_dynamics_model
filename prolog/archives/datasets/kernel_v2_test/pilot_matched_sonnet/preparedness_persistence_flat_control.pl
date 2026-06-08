% ============================================================================
% CONSTRAINT STORY: preparedness_persistence_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
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
 *   The post-1953 flood preparedness regime in the Netherlands
 *   institutionalized a shared commitment that recurring drills, inspections,
 *   and infrastructure maintenance would prevent catastrophic flooding across
 *   generations. The 1953 North Sea flood killed 1,836 people and catalyzed
 *   the Delta Works infrastructure program and a comprehensive preparedness
 *   bureaucracy. Sixty years later, the regime persists: mandatory evacuation
 *   drills, annual dike inspections, emergency management agency budgets, and
 *   a cultural narrative that preparedness prevents catastrophe. This
 *   constraint exhibits the full range of DR types depending on observer
 *   position. The coastal population benefits from reduced flood risk but
 *   bears the cost of recurring drill participation. Municipal budgets face
 *   extraction (preparedness competes with other needs) alongside
 *   coordination (regional standards reduce planning complexity). Emergency
 *   management agencies are primary beneficiaries — institutional continuity
 *   and budget justification flow from the preparedness mandate. The
 *   preparedness policy coalition sees a scaffold with a real sunset: as
 *   infrastructure matures and climate adaptation advances, the
 *   drill-and-inspect regime should transition to automated monitoring. The
 *   inspection bureaucracy, from a long time horizon, has become partly piton
 *   — much of the ritual is performative as tacit knowledge atrophies with
 *   generational turnover. The analytical observer risks naturalizing the
 *   regime as an immutable law of coastal settlement, but the structural data
 *   reveals this as a false summit: the specific institutional form is
 *   contingent, not natural.
 *
 * KEY AGENTS:
 *   - Coastal Population: Mixed beneficiary/victim (powerless to moderate / trapped to constrained) — benefits from reduced flood risk but bears drill participation costs and property restrictions
 *   - Municipal Budget Officers: Secondary victim (moderate/constrained) — face extraction through budget allocation mandates but benefit from coordination via regional standards
 *   - Emergency Management Agencies: Primary beneficiary (institutional/arbitrage) — institutional continuity, budget justification, and professional identity flow from preparedness mandate
 *   - Infrastructure Maintenance Sector: Secondary beneficiary (organized/mobile) — contracts for dike inspection, pump maintenance, and infrastructure upgrades
 *   - Preparedness Policy Coalition: Organized agents (organized/mobile) — disaster researchers, infrastructure planners, insurance industry building alternative pathways with sunset logic
 *   - Inspection Bureaucracy: Institutional actor (institutional/arbitrage) — maintains performative ritual; sees own process as partly degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent requirements of coastal settlement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence_flat_control, 0.28).
domain_priors:suppression_score(preparedness_persistence_flat_control, 0.35).
domain_priors:theater_ratio(preparedness_persistence_flat_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence_flat_control, extractiveness, 0.28).
narrative_ontology:constraint_metric(preparedness_persistence_flat_control, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(preparedness_persistence_flat_control, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence_flat_control, scaffold).
narrative_ontology:human_readable(preparedness_persistence_flat_control, "Post-1953 Flood Preparedness as Institutionalized Practice").
narrative_ontology:topic_domain(preparedness_persistence_flat_control, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_persistence_flat_control).
narrative_ontology:has_sunset_clause(preparedness_persistence_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence_flat_control, '291d58a7-7272-4610-9b56-c788a75a8771').
narrative_ontology:cs_kernel_codification('291d58a7-7272-4610-9b56-c788a75a8771', formalized).
narrative_ontology:cs_authority_grounding('291d58a7-7272-4610-9b56-c788a75a8771', lineage).
narrative_ontology:cs_interpretation_layer_present('291d58a7-7272-4610-9b56-c788a75a8771').
narrative_ontology:cs_created_at('291d58a7-7272-4610-9b56-c788a75a8771', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(preparedness_persistence_flat_control, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence_flat_control, coastal_population).
narrative_ontology:constraint_beneficiary(preparedness_persistence_flat_control, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_persistence_flat_control, infrastructure_maintenance_sector).
narrative_ontology:constraint_victim(preparedness_persistence_flat_control, municipal_budgets).
narrative_ontology:constraint_victim(preparedness_persistence_flat_control, drill_fatigued_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence_flat_control, municipal_budget_officers).
narrative_ontology:constraint_victim(preparedness_persistence_flat_control, coastal_population).
narrative_ontology:constraint_victim(preparedness_persistence_flat_control, municipal_budget_officers).
narrative_ontology:constraint_vindicates(preparedness_persistence_flat_control, preparedness_prevents_catastrophe_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents of flood-vulnerable zones who benefit from reduced catastrophic risk but bear the cost of recurring drill participation, property restrictions, and evacuation mandates. Cannot exit the preparedness regime without relocating from coastal geography. Experience diminishing coordination benefit as the 1953 catastrophe recedes into historical memory and drills become routine rather than urgent.
narrative_ontology:constraint_stakeholder(preparedness_persistence_flat_control, coastal_population, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence_flat_control, coastal_population, beneficiary).

% Local government officials who allocate budgets for preparedness infrastructure, drill coordination, and emergency management. Face extraction through legal mandates and political pressure to maintain preparedness spending, which competes with other municipal needs (schools, healthcare, infrastructure). Also benefit from coordination: regional preparedness standards reduce planning complexity and enable mutual aid agreements. Constrained by legal requirements but have some discretion in implementation.
narrative_ontology:constraint_stakeholder(preparedness_persistence_flat_control, municipal_budget_officers, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence_flat_control, municipal_budget_officers, beneficiary).

% National and regional agencies responsible for flood preparedness policy, drill coordination, and disaster response. Primary beneficiaries: institutional continuity, budget justification, professional identity, and political influence flow from the preparedness mandate. Set the agenda for drill frequency, inspection standards, and infrastructure priorities. Have arbitrage exit options — can shift focus to other disaster types or policy domains if flood preparedness becomes politically untenable.
narrative_ontology:constraint_stakeholder(preparedness_persistence_flat_control, emergency_management_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence_flat_control, emergency_management_agencies, beneficiary).

% Private contractors and engineering firms that perform dike inspections, pump maintenance, and infrastructure upgrades. Benefit from recurring contracts funded by preparedness budgets. Have mobile exit options — can shift to other infrastructure domains if flood preparedness spending declines. Organized through industry associations that lobby for preparedness funding.
narrative_ontology:constraint_stakeholder(preparedness_persistence_flat_control, infrastructure_maintenance_sector, beneficiary,
    organized, biographical, mobile, national).

% Disaster researchers, infrastructure planners, insurance industry analysts, and climate adaptation advocates who see the drill-and-inspect regime as transitional. Building alternative pathways: automated monitoring systems, risk-based land-use planning, insurance-based risk transfer, managed retreat from high-vulnerability zones. See a real sunset: as infrastructure matures and climate adaptation advances, recurring drills should become obsolete. Have mobile exit options and agency to build alternative institutions.
narrative_ontology:constraint_stakeholder(preparedness_persistence_flat_control, preparedness_policy_coalition, observer,
    organized, generational, mobile, national).

% The institutional apparatus that conducts annual dike inspections, certifies compliance, and maintains preparedness records. From a long time horizon, sees its own function as partly degraded: checklists are completed and certifications issued, but the tacit knowledge of actual flood response has atrophied as the 1953 generation retires. Maintains the theater of preparedness through institutional inertia. Has arbitrage exit options but persists because the ritual maintains legitimacy.
narrative_ontology:constraint_stakeholder(preparedness_persistence_flat_control, inspection_bureaucracy, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Subset of coastal population who experience recurring drills as extraction of time and attention with diminishing coordination benefit. Trapped in mandatory participation cycles with no exit option short of relocation. From an immediate time horizon, the drills are pure extraction — the coordination function (maintaining response capacity) is not salient because catastrophe is not imminent. Maximum experienced extraction.
narrative_ontology:constraint_stakeholder(preparedness_persistence_flat_control, drill_fatigued_residents, payer,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The preparedness regime coordinates flood response capacity across jurisdictions, maintains infrastructure inspection standards, and transmits (or attempts to transmit) tacit knowledge of disaster response across generational turnover. Solves the collective action problem of maintaining readiness for low-frequency, high-consequence events where individual incentives favor free-riding.
% TRANSFER_FUNCTION: Transfers time and attention from residents (drill participation), budget resources from municipalities (preparedness spending), and political capital from elected officials (justifying preparedness mandates) to emergency management agencies (institutional continuity and budget justification) and infrastructure maintenance contractors (recurring inspection and upgrade contracts).
% ABSENT_VOICES: Future generations who will bear flood risk if preparedness atrophies, and alternative disaster preparedness frameworks (insurance-based risk transfer, managed retreat, automated monitoring) that are excluded from the drill-and-inspect paradigm. The preparedness regime's institutional beneficiaries have strong voice; those who would prefer alternative approaches (land-use restrictions instead of drills, insurance instead of infrastructure) are underrepresented in policy formation.
% DISAPPEARANCE_RATIONALE: If the preparedness regime disappeared overnight, flood response capacity would degrade, infrastructure inspection would become ad hoc, and coordination across jurisdictions would fragment. Emergency management agencies would lose institutional continuity and budget justification. Infrastructure maintenance contracts would shift to reactive repair rather than preventive maintenance. The world would rearrange itself — not necessarily toward catastrophe (alternative preparedness frameworks exist), but the current institutional arrangements depend on the regime's persistence.
% FOUNDING_PROBLEM: The 1953 North Sea flood killed 1,836 people in the Netherlands and revealed catastrophic gaps in flood preparedness: inadequate dike maintenance, no coordinated evacuation procedures, fragmented emergency response, and no institutional memory of the 1916 flood. The founding problem was genuine: coastal settlement in subsiding, storm-surge-vulnerable geography requires some form of preparedness coordination to prevent mass casualties.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is contested between two camps. Emergency management agencies and disaster researchers attest that the problem remains live: climate change is increasing storm surge risk, subsidence continues, and generational turnover erodes tacit knowledge — preparedness must be actively maintained. Infrastructure planners and insurance industry analysts attest that the problem has shifted: the Delta Works infrastructure has dramatically reduced catastrophic flood risk, and the drill-and-inspect regime is addressing a 1953 problem with 2013 tools — the founding problem (inadequate infrastructure and no coordination) has been solved, but the institutional apparatus persists. The contestation is not over whether flood risk exists (it does), but over whether the specific institutional form (recurring drills, inspection bureaucracy) remains the appropriate response or has become extractive theater maintained by institutional inertia.
narrative_ontology:disappearance_verdict(preparedness_persistence_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence_flat_control, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DRILL-FATIGUED RESIDENT (SNARE) — Trapped in mandatory participation cycles with no exit option. Experiences recurring drills as extraction of time and attention with diminishing coordination benefit as catastrophe recedes into historical memory. Maximum experienced extraction from immediate perspective.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MUNICIPAL BUDGET OFFICER (TANGLED ROPE) — Constrained by legal mandates and political pressure to maintain preparedness infrastructure. Benefits from coordination (shared regional standards reduce planning complexity) but bears extraction (budget allocation to preparedness competes with other municipal needs). Mixed coordination and extraction.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EMERGENCY MANAGEMENT AGENCY (ROPE) — Primary beneficiary with arbitrage exit options. Experiences the constraint as coordination: standardized protocols enable resource sharing and mutual aid across jurisdictions. Institutional continuity and budget justification flow from preparedness mandate. Net beneficiary.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PREPAREDNESS POLICY COALITION (SCAFFOLD) — Organized agents (disaster researchers, infrastructure planners, insurance industry) see preparedness as transitional: the goal is to build resilient infrastructure and land-use patterns that reduce catastrophic vulnerability, making recurring drills obsolete. Sunset logic: as physical infrastructure matures and climate adaptation advances, the drill-and-inspect regime should transition to automated monitoring and risk-based maintenance.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INSPECTION BUREAUCRACY / DEGRADED FUNCTION (PITON) — From a long time horizon, much of the inspection ritual has become performative: checklists are completed, certifications issued, but the tacit knowledge of actual flood response has atrophied as the 1953 generation retires. The bureaucracy maintains the theater of preparedness through institutional inertia, not because the drills meaningfully test response capacity. Theater ratio drives piton classification.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of preparedness maintenance is inherent to living in flood-vulnerable geography: the physical reality of storm surge and subsidence creates an irreducible coordination requirement. This perspective sees preparedness as an immutable property of coastal settlement. However, the structural data contradicts this — the specific institutional form (recurring drills, inspection bureaucracy) is contingent, not natural. The engine's false summit detector will identify this as naturalization of a constructed institutional arrangement.
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
    constraint_indexing:constraint_classification(preparedness_persistence_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.28): Moderate-low. The preparedness regime extracts time, attention, and budget resources, but much of this extraction is legitimate coordination cost — maintaining flood infrastructure and response capacity in vulnerable geography. The extraction has increased over the 60-year interval as the catastrophe recedes into historical memory and the coordination function becomes less salient relative to the bureaucratic overhead. The value reflects that the regime is not primarily extractive but has accumulated extractive overhead as it matured. Suppression (0.35): Moderate. Significant barriers to exit include legal mandates for drill participation, property restrictions in flood zones, and political pressure to maintain preparedness budgets. But suppression has decreased over the interval — early post-1953 enforcement was more coercive (mandatory evacuations, property condemnations), while contemporary enforcement relies more on normalization and cultural narrative. The declining trajectory reflects that the regime has shifted from active coercion to internalized commitment. Theater ratio (0.58): Moderate-high. A substantial portion of the preparedness regime is performative: checklists completed, certifications issued, drills conducted, but the tacit knowledge of actual flood response has atrophied as the 1953 generation retires. The theater ratio has increased steadily over the interval — in 1953, drills tested real response capacity with living memory of catastrophe; by 2013, much of the ritual maintains institutional legitimacy rather than functional readiness. The rising trajectory reflects Goodhart drift: the preparedness metrics (drill frequency, inspection completion rates) have become targets, and the underlying function (actual response effectiveness) has degraded.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from a single set of base properties. Emergency management agencies see coordination (Rope) — they are solving the legitimate problem of maintaining flood response capacity. The preparedness policy coalition sees a temporary problem with a sunset (Scaffold) — infrastructure maturity and automated monitoring are building alternative pathways that make recurring drills obsolete. The inspection bureaucracy sees its own degraded ritual (Piton) — the regime persists through institutional inertia and performative compliance, not because drills meaningfully test response capacity. Municipal budget officers see mixed coordination and extraction (Tangled Rope) — the system both enables regional coordination and extracts budget resources. Drill-fatigued residents see pure extraction (Snare) — mandatory participation with diminishing coordination benefit as catastrophe recedes into historical memory. The civilizational analytical observer risks seeing an immutable natural law (Mountain) — some form of preparedness is inherent to coastal settlement — but the structural data reveals this as a false summit: the specific institutional form (recurring drills, inspection bureaucracy, emergency management agencies) is contingent and benefits identifiable agents, not an immutable requirement of flood-vulnerable geography.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value is determined by the agent's structural position relative to the preparedness regime. Emergency management agencies are primary beneficiaries with arbitrage exit options — they experience low or negative effective extraction because the regime subsidizes their institutional continuity. Municipal budget officers are mixed — they benefit from coordination (regional standards) but bear extraction (budget mandates), producing moderate directionality. Drill-fatigued residents are victims with trapped exit options — they bear maximum extraction because they cannot exit the drill participation mandate and experience diminishing coordination benefit as catastrophe recedes into memory. The preparedness policy coalition has mobile exit options and sees a sunset — they experience low extraction because they have agency to build alternative pathways. The inspection bureaucracy has arbitrage exit options and sees its own degraded function — the piton classification derives from the theater gate rather than from high experienced extraction. The analytical observer's mountain classification is perspectival — the false summit detector identifies it as naturalization of a contingent institutional arrangement that benefits identifiable agents.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by showing that all six types are legitimate perspectival readings of the same structural data. The mandatrophy is not 'which type is correct?' but 'which perspective are you measuring from?' The analytical observer's mountain is a false summit (naturalized contingent institutional arrangement). The beneficiary's rope is their genuine experience (coordination enables resource sharing). The scaffold is a real structural feature (infrastructure maturity creates a sunset for drill-and-inspect regimes). The piton is a real observation (performative compliance as tacit knowledge atrophies). The snare is the powerless agent's structural reality (trapped in drill participation with diminishing benefit). The tangled rope is the moderate agent's mixed experience (coordination and extraction coexist). No single type is 'the' answer — the presheaf over the observation site IS the answer. The constraint's sunset clause is real but contested: the preparedness policy coalition sees infrastructure maturity creating an exit path, while emergency management agencies have institutional incentives to maintain the regime regardless of infrastructure improvements. The omega variable 'infrastructure_maturity_sunset' captures this irreducible uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophe_recurrence_threshold,
    'What recurrence interval of actual flood events is required to maintain preparedness as genuine coordination rather than theatrical extraction?',
    'Comparative analysis of preparedness regimes in regions with different flood recurrence rates; correlation between drill frequency and actual response effectiveness during events',
    'If threshold < 25 years: current Dutch regime is justified coordination. If threshold > 75 years: preparedness has become extractive theater maintained by institutional inertia rather than genuine risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_recurrence_threshold, empirical, 'Flood recurrence threshold for distinguishing coordination from theater').

omega_variable(
    tacit_knowledge_transmission,
    'Do recurring drills actually transmit the tacit knowledge required for effective flood response across generational turnover, or do they primarily maintain institutional legitimacy?',
    'Ethnographic study of drill participants; comparison of response effectiveness in jurisdictions with high vs low drill frequency; analysis of knowledge retention after personnel turnover',
    'If drills transmit knowledge: scaffold perspective confirmed — preparedness is functional coordination with a real sunset (infrastructure maturity). If drills are primarily legitimacy theater: piton perspective confirmed — the regime persists through performance, not function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tacit_knowledge_transmission, empirical, 'Whether drills transmit tacit knowledge or maintain legitimacy theater').

omega_variable(
    infrastructure_maturity_sunset,
    'Is the preparedness regime genuinely transitional (scaffold with real sunset as infrastructure matures), or is it a permanent extraction mechanism that will persist regardless of infrastructure improvements?',
    'Longitudinal tracking of preparedness budgets and drill frequency in regions with improving flood infrastructure; political economy analysis of emergency management agency incentives',
    'If transitional: scaffold classification holds — the regime has a real sunset as automated monitoring and resilient infrastructure reduce drill necessity. If permanent: reclassify as tangled_rope or snare — the coordination story is cover for institutional rent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_maturity_sunset, preference, 'Whether preparedness regime has a real sunset or is permanent extraction').

omega_variable(
    false_summit_naturalization,
    'Is the preparedness regime a natural law of coastal settlement (mountain), or a contingent institutional arrangement that naturalizes specific Dutch post-1953 policy choices?',
    'Cross-national comparison of flood preparedness regimes; identification of alternative institutional forms (insurance-based risk transfer, land-use restrictions, managed retreat) that achieve similar risk reduction without recurring drill mandates',
    'If natural law: mountain classification holds — some form of preparedness is inherent to flood-vulnerable geography. If contingent: false summit — the specific institutional form benefits identifiable agents (emergency management agencies, infrastructure maintenance sector) and is not an immutable requirement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether preparedness regime is natural law or contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence_flat_control, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_persist_theater_1953, preparedness_persistence_flat_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(prep_persist_theater_1963, preparedness_persistence_flat_control, theater_ratio, 10, 0.42).
narrative_ontology:measurement(prep_persist_theater_1973, preparedness_persistence_flat_control, theater_ratio, 20, 0.48).
narrative_ontology:measurement(prep_persist_theater_1983, preparedness_persistence_flat_control, theater_ratio, 30, 0.52).
narrative_ontology:measurement(prep_persist_theater_1993, preparedness_persistence_flat_control, theater_ratio, 40, 0.55).
narrative_ontology:measurement(prep_persist_theater_2003, preparedness_persistence_flat_control, theater_ratio, 50, 0.57).
narrative_ontology:measurement(prep_persist_theater_2013, preparedness_persistence_flat_control, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(prep_persist_extract_1953, preparedness_persistence_flat_control, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(prep_persist_extract_1963, preparedness_persistence_flat_control, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(prep_persist_extract_1973, preparedness_persistence_flat_control, base_extractiveness, 20, 0.21).
narrative_ontology:measurement(prep_persist_extract_1983, preparedness_persistence_flat_control, base_extractiveness, 30, 0.23).
narrative_ontology:measurement(prep_persist_extract_1993, preparedness_persistence_flat_control, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(prep_persist_extract_2003, preparedness_persistence_flat_control, base_extractiveness, 50, 0.27).
narrative_ontology:measurement(prep_persist_extract_2013, preparedness_persistence_flat_control, base_extractiveness, 60, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(prep_persist_suppress_1953, preparedness_persistence_flat_control, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(prep_persist_suppress_1983, preparedness_persistence_flat_control, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(prep_persist_suppress_2013, preparedness_persistence_flat_control, suppression_requirement, 60, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence_flat_control, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This is a flat construction control — no decomposition into readings. Contestation over whether preparedness is natural law, functional coordination, or extractive theater is captured through perspectival disagreement and omega variables, not through reading decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
