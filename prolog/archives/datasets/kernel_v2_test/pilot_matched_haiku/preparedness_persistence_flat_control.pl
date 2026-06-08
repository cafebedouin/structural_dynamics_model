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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   Post-1953 flood preparedness in developed nations institutionalized a
 *   specific approach to managing recurring flood risk: standardized drills,
 *   regular inspections, inter-agency coordination protocols, and
 *   bureaucratic oversight. The constraint embodies a shared commitment that
 *   this institutional practice maintains readiness across generations
 *   without catastrophe. However, the constraint exhibits structural tension
 *   between its stated coordination function (maintaining readiness) and its
 *   actual operation (sustaining a bureaucratic apparatus that may or may not
 *   improve disaster outcomes). The theater_ratio trajectory (0.35 → 0.62 →
 *   0.58) shows that performative compliance has increased over the interval,
 *   suggesting that the constraint's primary function has partially atrophied
 *   into institutional ritual. Simultaneously, extractiveness has risen
 *   modestly (0.25 → 0.38 → 0.35), indicating that the constraint
 *   increasingly benefits institutional actors (water management bureaucracy,
 *   engineering profession, insurance industry) while vulnerable populations
 *   bear the actual catastrophic risk. The constraint is a tangled rope:
 *   genuine coordination function (drills do improve inter-agency
 *   communication and response capacity) mixed with asymmetric extraction
 *   (federal mandates constrain local autonomy, shift liability upward, and
 *   sustain institutional budgets regardless of actual preparedness
 *   outcomes).
 *
 * KEY AGENTS:
 *   - Flood-Vulnerable Populations: Primary victim (powerless/trapped) — bear catastrophic risk while participating in preparedness rituals that may not protect them
 *   - Local Emergency Management Officials: Secondary victim (moderate/constrained) — constrained by federal mandates and funding tied to compliance; benefit from institutional legitimacy but bear responsibility for outcomes
 *   - Water Management Bureaucracy: Primary beneficiary (institutional/arbitrage) — sustains budget, career pathways, and institutional legitimacy through preparedness framework
 *   - Engineering Profession: Secondary beneficiary (institutional/arbitrage) — benefits from sustained demand for infrastructure design and inspection expertise
 *   - Insurance Industry: Tertiary beneficiary (institutional/arbitrage) — uses preparedness compliance as basis for risk assessment and premium calculation
 *   - Institutional Ritual System: Institutional actor (institutional/arbitrage) — the preparedness apparatus itself, maintained through inertia and theater
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable feature of disaster management
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence_flat_control, 0.35).
domain_priors:suppression_score(preparedness_persistence_flat_control, 0.42).
domain_priors:theater_ratio(preparedness_persistence_flat_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence_flat_control, extractiveness, 0.35).
narrative_ontology:constraint_metric(preparedness_persistence_flat_control, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(preparedness_persistence_flat_control, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence_flat_control, tangled_rope).
narrative_ontology:human_readable(preparedness_persistence_flat_control, "Post-1953 Flood Preparedness as Institutionalized Practice").
narrative_ontology:topic_domain(preparedness_persistence_flat_control, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_persistence_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(preparedness_persistence_flat_control, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence_flat_control, water_management_bureaucracy).
narrative_ontology:constraint_beneficiary(preparedness_persistence_flat_control, engineering_profession).
narrative_ontology:constraint_beneficiary(preparedness_persistence_flat_control, insurance_industry).
narrative_ontology:constraint_victim(preparedness_persistence_flat_control, flood_vulnerable_populations).
narrative_ontology:constraint_victim(preparedness_persistence_flat_control, local_emergency_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_persistence_flat_control, local_emergency_management).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents in flood-prone areas participate in mandatory drills and inspections, comply with preparedness regulations, and bear the catastrophic risk of flood events. They have no exit option — geographic and economic circumstances trap them in flood-prone areas. They experience the preparedness system as a requirement imposed on them without meaningful protection in return.
narrative_ontology:constraint_stakeholder(preparedness_persistence_flat_control, flood_vulnerable_populations, payer,
    powerless, biographical, trapped, regional).

% Local emergency officials administer preparedness protocols, conduct drills and inspections, and coordinate inter-agency response. They benefit from federal funding tied to preparedness compliance and from professional legitimacy. They also bear responsibility for actual disaster outcomes and face constraints from federal mandates that limit local autonomy. They are both agenda-setters (implementing preparedness) and payers (bearing responsibility for failures).
narrative_ontology:constraint_stakeholder(preparedness_persistence_flat_control, local_emergency_management, agenda_setter,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence_flat_control, local_emergency_management, payer).

% The water management bureaucracy sustains its budget, career pathways, and institutional legitimacy through the preparedness framework. It coordinates inter-agency communication, allocates resources, and sets standards for preparedness compliance. It benefits from the constraint without bearing significant costs — it can reallocate resources, shift focus, or redefine success metrics as needed.
narrative_ontology:constraint_stakeholder(preparedness_persistence_flat_control, water_management_bureaucracy, beneficiary,
    institutional, immediate, arbitrage, national).

% Engineers benefit from sustained demand for infrastructure design, inspection protocols, and technical expertise related to flood preparedness. The constraint creates professional legitimacy and career pathways. Engineers have high exit optionality — they can shift focus to other infrastructure domains if preparedness demand declines.
narrative_ontology:constraint_stakeholder(preparedness_persistence_flat_control, engineering_profession, beneficiary,
    institutional, biographical, arbitrage, national).

% Insurance companies use preparedness compliance as a basis for risk assessment and premium calculation. They benefit from the constraint through reduced uncertainty about disaster outcomes and through the ability to differentiate premiums based on preparedness status. They have high exit optionality — they can adjust pricing models or exit markets as needed.
narrative_ontology:constraint_stakeholder(preparedness_persistence_flat_control, insurance_industry, beneficiary,
    institutional, immediate, arbitrage, national).

% The preparedness apparatus itself — the drills, inspections, protocols, and bureaucratic structures — persists through institutional inertia and theater. It maintains itself by creating the appearance of preparedness and by justifying its own existence through compliance metrics. It has high exit optionality in the sense that it can evolve or be replaced, but it resists change because the institutional form has become self-justifying.
narrative_ontology:constraint_stakeholder(preparedness_persistence_flat_control, institutional_ritual_system, agenda_setter,
    institutional, civilizational, arbitrage, national).

% The actual effectiveness of disaster response — whether drills and inspections actually improve outcomes when floods occur — is excluded from the preparedness system's decision-making. Response effectiveness is not measured systematically, is not tied to funding or compliance, and is not represented in the institutional apparatus. It is the absent voice in preparedness policy.
narrative_ontology:constraint_stakeholder(preparedness_persistence_flat_control, disaster_response_effectiveness, excluded,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(preparedness_persistence_flat_control, disaster_response_effectiveness).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintaining inter-agency communication, resource coordination, and institutional readiness across generations so that when flood events occur, response capacity is available and coordinated rather than improvised.
% TRANSFER_FUNCTION: The constraint transfers compliance burden, psychological reassurance, and institutional legitimacy from vulnerable populations to the water management bureaucracy, engineering profession, and insurance industry. It moves resources (federal preparedness funding) from general budgets to preparedness-specific activities. It moves career opportunities and professional legitimacy to engineers and emergency managers.
% ABSENT_VOICES: Disaster response effectiveness is excluded from the preparedness system's decision-making. Communities that have experienced preparedness failures but lack institutional voice are excluded. Alternative approaches to flood risk management (relocation assistance, insurance-based risk transfer, decentralized community preparedness) are excluded from the conversation because the post-1953 framework is the default institutional choice.
% DISAPPEARANCE_RATIONALE: If the post-1953 preparedness system disappeared overnight, the world would rearrange significantly. Water management bureaucracies would lose institutional legitimacy and budget justification. Engineering firms would lose demand for preparedness-related work. Insurance companies would lose a basis for risk differentiation. Local emergency management would lose federal funding and coordination frameworks. However, flood risk would not disappear — it would be managed through alternative mechanisms (insurance-based risk transfer, relocation assistance, decentralized community preparedness, or ad-hoc response). The rearrangement would be substantial but not catastrophic.
% FOUNDING_PROBLEM: The 1953 flood catastrophe revealed that uncoordinated, unprepared response to major disasters resulted in massive casualties and economic loss. The founding problem was: how can societies maintain readiness across generations so that when catastrophic floods occur, response capacity is available and coordinated?
% FOUNDING_PROBLEM_CORROBORATION: Water management bureaucracies and engineering professionals attest that the founding problem (unpreparedness) remains live and that the post-1953 framework addresses it. Flood-vulnerable populations and some disaster researchers contest whether the founding problem is still live or whether it has been superseded by other problems (climate change, infrastructure aging, institutional capture). The disagreement is located in whether the 1953 framework is still optimal or whether it has become obsolete.
narrative_ontology:disappearance_verdict(preparedness_persistence_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence_flat_control, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FLOOD-VULNERABLE RESIDENT (SNARE) — Trapped by geography and economic circumstance in flood-prone areas. Participates in mandatory drills and inspections that create the appearance of protection while bearing the actual catastrophic risk. No exit option; bears full cost of preparedness failure. Extraction is maximal: the constraint extracts compliance and psychological reassurance while delivering uncertain protection.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOCAL EMERGENCY MANAGEMENT OFFICIAL (TANGLED ROPE) — Constrained by state/federal mandates and funding tied to compliance with preparedness protocols. Benefits from the institutional framework (career stability, resource allocation, professional legitimacy) while also bearing responsibility for actual outcomes. Genuine coordination function (drills do improve response capacity) mixed with asymmetric extraction (federal mandates constrain local autonomy and shift liability upward).
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: WATER MANAGEMENT BUREAUCRACY (ROPE) — Institutional beneficiary with high exit optionality (can shift focus, reallocate resources, redefine success metrics). Experiences the constraint as pure coordination: standardized drills and inspections enable inter-agency communication, resource pooling, and liability distribution. Net beneficiary — the constraint sustains the bureaucratic apparatus and justifies its budget.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ENGINEERING PROFESSION (ROPE) — Benefits from the constraint through sustained demand for infrastructure design, inspection protocols, and technical expertise. Experiences the constraint as coordination: standardized preparedness frameworks create professional legitimacy and career pathways. Low extraction — the profession's interests align with the constraint's persistence.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL RITUAL SYSTEM (PITON) — The constraint's primary function (maintaining readiness across generations) has atrophied into performative compliance. Drills and inspections persist through institutional inertia and theater: they create the appearance of preparedness without necessarily improving actual disaster response. Theater ratio (0.58) reflects that much of the activity is maintenance of the institutional form rather than functional readiness. The system is maintained because alternatives haven't fully replaced it and because the ritual itself has become institutionally self-justifying.
constraint_indexing:constraint_classification(preparedness_persistence_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some preparedness lag is inherent to human systems: complex infrastructure always carries residual risk, and the gap between preparation and catastrophe is an immutable feature of how societies manage uncertainty. This perspective risks naturalizing what is actually a contingent institutional arrangement — the specific form of post-1953 preparedness (drills, inspections, bureaucratic oversight) is not a law of nature but a particular institutional choice that benefits specific actors.
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
 *   Extractiveness (0.35): Moderate. The constraint extracts compliance, psychological reassurance, and institutional legitimacy from vulnerable populations while delivering uncertain protection. The extraction is not maximal because some genuine coordination function exists (drills do improve response capacity) and because the constraint is not purely coercive (vulnerable populations have some agency in preparedness participation). The modest rise over the interval (0.25 → 0.38 → 0.35) reflects increasing bureaucratic overhead and institutional benefit-capture, partially offset by genuine improvements in inter-agency coordination. Suppression (0.42): Moderate. Vulnerable populations face significant barriers to exit (geographic, economic, legal) but are not completely trapped. Suppression is maintained through regulatory mandate (mandatory participation in drills), funding conditionality (federal preparedness funding tied to compliance), and institutional inertia (the preparedness framework is the default approach). Theater ratio (0.58): Moderate-high. The constraint's performative content has increased over the interval as the original functional purpose (maintaining readiness after the 1953 flood) has receded and the institutional apparatus has become self-justifying. Drills and inspections create the appearance of preparedness without necessarily improving actual disaster response. The peak at 2003 (0.62) reflects post-9/11 expansion of security theater into disaster preparedness; the slight decline by 2023 (0.58) reflects some shift toward data-driven preparedness and real-time monitoring, though the institutional ritual persists.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival disagreement across observer positions. The water management bureaucracy sees pure coordination (Rope) — the constraint enables inter-agency communication and resource pooling. The engineering profession sees coordination with professional benefit (Rope) — the constraint sustains demand for expertise. The local emergency official sees mixed coordination and extraction (Tangled Rope) — genuine readiness improvement mixed with federal mandate constraints. The flood-vulnerable resident sees pure extraction (Snare) — participation in drills without meaningful protection. The institutional ritual system sees degraded function maintained through theater (Piton) — the apparatus persists through inertia, not efficacy. The analytical observer risks seeing natural law (Mountain) — flood risk management requires preparedness, so the institutional form appears inevitable. The perspectival gap reveals that the constraint's classification depends entirely on the observer's structural position: beneficiaries see coordination, victims see extraction, institutional actors see ritual maintenance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural relationship to the constraint. Water management bureaucracy: beneficiary status + arbitrage exit options → d ≈ 0.1 (full beneficiary). Engineering profession: beneficiary status + arbitrage exit options → d ≈ 0.15 (strong beneficiary). Local emergency official: mixed beneficiary/victim status + constrained exit → d ≈ 0.45 (moderate extraction). Flood-vulnerable resident: victim status + trapped exit → d ≈ 0.85 (maximum extraction). The engine derives effective extraction (χ) from these d values, power level, and spatial scope. Trapped agents at regional scope experience maximum χ; institutional beneficiaries with arbitrage options experience negative χ (subsidy). The perspectival gap in d values (0.1 to 0.85) explains why the same constraint classifies as rope for beneficiaries and snare for victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The post-1953 preparedness mandate (recurring drills and inspections as the primary mechanism for maintaining readiness) may have outlived its original function. The founding problem (catastrophic unpreparedness after the 1953 flood) has been addressed through infrastructure investment and institutional development. However, the preparedness apparatus persists and has expanded, suggesting that the mandate has become decoupled from its original purpose. The theater_ratio trajectory (rising from 0.35 to 0.62 and stabilizing at 0.58) indicates that the constraint increasingly consists of performative compliance rather than functional readiness improvement. The extractiveness trajectory (rising from 0.25 to 0.38) indicates that the constraint increasingly benefits institutional actors rather than vulnerable populations. These patterns suggest mandatrophy: the constraint's original mandate (maintaining readiness) has been superseded by a new de facto mandate (sustaining the preparedness bureaucracy). However, mandatrophy is not fully resolved because the constraint retains some genuine coordination function (drills do improve inter-agency communication) and some genuine protective value (preparedness does reduce some disaster impacts). The constraint is in a state of partial mandatrophy: the original mandate is obsolete, but the institutional apparatus persists because it provides secondary benefits to powerful actors and because alternatives have not fully replaced it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preparedness_function_atrophy,
    'Has the post-1953 preparedness system''s primary function (maintaining readiness across generations) genuinely atrophied into theater, or does it continue to provide measurable protection?',
    'Longitudinal comparison of flood response outcomes in jurisdictions with high vs low compliance with preparedness protocols; analysis of whether drills correlate with improved response times and reduced casualties; examination of whether preparedness failures are preceded by protocol violations or by protocol inadequacy.',
    'If atrophied: piton classification confirmed, theater_ratio justified at 0.58+. If functional: tangled_rope classification strengthened, theater_ratio should be lower (0.35-0.45). If mixed: constraint is genuinely hybrid with regional variation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preparedness_function_atrophy, empirical, 'Whether preparedness system function has atrophied into theater').

omega_variable(
    extraction_beneficiary_identification,
    'Who specifically benefits from the persistence of the post-1953 preparedness system, and what would they lose if it were replaced by alternative approaches (e.g., relocation assistance, insurance-based risk transfer, decentralized community preparedness)?',
    'Institutional analysis of budget flows, career pathways, and professional legitimacy tied to preparedness bureaucracy; comparison of resource allocation under preparedness vs alternative frameworks; examination of resistance to preparedness reform proposals.',
    'If beneficiaries are primarily institutional (water management, engineering, insurance): tangled_rope classification confirmed, extractiveness justified. If beneficiaries are primarily vulnerable populations (through genuine protection): rope classification strengthened, extractiveness should be lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_beneficiary_identification, empirical, 'Identification of who benefits from preparedness system persistence').

omega_variable(
    mandate_obsolescence,
    'Is the post-1953 preparedness mandate (recurring drills and inspections as the primary mechanism for maintaining readiness) still the optimal approach, or has it been superseded by better methods (real-time monitoring, predictive modeling, insurance mechanisms, relocation)?',
    'Comparative analysis of preparedness effectiveness across different institutional frameworks; examination of whether newer jurisdictions or nations use different approaches; assessment of whether the 1953 mandate persists because it works or because institutional inertia prevents change.',
    'If mandate is obsolete: mandatrophy_resolved should be true, constraint should be reclassified as piton or snare. If mandate is still optimal: classification stands. If mandate is contested: omega documents the disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_obsolescence, empirical, 'Whether post-1953 preparedness mandate is still optimal').

omega_variable(
    generational_knowledge_transfer,
    'Do recurring drills and inspections actually maintain institutional memory and readiness across generations, or do they create the appearance of continuity while tacit knowledge erodes?',
    'Longitudinal study of disaster response effectiveness across generational cohorts; analysis of whether response quality degrades after personnel turnover; examination of whether drills capture tacit knowledge or only explicit procedures.',
    'If knowledge transfer is effective: rope classification strengthened, theater_ratio should be lower. If knowledge erodes: snare classification strengthened for vulnerable populations, theater_ratio justified at 0.58+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_knowledge_transfer, empirical, 'Whether drills maintain institutional memory across generations').

omega_variable(
    false_summit_natural_law,
    'Is the post-1953 preparedness system a natural law (immutable feature of how societies manage flood risk) or a constructed institutional arrangement that benefits specific actors and could be replaced?',
    'Comparative institutional analysis: examination of how different societies approach flood preparedness; identification of whether the 1953 framework is universal or culturally/institutionally contingent; analysis of whether beneficiaries actively defend the framework against alternatives.',
    'If natural law: mountain classification confirmed. If constructed: false summit detected, reclassification to tangled_rope or snare depending on extraction severity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether preparedness system is natural law or constructed arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence_flat_control, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_theater_1953, preparedness_persistence_flat_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(prep_theater_1968, preparedness_persistence_flat_control, theater_ratio, 15, 0.48).
narrative_ontology:measurement(prep_theater_1988, preparedness_persistence_flat_control, theater_ratio, 35, 0.58).
narrative_ontology:measurement(prep_theater_2003, preparedness_persistence_flat_control, theater_ratio, 50, 0.62).
narrative_ontology:measurement(prep_theater_2023, preparedness_persistence_flat_control, theater_ratio, 70, 0.58).

% Extraction over time
narrative_ontology:measurement(prep_extract_1953, preparedness_persistence_flat_control, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(prep_extract_1968, preparedness_persistence_flat_control, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(prep_extract_1988, preparedness_persistence_flat_control, base_extractiveness, 35, 0.35).
narrative_ontology:measurement(prep_extract_2003, preparedness_persistence_flat_control, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(prep_extract_2023, preparedness_persistence_flat_control, base_extractiveness, 70, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(prep_suppress_1953, preparedness_persistence_flat_control, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(prep_suppress_1988, preparedness_persistence_flat_control, suppression_requirement, 35, 0.42).
narrative_ontology:measurement(prep_suppress_2023, preparedness_persistence_flat_control, suppression_requirement, 70, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence_flat_control, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_persistence_flat_control, flood_insurance_risk_assessment).
narrative_ontology:affects_constraint(preparedness_persistence_flat_control, infrastructure_maintenance_mandate).
narrative_ontology:affects_constraint(preparedness_persistence_flat_control, emergency_response_capacity).

% DUAL FORMULATION NOTE:
% Post-1953 preparedness is downstream of the founding catastrophe (1953 flood) and upstream of specific institutional outcomes (insurance pricing, infrastructure investment, emergency response effectiveness). The constraint represents a particular institutional choice about how to manage flood risk; alternative approaches (relocation assistance, insurance-based risk transfer, decentralized community preparedness) would produce different constraint stories with different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_persistence_flat_control, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
