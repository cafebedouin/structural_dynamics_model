% ============================================================================
% CONSTRAINT STORY: husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_husk_reading, []).

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
    narrative_ontology:affects_constraint/2,
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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: husk_reading
 *   human_readable: Preparedness Sustained Through Memorial Ritual (Husk Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   The husk reading models preparedness institutions that sustain disaster
 *   memory through memorial ritual while operational response capacity
 *   atrophies. This reading emerged from observation of long-established
 *   disaster memorial systems where annual commemoration drills, symbolic
 *   inspections, and ritual observances consume institutional resources while
 *   actual flood response infrastructure degrades. The constraint's
 *   theater_ratio (0.81) reflects that most preparedness activity is
 *   performative: drills are staged for memorial purposes rather than skill
 *   building; inspections verify checklist completion rather than operational
 *   readiness; participation is measured by attendance at ceremonies rather
 *   than by capacity metrics. The trajectory shows steady displacement of
 *   function by performance over a 50-year interval, modeling the
 *   institutional lifecycle from founding disaster (t=0, genuine operational
 *   focus) to mature memorial system (t=50, primarily theatrical). The husk
 *   reading holds that this is a stable structural pattern, not a correctable
 *   institutional failure — the identity-maintenance function captured the
 *   preparedness mandate and converted it into ritual apparatus.
 *
 * KEY AGENTS:
 *   - Coastal Communities: Primary victim (powerless/trapped) — trapped in flood zones with degraded actual response capacity; required to participate in memorial ritual while operational infrastructure decays; cannot exit
 *   - Emergency Response Personnel: Secondary victim (moderate/constrained) — professionals who see their own work as degraded; know the ritual is hollow but cannot exit without abandoning profession; experience preparedness as theater
 *   - National Identity Maintenance Apparatus: Primary beneficiary (institutional/arbitrage) — memorial ritual serves genuine identity-maintenance function; benefits from sustained national resilience narrative; has exit options to other identity mechanisms
 *   - Disaster Preparedness Bureaucracy: Hybrid actor (institutional/constrained) — coordinates memorial ritual (genuine function) while extracting budget and authority as operational capacity degrades (asymmetric extraction); constrained by institutional position
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — diagnoses piton structure from theater_ratio and victim identification; sees the constraint as institutional inertia maintaining performance after function atrophied
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(husk_reading, 0.58).
domain_priors:suppression_score(husk_reading, 0.62).
domain_priors:theater_ratio(husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(husk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(husk_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(husk_reading, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(husk_reading, piton).
narrative_ontology:human_readable(husk_reading, "Preparedness Sustained Through Memorial Ritual (Husk Reading)").
narrative_ontology:topic_domain(husk_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(husk_reading, 'c2ac472a-a225-499b-91af-8a81a3c0d9af').
narrative_ontology:cs_kernel_codification('c2ac472a-a225-499b-91af-8a81a3c0d9af', formalized).
narrative_ontology:cs_authority_grounding('c2ac472a-a225-499b-91af-8a81a3c0d9af', lineage).
narrative_ontology:cs_interpretation_layer_present('c2ac472a-a225-499b-91af-8a81a3c0d9af').
narrative_ontology:cs_reading_relation('c2ac472a-a225-499b-91af-8a81a3c0d9af', husk_reading__competence_reading, coexists_with).
narrative_ontology:cs_axiom('c2ac472a-a225-499b-91af-8a81a3c0d9af', foundational, memorial_sufficiency_for_preparedness).
narrative_ontology:cs_axiom_status(memorial_sufficiency_for_preparedness, holdable).
narrative_ontology:cs_axiom_grounding('c2ac472a-a225-499b-91af-8a81a3c0d9af', memorial_sufficiency_for_preparedness, empirically_contingent).
narrative_ontology:cs_axiom('c2ac472a-a225-499b-91af-8a81a3c0d9af', secondary, operational_capacity_through_ritual_transmission).
narrative_ontology:cs_axiom_status(operational_capacity_through_ritual_transmission, overridden).
narrative_ontology:cs_axiom_grounding('c2ac472a-a225-499b-91af-8a81a3c0d9af', operational_capacity_through_ritual_transmission, empirically_contingent).
narrative_ontology:cs_reference_frame('c2ac472a-a225-499b-91af-8a81a3c0d9af', founding_disaster_operational_mandate).
narrative_ontology:cs_drift_state('c2ac472a-a225-499b-91af-8a81a3c0d9af', contemporary_memorial_system, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c2ac472a-a225-499b-91af-8a81a3c0d9af', '2025-01-09T19:45:00Z').
narrative_ontology:cs_kernel_id(husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(husk_reading, national_identity_maintenance_apparatus).
narrative_ontology:constraint_beneficiary(husk_reading, disaster_preparedness_bureaucracy).
narrative_ontology:constraint_beneficiary(husk_reading, memorial_ritual_organizers).
narrative_ontology:constraint_victim(husk_reading, actual_flood_response_capacity).
narrative_ontology:constraint_victim(husk_reading, coastal_communities).
narrative_ontology:constraint_victim(husk_reading, emergency_response_personnel).
narrative_ontology:constraint_vindicates(husk_reading, memorial_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in flood-prone zones where levee maintenance and emergency response infrastructure have degraded while budgets are redirected to memorial ceremony coordination. Required to participate in annual commemoration drills that prioritize symbolic observance over operational skill building. Cannot relocate due to economic constraints; cannot opt out of ritual participation without social penalty. Experience the gap between ceremonial preparedness (high) and actual response capacity (low) most acutely when floods occur.
narrative_ontology:constraint_stakeholder(husk_reading, coastal_communities, payer,
    powerless, immediate, trapped, local).

% Professionals trained for operational flood response who now spend most of their time coordinating memorial events and completing compliance checklists. See their own work as degraded: training exercises prioritize ceremonial participation over skill maintenance; equipment inspections verify bureaucratic compliance rather than operational readiness. Know the ritual is hollow but cannot publicly challenge the gap without career damage. Could exit the profession but only at high personal cost (lost specialized training, pension, professional identity).
narrative_ontology:constraint_stakeholder(husk_reading, emergency_response_personnel, payer,
    moderate, biographical, constrained, regional).

% Institutions responsible for sustaining national narratives and collective memory (museums, education ministries, cultural agencies, national media). The memorial ritual serves their genuine function: transmitting the founding disaster story and national resilience narrative across generations. Benefits from sustained public engagement with the commemoration cycle. Has arbitrage exit: if memorial ritual loses effectiveness, could shift resources to other national identity mechanisms (monuments, curriculum, media campaigns).
narrative_ontology:constraint_stakeholder(husk_reading, national_identity_maintenance_apparatus, beneficiary,
    institutional, generational, arbitrage, national).

% Agencies with statutory preparedness mandate (emergency management, infrastructure ministries, civil defense). Coordinate memorial ritual scheduling, participation tracking, and compliance reporting (genuine coordination function). Extract budget allocation and institutional authority from preparedness mandate while operational capacity metrics decline (asymmetric extraction). Constrained by institutional position: cannot abandon preparedness mission without losing organizational legitimacy; cannot acknowledge ritual-reality gap without undermining authority to coordinate the ritual.
narrative_ontology:constraint_stakeholder(husk_reading, disaster_preparedness_bureaucracy, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(husk_reading, disaster_preparedness_bureaucracy, beneficiary).

% Event coordinators, ceremony planners, and commemoration specialists employed to design and execute annual memorial observances. Set the agenda for how preparedness is performed: what drills look like, what symbols are used, what narratives are emphasized. Mobile exit: specialized in event coordination generally, not locked to disaster memorial niche. Benefit from sustained demand for their services but do not extract rents from degraded response capacity (they coordinate the ritual, not the preparedness system).
narrative_ontology:constraint_stakeholder(husk_reading, memorial_ritual_organizers, agenda_setter,
    moderate, biographical, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The memorial ritual coordinates collective memory transmission: ensuring that each generation knows the founding disaster story, recognizes flood risk, and maintains identification with national resilience narrative. Coordinating who participates in annual commemoration, what the ritual consists of, and how disaster memory is officially preserved.
% TRANSFER_FUNCTION: Budget and institutional authority flow from operational preparedness (levee maintenance, emergency response training, infrastructure resilience) to memorial observance (ceremony coordination, symbolic drill staging, compliance documentation). Participation labor flows from coastal communities and emergency personnel to the ritual apparatus. National identity capital (resilience narrative, collective memory) flows from the ritual to the identity maintenance apparatus.
% ABSENT_VOICES: Future flood victims: those who will bear the costs of degraded actual response capacity but are not yet in the conversation because the flood has not yet occurred. Reform advocates who would prioritize operational capacity over ceremonial observance: marginalized because challenging the ritual is framed as disrespecting disaster victims or abandoning national identity. Independent capacity auditors: excluded from preparedness assessment because the bureaucracy controls the metrics, which measure ritual observance rather than response capability.
% DISAPPEARANCE_RATIONALE: If the memorial ritual disappeared overnight, multiple arrangements would rearrange: the preparedness bureaucracy would lose its primary activity and institutional legitimacy; the identity maintenance apparatus would lose a key national narrative transmission mechanism; coastal communities would face the revealed gap between ceremonial preparedness and actual capacity (currently masked by the ritual); emergency personnel would face the choice between rebuilding operational capacity or finding new employment. The ritual is not a natural fact — it is an institutional arrangement that multiple stakeholder groups depend on, even if for different reasons (identity maintenance, bureaucratic authority, career continuity) than the original preparedness mandate.
% FOUNDING_PROBLEM: The founding problem was operational flood disaster response failure. A catastrophic flood event revealed that existing preparedness was inadequate, resulting in significant loss of life and infrastructure damage. The memorial system was created to ensure that future generations would maintain vigilance and operational readiness by ritually remembering the disaster's costs.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (ensuring operational readiness through sustained disaster memory) is dead in the sense that operational capacity has decayed even as memorial ritual intensified — the ritual failed to sustain the capacity it was built to preserve. This status is corroborated by emergency response personnel who witness the capacity decay directly, by analytical observers (disaster policy researchers, capacity auditors) who measure response capability independently of ritual participation, and by the revealed gap during subsequent flood events where ceremonial preparedness did not translate to effective response. The identity maintenance apparatus would contest this status, arguing that the problem was always collective memory maintenance rather than operational capacity, but their testimony is from the beneficiary seat (they are describing the problem the ritual successfully solves for them, not the problem it was originally built to solve). The preparedness bureaucracy cannot corroborate honestly without undermining its own authority, so its official position is that the founding problem remains live and is being addressed through ritual observance — but this is self-serving claim, not external corroboration.
narrative_ontology:disappearance_verdict(husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(husk_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COASTAL COMMUNITIES (SNARE) — Trapped in flood zones with degraded actual response capacity. Experience maximum extraction: required to participate in annual memorial drills while levee maintenance budgets are diverted to ceremony coordination. Cannot exit the flood zone; cannot opt out of performative preparedness. The ritual structure extracts participation labor while operational capacity atrophies.
constraint_indexing:constraint_classification(husk_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: EMERGENCY RESPONSE PERSONNEL (PITON) — Constrained by institutional position. See their own preparedness work as degraded: inspection checklists are completed for compliance rather than verification; training exercises prioritize symbolic participation over skill building. The function they were trained to perform (operational flood response) has atrophied into performance of preparedness. They know the ritual is hollow but cannot exit the system without abandoning their profession.
constraint_indexing:constraint_classification(husk_reading, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NATIONAL IDENTITY MAINTENANCE APPARATUS (ROPE) — Benefits from the constraint's coordination function. The memorial ritual solves a genuine problem: maintaining collective memory of the founding disaster and national resilience narrative across generations. Experiences low extraction because the ritual serves their institutional purpose. Has arbitrage exit: could shift to other national identity mechanisms if memorial ritual loses effectiveness.
constraint_indexing:constraint_classification(husk_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DISASTER PREPAREDNESS BUREAUCRACY (TANGLED ROPE) — Both coordinates and extracts. Coordinates memorial ritual scheduling and participation tracking (genuine coordination function); extracts budget allocation and institutional authority from preparedness mandate while operational capacity degrades (asymmetric extraction). Constrained by institutional position: cannot abandon preparedness mission without losing organizational legitimacy, but also cannot acknowledge the ritual-reality gap without undermining their own authority.
constraint_indexing:constraint_classification(husk_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (PITON) — From the civilizational analytical perspective, this constraint is a textbook piton: a preparedness mechanism whose primary function (operational flood response capacity) has atrophied while the memorial ritual persists through institutional inertia and identity-maintenance value. The theater_ratio is diagnostic: 81% of preparedness activity is performative. The constraint remains because removing it would require acknowledging the ritual-reality gap, which would threaten national identity narratives built on disaster resilience.
constraint_indexing:constraint_classification(husk_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(husk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(husk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(husk_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(husk_reading, TR),
    TR >= 0.70.

:- end_tests(husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. The preparedness bureaucracy extracts budget allocation and institutional authority from the preparedness mandate while operational capacity degrades; coastal communities bear costs of degraded response capacity while providing participation labor for memorial ritual; identity maintenance apparatus benefits from sustained national narrative. The extraction is not maximal because some residual coordination function remains (memorial ritual does transmit collective memory, even if operational knowledge is lost). Suppression (0.62): Moderate-high. Coastal communities cannot exit flood zones and cannot opt out of ritual participation without social penalty; emergency personnel cannot challenge the ritual-reality gap without career damage; the identity-maintenance apparatus suppresses acknowledgment that operational capacity has degraded (admitting the gap would threaten national resilience narrative). Suppression is not total because some stakeholders (analytical observers, reform advocates) can name the problem, but their challenges are marginalized. Theater ratio (0.81): Very high. The vast majority of preparedness activity is performative: drills prioritize symbolic participation over skill transfer; inspections verify bureaucratic compliance rather than operational readiness; preparedness success is measured by ritual observance rather than response capacity metrics. The trajectory shows theater displacing function steadily over the 50-year interval, consistent with piton lifecycle dynamics.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in the husk reading reveals how different structural positions produce radically different classifications of the same memorial-preparedness system. Coastal communities see a snare: they are trapped with no exit, bearing maximum extraction as operational capacity degrades while ritual demands increase. Emergency response personnel see a piton: they recognize their own work as degraded performance, maintained through institutional inertia rather than function. The national identity maintenance apparatus sees a rope: the memorial ritual solves their genuine coordination problem (sustaining national narrative) with minimal extraction. The disaster preparedness bureaucracy sees a tangled rope: they coordinate the ritual (genuine function) while extracting rents as capacity atrophies (asymmetric extraction), with no clean exit from the contradiction. The analytical observer sees a piton from the civilizational perspective: an institution whose original function (operational preparedness) has atrophied while the ritual structure persists, diagnostic of Goodhart drift where the measure (memorial observance) displaced the goal (response capacity). The gap between the identity apparatus's rope and the coastal communities' snare is the core tension: what is genuine coordination for one agent is pure extraction for another, and both classifications are structurally accurate from their respective positions. The perspectival diversity is not noise — it is the signal that this constraint operates differently for different agents, which is exactly what indexical classification is designed to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The husk reading's beneficiary/victim structure reflects identity-maintenance capture of the preparedness mandate. The national identity maintenance apparatus is the primary beneficiary: the memorial ritual serves its genuine institutional purpose (sustaining national resilience narrative and collective disaster memory) with low experienced extraction because the ritual performs the function it was captured for. The disaster preparedness bureaucracy has hybrid directionality: it coordinates the ritual (pushing d toward beneficiary) but also extracts rents while operational capacity degrades (pushing d toward target), resulting in moderate effective extraction. Coastal communities are primary victims: they bear the costs of degraded actual response capacity while providing compulsory participation labor for the ritual, with no exit options, resulting in high effective extraction. Emergency response personnel are secondary victims: they experience the constraint as degraded function (their professional work has become performance) with constrained but non-zero exit options (can leave the profession but at high career cost), resulting in moderate-high effective extraction. The analytical observer has zero extraction (analytical exit, no stake in the ritual's outcomes). The directionality pattern is diagnostic of piton structure: beneficiaries experience the constraint as serving a genuine function (just not the original preparedness function), while victims experience it as hollow performance of the abandoned mandate.
 *
 * MANDATROPHY ANALYSIS:
 *   The husk reading resolves mandatrophy by showing that the preparedness mandate and the identity-maintenance mandate are structurally distinct, and the latter captured the former. The preparedness bureaucracy's mandate (operational flood response capacity) has not been fulfilled — it has been converted into a different mandate (sustaining national resilience narrative through memorial ritual). The analytical piton classification identifies this as institutional mandatrophy: the original function atrophied but the ritual persists because it now serves a different beneficiary (identity maintenance) than the one it was created for (coastal flood protection). The mandate-function gap is not a bug to be corrected; it is the stable structure of the constraint under the husk reading. The risk of mislabeling is mitigated by the perspectival structure: the identity apparatus genuinely experiences rope (their mandate IS being served), while coastal communities genuinely experience snare (the preparedness mandate that would serve them has been abandoned). Both are true. The mandatrophy is in the gap between them: one mandate was sacrificed to serve another, and the ritual apparatus is the mechanism of that sacrifice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_reading_displacement,
    'Is this reading (husk) a structural truth about preparedness institutions under memorial pressure, or is the competence reading (operational preparedness maintained through ritual as mnemonic device) the actual constraint and this reading represents institutional failure rather than stable structure?',
    'Comparative analysis across multiple disaster-memorial systems: Japan (tsunami), Netherlands (flood), California (earthquake). Measure theater_ratio and response capacity correlation longitudinally. If memorial ritual reliably preserves operational capacity across diverse contexts, competence reading is structurally true. If ritual reliably displaces capacity, husk reading is structurally true.',
    'If competence reading is correct: husk classification as piton is mis-diagnosis of temporary institutional failure. If husk reading is correct: piton classification is accurate structural description of memorial-preparedness dynamics under identity-maintenance pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_reading_displacement, empirical, 'Whether memorial ritual sustains or displaces operational capacity').

omega_variable(
    ritual_kernel_ambiguity,
    'What is the kernel being transmitted: operational flood preparedness knowledge or national disaster resilience narrative? The husk reading assumes the kernel is identity-narrative and preparedness is the failed container; the competence reading assumes the kernel is preparedness and narrative is the transmission mechanism.',
    'Content analysis of memorial ritual: ratio of operational instruction (evacuation routes, levee inspection protocols, emergency supply requirements) to identity narrative (national resilience, founding disaster heroism, collective memory). Interview participants about what they believe they are sustaining through the ritual.',
    'If kernel is identity-narrative: husk reading''s beneficiary structure (identity maintenance apparatus) is correct. If kernel is operational preparedness: competence reading''s beneficiary structure (coastal communities protected by sustained capacity) is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ritual_kernel_ambiguity, conceptual, 'What the ritual is actually transmitting as its kernel').

omega_variable(
    response_capacity_measurement,
    'How do we measure ''actual flood response capacity'' independently of the institutional measures that are themselves part of the ritual structure?',
    'Counterfactual test: measure response effectiveness in communities with high memorial ritual participation vs. communities with low participation but equivalent infrastructure investment. Alternatively: longitudinal tracking of response time, casualty rates, and infrastructure damage in actual flood events, correlated with memorial ritual intensity rather than with self-reported preparedness scores.',
    'If ritual-heavy communities respond more effectively: husk reading''s victim identification (actual response capacity) is incorrect — the ritual is sustaining capacity, not displacing it. If ritual-heavy communities respond less effectively: victim identification is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(response_capacity_measurement, empirical, 'Independent measure of operational capacity vs. ritual performance').

omega_variable(
    sunset_blocked_mechanism,
    'What prevents the husk from being swept away once its hollowness becomes visible? Is it identity-maintenance pressure (removing the ritual threatens national narrative), bureaucratic inertia (the apparatus has no alternative function), or genuine residual coordination value (the ritual still transmits some preparedness knowledge even in degraded form)?',
    'Analysis of attempted reforms: when stakeholders propose replacing memorial ritual with operational drills, what objections are raised and by whom? If objections center on loss of national identity or disrespect to disaster victims, identity-maintenance is the binding mechanism. If objections center on loss of institutional authority or budget, bureaucratic inertia is the mechanism. If objections center on loss of community cohesion or knowledge transmission, residual coordination is the mechanism.',
    'Identifies which beneficiary group has veto power over reform, revealing the actual extraction mechanism. If identity-maintenance apparatus blocks reform: extractiveness is higher than measured (includes symbolic authority extraction). If bureaucracy blocks reform: extractiveness is correctly measured (institutional rent extraction). If communities resist reform: husk reading may be incorrect — communities may perceive residual value not captured in the operational capacity metric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_blocked_mechanism, empirical, 'What mechanism prevents the piton from being removed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(husk_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(husk_theater_founding, husk_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(husk_tr_t10, husk_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(husk_tr_t20, husk_reading, theater_ratio, 20, 0.61).
narrative_ontology:measurement(husk_tr_t30, husk_reading, theater_ratio, 30, 0.73).
narrative_ontology:measurement(husk_tr_t40, husk_reading, theater_ratio, 40, 0.79).
narrative_ontology:measurement(husk_tr_t50, husk_reading, theater_ratio, 50, 0.81).

% Extraction over time
narrative_ontology:measurement(husk_extract_founding, husk_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(husk_be_t10, husk_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(husk_be_t20, husk_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement(husk_be_t30, husk_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(husk_be_t40, husk_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(husk_be_t50, husk_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(husk_suppress_founding, husk_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(husk_su_t10, husk_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(husk_su_t20, husk_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(husk_su_t30, husk_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(husk_su_t40, husk_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(husk_su_t50, husk_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(husk_reading, identity_coordination).
narrative_ontology:affects_constraint(husk_reading, competence_reading).

% DUAL FORMULATION NOTE:
% The husk reading and competence reading are alternative framings of the same preparedness-memorial institutional dynamics, linked via the preparedness_persistence kernel. They have different epsilon values because they describe different stable structures: the husk reading models identity-maintenance capture and operational atrophy (high extractiveness, very high theater); the competence reading models sustained capacity through mnemonic ritual (low extractiveness, low theater). Both cannot be true of the same system simultaneously — they are competing hypotheses about what preparedness-memorial institutions actually do, resolvable through empirical comparative analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
