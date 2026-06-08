% ============================================================================
% CONSTRAINT STORY: competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_reading, []).

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
 *   constraint_id: competence_reading
 *   human_readable: Preparedness Sustained Through Exercised Operational Knowledge (Competence Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint story models ONE READING of the preparedness_persistence
 *   kernel — the competence reading, where preparedness regimes are
 *   maintained through exercised operational knowledge rather than memorial
 *   obligation. The same institutional forms (drills, inspections,
 *   certifications, stockpile rotations) can instantiate either this reading
 *   or its sibling (husk_reading), depending on whether the metrics measure
 *   actual capability or theatrical compliance. In the competence reading,
 *   drill participation is measured by skill retention curves, inspection
 *   routines verify structural integrity rather than paperwork compliance,
 *   emergency stockpiles are rotated based on expiration dates and tested for
 *   functionality, and certification renewals filter out actual skill decay.
 *   The constraint coordinates institutional memory: procedural knowledge is
 *   encoded in muscle memory through repeated practice, equipment familiarity
 *   reduces response-time variance, and mutual aid agreements maintain
 *   resource-sharing capability across jurisdictions. The theater_ratio is
 *   low (0.20) because most activity is functional — drills maintain
 *   competence, inspections catch real deficiencies, certifications track
 *   actual capability. The extractiveness is low (0.18) because the overhead
 *   (time, resources, bureaucratic process) is proportional to the
 *   coordination function: maintaining distributed operational readiness for
 *   low-frequency high-consequence events requires sustained practice, and
 *   the constraint organizes that practice efficiently.
 *
 * KEY AGENTS:
 *   - At-Risk Population: Primary beneficiary (powerless/trapped) — benefits from functional preparedness capability maintained through the constraint's coordination function
 *   - Emergency Response Agency: Primary coordinator (institutional/constrained) — maintains drill schedules, inspection protocols, certification standards, and mutual aid agreements; both coordinates and benefits
 *   - Response Personnel: Secondary beneficiary (moderate/mobile) — drill participation maintains skill retention and team coordination; can exit to other careers but benefits while participating
 *   - Standards Development Coalition: Organized agents (organized/mobile) — FEMA, NFPA, professional associations updating preparedness standards based on operational feedback and post-event analysis; coordinates evolving best practices
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the constraint as coordination infrastructure for maintaining institutional memory across low-frequency events
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_reading, 0.18).
domain_priors:suppression_score(competence_reading, 0.25).
domain_priors:theater_ratio(competence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(competence_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(competence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(competence_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_reading, rope).
narrative_ontology:human_readable(competence_reading, "Preparedness Sustained Through Exercised Operational Knowledge (Competence Reading)").
narrative_ontology:topic_domain(competence_reading, "disaster_preparedness/institutional_memory/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_reading, '79e3faa2-1b2a-4bfd-b46a-1c5342f736d6').
narrative_ontology:cs_kernel_codification('79e3faa2-1b2a-4bfd-b46a-1c5342f736d6', formalized).
narrative_ontology:cs_authority_grounding('79e3faa2-1b2a-4bfd-b46a-1c5342f736d6', expertise).
narrative_ontology:cs_interpretation_layer_present('79e3faa2-1b2a-4bfd-b46a-1c5342f736d6').
narrative_ontology:cs_reading_relation('79e3faa2-1b2a-4bfd-b46a-1c5342f736d6', competence_reading__husk_reading, influences).
narrative_ontology:cs_axiom('79e3faa2-1b2a-4bfd-b46a-1c5342f736d6', foundational, preparedness_requires_exercised_competence).
narrative_ontology:cs_axiom_status(preparedness_requires_exercised_competence, holdable).
narrative_ontology:cs_axiom_grounding('79e3faa2-1b2a-4bfd-b46a-1c5342f736d6', preparedness_requires_exercised_competence, empirically_contingent).
narrative_ontology:cs_axiom('79e3faa2-1b2a-4bfd-b46a-1c5342f736d6', foundational, metrics_must_track_capability).
narrative_ontology:cs_axiom_status(metrics_must_track_capability, holdable).
narrative_ontology:cs_axiom_grounding('79e3faa2-1b2a-4bfd-b46a-1c5342f736d6', metrics_must_track_capability, instrumental).
narrative_ontology:cs_reference_frame('79e3faa2-1b2a-4bfd-b46a-1c5342f736d6', post_event_operational_effectiveness).
narrative_ontology:cs_drift_state('79e3faa2-1b2a-4bfd-b46a-1c5342f736d6', contemporary_compliance_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('79e3faa2-1b2a-4bfd-b46a-1c5342f736d6', '').
narrative_ontology:cs_kernel_id(competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_reading, at_risk_population).
narrative_ontology:constraint_beneficiary(competence_reading, response_personnel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_reading, emergency_response_agency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents in hazard zones (earthquake fault lines, flood plains, wildfire corridors) who cannot relocate but benefit from functional emergency response capability. Drill participation teaches evacuation routes and shelter procedures. Inspection certifications verify that buildings meet structural integrity standards. Emergency stockpiles (water, medical supplies, backup power) are maintained and accessible. The population experiences the constraint as essential coordination: the overhead (drill participation time, inspection fees embedded in rent or property taxes) is proportional to the safety benefit received.
narrative_ontology:constraint_stakeholder(competence_reading, at_risk_population, beneficiary,
    powerless, immediate, trapped, local).

% Municipal or regional emergency management authority responsible for coordinating preparedness activities. Schedules and conducts drills, maintains inspection protocols, certifies personnel, manages mutual aid agreements with neighboring jurisdictions, and operates emergency stockpiles. Constrained by budget allocations and regulatory mandates but also the primary coordination beneficiary: drill schedules maintain personnel skill retention, equipment maintenance logs ensure operational readiness, and the agency's institutional memory (procedural documentation, training curricula, after-action reviews) accumulates over time. The agency both sets the preparedness agenda and benefits from its coordination function.
narrative_ontology:constraint_stakeholder(competence_reading, emergency_response_agency, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(competence_reading, emergency_response_agency, beneficiary).

% Firefighters, paramedics, search-and-rescue teams, emergency dispatchers who participate in drills and maintain certifications. Mobile in the sense that they can change careers, but while employed in emergency services they benefit from the constraint's coordination function. Drill participation maintains muscle memory for rare procedures (confined space rescue, hazmat response, mass casualty triage), equipment familiarity reduces response-time variance under stress, and team coordination exercises build trust and communication efficiency. Certification renewals verify current competence and filter out skill decay. The constraint coordinates distributed expertise: no single responder knows all procedures, but the drill regime ensures that procedural knowledge is distributed across the team and accessible when needed.
narrative_ontology:constraint_stakeholder(competence_reading, response_personnel, beneficiary,
    moderate, biographical, mobile, local).

% Federal agencies (FEMA, CDC, OSHA), professional associations (NFPA, IAFC, NAEMT), and academic researchers who develop and update preparedness standards. Mobile across different standard-setting domains (can shift focus between hazard types or professional fields). The coalition coordinates evolving best practices: drill protocols are updated to incorporate lessons from real events (post-Hurricane Katrina updates to evacuation procedures, post-9/11 updates to building codes), inspection standards tighten as engineering knowledge advances (seismic retrofitting requirements, wildfire-resistant construction), and certification requirements adapt to new technologies (drone-assisted search and rescue, telemedicine for remote triage). The coalition experiences the constraint as coordination infrastructure for institutional learning: the overhead (consensus-building process, research validation, stakeholder review) is proportional to the complexity of maintaining current operational knowledge across diverse hazard contexts.
narrative_ontology:constraint_stakeholder(competence_reading, standards_development_coalition, agenda_setter,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains distributed operational readiness for low-frequency high-consequence events through exercised practice. Solves the institutional memory problem: procedural knowledge decays between rare events, equipment familiarity atrophies, team coordination degrades, and structural vulnerabilities accumulate unnoticed. The constraint coordinates sustained competence maintenance across rotating personnel, evolving best practices, and multi-jurisdictional resource sharing.
% TRANSFER_FUNCTION: Attention and resources flow from at-risk population and emergency personnel (drill participation time, training investment) and from public budgets (inspection costs, stockpile maintenance, certification administration) into distributed preparedness capability. The transfer is proportional: participants pay with time and attention, receive safety capability in return. No asymmetric extraction: no party collects rents from the arrangement beyond compensation for coordination services rendered.
% ABSENT_VOICES: Future disaster victims who are not yet in the hazard zone (have not moved to the region, have not been born) and whose interests are not represented in current preparedness decisions. Also: low-probability high-consequence scenarios that are not drilled because they seem too unlikely or too expensive to prepare for (asteroid impact, coordinated infrastructure attack, novel pandemic pathogen). These absent voices would object to under-preparedness for tail risks, but they are not in the room because the coordination regime optimizes for known hazards with established response protocols.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared overnight — drills stopped, inspections ceased, certifications lapsed, stockpiles expired — the world would rearrange itself. Initial response capability would persist through residual muscle memory and existing infrastructure, but capability would decay rapidly: personnel would forget rare procedures, equipment would degrade unmaintained, structural vulnerabilities would accumulate undetected, and mutual aid agreements would atrophy unused. The first major event after constraint disappearance would reveal the accumulated capability loss through higher casualty rates, longer response times, and coordination failures. The constraint is not a natural fact (disasters would still occur without it) but a coordination mechanism that maintains capability between events. Its disappearance would force a rearrangement: either ad-hoc reconstruction of preparedness infrastructure after a catastrophic failure, or explicit acceptance of higher disaster risk as a cost-saving measure.
% FOUNDING_PROBLEM: The founding problem is institutional memory decay for low-frequency high-consequence events. When disasters occur rarely (once per generation or less), procedural knowledge decays between events, personnel turnover loses experiential learning, equipment maintenance lapses, structural vulnerabilities accumulate, and coordination protocols atrophy. The problem: how to maintain readiness for events that most personnel will never experience, using skills that decay without practice, across organizations that must coordinate under stress despite having no routine interaction. The constraint was built to solve this problem through sustained exercise: drills maintain procedural memory, inspections verify structural integrity, certifications filter competence, and mutual aid agreements institutionalize resource-sharing before events occur.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live, corroborated by: (1) Post-event analysis from FEMA and NTSB consistently identifying coordination failures, procedural errors, and infrastructure vulnerabilities that pre-event preparedness regimes failed to catch. (2) Academic disaster research (Tierney, Quarantelli, Dynes) documenting institutional memory decay as a persistent problem across hazard types and jurisdictions. (3) Response personnel testimony (after-action reviews, professional association surveys) reporting skill decay between drills and competence loss from inadequate training frequency. (4) Structural engineering assessments identifying undetected seismic vulnerabilities in buildings that passed compliance inspections — evidence that inspection regimes do catch real deficiencies, confirming the problem is not solved by one-time construction standards alone. Corroboration is multi-sourced and includes actors outside the direct beneficiary set (academic researchers, post-event investigators, structural engineers), not merely self-reported by emergency management agencies defending their budgets.
narrative_ontology:disappearance_verdict(competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(competence_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AT-RISK POPULATION (ROPE) — Trapped in the hazard zone but benefits immediately from the constraint's coordination function. Drills teach evacuation routes, inspection certifications verify structural integrity, emergency stockpiles are maintained and rotated. Low extractiveness: the coordination cost (drill participation time, inspection fees) is proportional to the coordination benefit (functional preparedness capability). Experiences the constraint as genuine coordination solving the real problem of maintaining response readiness.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: EMERGENCY RESPONSE AGENCY (ROPE) — Constrained by resource requirements and regulatory obligations but also the primary coordination beneficiary. Drill schedules maintain personnel skill retention, equipment maintenance logs ensure operational readiness, mutual aid agreements enable resource sharing. The agency experiences the constraint as coordination infrastructure: the enforcement overhead is functional verification, not performative compliance. Low extractiveness because the agency both coordinates and benefits from the coordination.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RESPONSE PERSONNEL (ROPE) — Mobile (can change careers) but benefits from the constraint while participating. Drill participation maintains muscle memory and team coordination; certification renewals verify current competence; equipment familiarity reduces response-time variance. The constraint coordinates skill retention across rotating personnel. Low extractiveness: training time is proportional to competence maintenance, not inflated by theatrical compliance.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: STANDARDS DEVELOPMENT COALITION (ROPE) — Organized actors (FEMA, NFPA, professional associations) developing and updating preparedness standards based on post-event analysis and operational feedback. Mobile across different standard-setting domains. The constraint coordinates evolving best practices: drill protocols update to incorporate lessons from real events, inspection standards tighten as engineering knowledge advances. Low extractiveness: standard development overhead is proportional to the coordination complexity of maintaining current operational knowledge across diverse hazard contexts.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From civilizational/global scope, preparedness-through-competence is a coordination mechanism that solves the genuine problem of maintaining readiness for low-frequency high-consequence events. The constraint coordinates institutional memory: drill participation encodes procedural knowledge, inspection routines verify structural integrity, certification renewals filter out skill decay. The overhead (time, resources, bureaucratic process) is proportional to the coordination function (maintaining distributed operational capability). Not a false summit: the constraint's function is actual preparedness capability, measurable through response-time metrics and failure-rate analysis.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_reading_tests).
:- end_tests(competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint's overhead is proportional to its coordination function. Drill participation time, inspection fees, certification renewal costs, and stockpile maintenance budgets are the minimum necessary to maintain distributed operational readiness. No identifiable beneficiary extracts rents from the arrangement — the emergency response agency coordinates but does not profit, standards bodies operate on cost-recovery, at-risk populations pay only for the service they receive. The slight extractiveness (not zero) reflects bureaucratic friction and the fact that some drill protocols persist past the point of marginal competence gain, but this is coordination overhead rather than asymmetric extraction. The increasing trajectory (0.12 → 0.18) reflects gradual accretion of compliance requirements as the regime matures, but the constraint remains well below the rope/tangled_rope threshold. Suppression (0.25): Low-moderate. Participation in drills and inspections is mandatory for regulated entities (building owners, certified personnel, emergency services), and non-compliance carries penalties. But the suppression is not coercive extraction — it enforces genuine coordination (maintaining readiness) rather than rent collection. The slight increase (0.20 → 0.25) reflects tightening enforcement as regulatory maturity increases. Theater ratio (0.20): Low. Most constraint activity is functional: drills test actual procedures under realistic conditions, inspections verify structural integrity and equipment operability, certifications filter competence, stockpile rotations replace expired supplies. Some theater exists (attendance logging without skill measurement, checklist completion without understanding, symbolic drills with no decision pressure), but it is a small fraction of total activity. The increasing trajectory (0.15 → 0.20) indicates gradual drift toward metrics-as-goals, but the constraint remains substantially functional. This trajectory is the early-warning signal: if theater_ratio continues rising while extractiveness remains low, the constraint is transitioning from competence_reading toward husk_reading — the form persists but the function atrophies.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a UNIFORM CLASSIFICATION across perspectives — all five perspectives classify as rope, reflecting that the constraint is pure coordination with no identifiable extraction mechanism. The perspectival invariance is itself diagnostic: when a constraint coordinates without extracting, all observers agree on its function regardless of their structural position. The at-risk population (powerless/trapped) sees functional safety infrastructure, the emergency agency (institutional/constrained) sees coordination overhead proportional to mission, response personnel (moderate/mobile) see skill maintenance with fair compensation, standards bodies (organized/mobile) see evolving best practices, and the analytical observer (analytical/analytical) sees institutional memory infrastructure. No perspective experiences the constraint as snare or tangled_rope because no asymmetric extraction exists. The LACK of perspectival gap is the signal: pure coordination looks like coordination from all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared stakeholders are beneficiaries — the constraint coordinates a genuine collective action problem (maintaining readiness for rare events) with no asymmetric extraction. The at-risk population, response personnel, and emergency agency all benefit from functional preparedness capability. No victims are declared because the coordination overhead is proportional to the coordination benefit. Directionality values derive from beneficiary status + exit options: trapped beneficiaries (at-risk population) experience the constraint as essential coordination with no exit, constrained beneficiaries (emergency agency) experience it as resource-intensive but necessary infrastructure, mobile beneficiaries (response personnel, standards bodies) experience it as valuable while participating but can exit to other domains. The engine will compute low directionality (toward beneficiary end) for all perspectives, producing low or negative effective extraction across the board — confirming the rope classification from all indexed perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING FRAME: This constraint instantiates the competence_reading of the preparedness_persistence kernel. The mandatrophy question for this kernel is NOT 'has the constraint's original function been achieved?' but 'which reading does the constraint currently instantiate?' The preparedness regime's founding problem (maintaining readiness for low-frequency high-consequence events) remains live — earthquakes, floods, pandemics, and industrial accidents continue to occur. But the institutional form that addresses this problem can degrade from functional competence maintenance (this reading) to memorial ritual (husk_reading sibling) without changing its surface appearance. The resolution mechanism is empirical: does drill participation correlate with skill retention? Do inspection findings correlate with structural failures? Do certification renewals filter actual competence decay? If yes: competence_reading is structurally accurate. If no: the constraint has transitioned to husk_reading — the form persists but the function has atrophied. The measurements show early drift (theater_ratio 0.15 → 0.20, extractiveness 0.12 → 0.18) indicating the constraint is beginning to accumulate performative overhead, but it remains substantially functional. The omega variables document the testable hypotheses that would determine which reading applies as the constraint continues to evolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does this preparedness regime measure and maintain actual operational competence, or has it degraded into memorial ritual with metrics-as-theater?',
    'Longitudinal comparison: (1) skill retention curves for drill participants vs non-participants, (2) actual response-time variance in real events vs predicted by drill performance, (3) inspection finding rates vs structural failure rates, (4) certification status vs operational errors in real deployments. If metrics correlate with capability: competence reading is structurally accurate. If metrics decorrelate: husk reading applies — the regime has become theatrical.',
    'This omega documents the committer-frame uncertainty: this constraint is one reading (competence_reading) of the preparedness_persistence kernel. The sibling reading (husk_reading) describes the same institutional form degraded into memorial ritual where drill participation rates are tracked but skill decay is not measured, inspections verify paperwork compliance rather than structural integrity, and the regime persists through commemorative obligation rather than functional necessity. The resolution mechanism is the correlation between authored metrics and actual capability — a testable empirical question that determines which reading the constraint has become.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Competence vs memorial-ritual reading disambiguation for preparedness_persistence kernel').

omega_variable(
    drill_realism_threshold,
    'At what fidelity threshold do drills stop maintaining competence and become performative compliance?',
    'Controlled comparison of drill realism levels vs actual-event performance: high-fidelity scenario-based exercises with real-time decision pressure vs low-fidelity tabletop exercises vs mere attendance logging. Measure skill transfer to real events across fidelity levels. Identify the threshold below which drill participation no longer predicts operational capability.',
    'If threshold is high (near-realistic scenarios required): many current drill regimes are already theatrical, and the competence reading mischaracterizes their function. If threshold is low (even simplified drills maintain coordination): the competence reading is robust across a wide range of implementation quality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_realism_threshold, empirical, 'Drill fidelity threshold for competence maintenance vs theatrical compliance').

omega_variable(
    institutional_memory_decay_rate,
    'How quickly does operational knowledge decay in the absence of exercised practice, and does the drill interval match the decay curve?',
    'Measure skill retention curves for emergency procedures as a function of time since last practice. Compare organizational drill frequency to the empirically measured half-life of procedural memory. Identify mismatches where drill intervals exceed retention windows (competence decays between drills) or vastly undershoot retention windows (excess drilling becomes ritual rather than maintenance).',
    'If drill intervals match decay curves: the constraint''s overhead is proportional to the coordination function. If intervals are mismatched: either the regime under-drills (competence reading fails — preparedness is illusory) or over-drills (transitioning toward memorial ritual — moving from competence_reading toward husk_reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_memory_decay_rate, empirical, 'Drill interval calibration to procedural memory decay rates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_prep_theater_t0, competence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comp_prep_theater_t3, competence_reading, theater_ratio, 3, 0.17).
narrative_ontology:measurement(comp_prep_theater_t6, competence_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(comp_prep_theater_t10, competence_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(comp_prep_extract_t0, competence_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(comp_prep_extract_t3, competence_reading, base_extractiveness, 3, 0.14).
narrative_ontology:measurement(comp_prep_extract_t6, competence_reading, base_extractiveness, 6, 0.16).
narrative_ontology:measurement(comp_prep_extract_t10, competence_reading, base_extractiveness, 10, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(comp_prep_suppress_t0, competence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(comp_prep_suppress_t10, competence_reading, suppression_requirement, 10, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_reading, husk_reading).

% DUAL FORMULATION NOTE:
% The competence_reading and husk_reading constraints form a kernel family. They describe the same institutional form (drills, inspections, certifications) but model different structural states: competence_reading when the form maintains actual operational capability, husk_reading when the form has degraded into memorial theater. The constraints are linked because the competence reading can transition to the husk reading over time if metrics decorrelate from capability (rising theater_ratio with stable or declining functional performance). The network edge is unidirectional: competence_reading influences husk_reading (sets the institutional baseline that can degrade) but husk_reading does not influence competence_reading (a degraded system must be rebuilt, not merely maintained).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
