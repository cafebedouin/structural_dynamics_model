% ============================================================================
% CONSTRAINT STORY: competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Preparedness Drills as Competence Maintenance (Competence Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   The competence_reading of preparedness persistence holds that drills and
 *   inspections are genuine competence maintenance mechanisms. Emergency
 *   response capacity decays without practice: personnel turn over,
 *   procedures are forgotten, coordination breaks down, and equipment
 *   familiarity atrophies. Regular drills solve this collective action
 *   problem by maintaining distributed operational knowledge across long
 *   intervals between real emergencies. This reading sees the preparedness
 *   apparatus as functional coordination (Rope) rather than theatrical
 *   legitimacy maintenance (Piton) or mixed coordination-extraction (Tangled
 *   Rope). The constraint's low extractiveness (0.12) and low theater ratio
 *   (0.15) reflect that drills primarily serve their stated function — they
 *   maintain real operational readiness rather than performing readiness for
 *   external audiences. The modest upward drift in both metrics over the
 *   10-year interval suggests some Goodhart pressure (drills becoming
 *   slightly more inspection-optimized) but not wholesale degradation. This
 *   reading is one of three sibling readings of the preparedness_persistence
 *   kernel; the others (husk_reading and hybrid_reading) interpret the same
 *   institutional apparatus as having atrophied into ritual or as mixing
 *   genuine coordination with performative extraction.
 *
 * KEY AGENTS:
 *   - Protected Population: Primary beneficiary (powerless/trapped) — benefits from maintained emergency response capacity; cannot exit jurisdiction but experiences drills as low-extraction coordination
 *   - Emergency Response Personnel: Primary beneficiary (moderate/constrained) — drills maintain their operational competence and coordination protocols; constrained exit but low experienced extraction
 *   - Coordinating Institutions: Primary beneficiary (institutional/mobile) — drills maintain interoperability and institutional memory; mobile exit but choose to maintain drill protocols because they work
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees drills as coordination solution to competence decay problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_reading, 0.12).
domain_priors:suppression_score(competence_reading, 0.08).
domain_priors:theater_ratio(competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(competence_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(competence_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_reading, rope).
narrative_ontology:human_readable(competence_reading, "Preparedness Drills as Competence Maintenance (Competence Reading)").
narrative_ontology:topic_domain(competence_reading, "disaster_preparedness/institutional_memory/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_reading, 'c4d592ef-6fc1-408e-b2b7-11174a50edc3').
narrative_ontology:cs_kernel_codification('c4d592ef-6fc1-408e-b2b7-11174a50edc3', formalized).
narrative_ontology:cs_authority_grounding('c4d592ef-6fc1-408e-b2b7-11174a50edc3', practice).
narrative_ontology:cs_interpretation_layer_present('c4d592ef-6fc1-408e-b2b7-11174a50edc3').
narrative_ontology:cs_reading_relation('c4d592ef-6fc1-408e-b2b7-11174a50edc3', competence_reading__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4d592ef-6fc1-408e-b2b7-11174a50edc3', competence_reading__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('c4d592ef-6fc1-408e-b2b7-11174a50edc3', foundational, drills_maintain_operational_capacity).
narrative_ontology:cs_axiom_status(drills_maintain_operational_capacity, holdable).
narrative_ontology:cs_axiom_grounding('c4d592ef-6fc1-408e-b2b7-11174a50edc3', drills_maintain_operational_capacity, empirically_contingent).
narrative_ontology:cs_axiom('c4d592ef-6fc1-408e-b2b7-11174a50edc3', foundational, competence_decays_without_practice).
narrative_ontology:cs_axiom_status(competence_decays_without_practice, holdable).
narrative_ontology:cs_axiom_grounding('c4d592ef-6fc1-408e-b2b7-11174a50edc3', competence_decays_without_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('c4d592ef-6fc1-408e-b2b7-11174a50edc3', operational_readiness_as_practiced_competence).
narrative_ontology:cs_drift_state('c4d592ef-6fc1-408e-b2b7-11174a50edc3', contemporary, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('c4d592ef-6fc1-408e-b2b7-11174a50edc3', '').
narrative_ontology:cs_kernel_id(competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_reading, protected_population).
narrative_ontology:constraint_beneficiary(competence_reading, emergency_response_personnel).
narrative_ontology:constraint_beneficiary(competence_reading, coordinating_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The population within the jurisdiction that emergency response systems are designed to protect. Cannot exit the jurisdiction during emergencies. Benefits from maintained operational readiness — when disasters strike, response effectiveness depends on whether personnel have practiced procedures and maintained coordination. Experiences drills as background infrastructure that occasionally disrupts daily life (road closures, alarm tests) but provides critical protection during actual emergencies.
narrative_ontology:constraint_stakeholder(competence_reading, protected_population, beneficiary,
    powerless, immediate, trapped, local).

% Firefighters, paramedics, emergency medical technicians, disaster response coordinators, and other personnel whose operational competence depends on regular practice. Drills maintain muscle memory for procedures, coordination protocols across departments, and familiarity with equipment. Can exit the profession but face career switching costs. Experience drills as necessary practice rather than extraction — the drills serve the function personnel need them to serve (maintaining their own competence and their teams' coordination).
narrative_ontology:constraint_stakeholder(competence_reading, emergency_response_personnel, beneficiary,
    moderate, biographical, constrained, regional).

% Emergency management agencies, fire departments, hospitals, and other institutions responsible for coordinating disaster response. Drills maintain interoperability across jurisdictions (mutual aid agreements depend on shared protocols) and preserve institutional memory through personnel turnover. Can relocate or restructure but choose to maintain drill protocols because they work. Benefit from maintained coordination infrastructure and from the legitimacy that comes from demonstrated preparedness, but the legitimacy is earned through actual function rather than performed through ritual.
narrative_ontology:constraint_stakeholder(competence_reading, coordinating_institutions, beneficiary,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Drills solve the competence decay problem: emergency response skills atrophy without practice, personnel turn over, and rare events mean long intervals between real exercises. Regular drills maintain distributed operational knowledge across shifts, departments, jurisdictions, and personnel transitions. They coordinate muscle memory (individual procedures), team coordination (cross-department protocols), and institutional memory (preserving knowledge through turnover).
% TRANSFER_FUNCTION: Drills transfer time and resources from daily operations to practice and coordination. Personnel spend hours in drills rather than other duties. Institutions allocate budget to drill planning, equipment, and coordination. The transfer is from present operational capacity to future emergency readiness — a temporal trade-off rather than an asymmetric extraction between parties.
% ABSENT_VOICES: No systematically excluded voices in this reading. The protected population participates passively (experiencing disruptions from drills) but benefits from the outcome. Emergency personnel participate actively and benefit from maintained competence. Institutions coordinate and benefit from interoperability. No party is excluded from the coordination or systematically bears costs without benefit.
% DISAPPEARANCE_RATIONALE: If drills disappeared overnight, emergency response effectiveness would degrade over time. Personnel would lose muscle memory for procedures. Coordination protocols would break down as personnel turned over without practicing handoffs. Equipment familiarity would atrophy. Interoperability across jurisdictions would erode. The world would rearrange: emergency response capacity would decline, and actual disaster outcomes (response times, casualty rates, resource mobilization) would worsen. The rearrangement would be gradual (competence decays over months to years, not overnight) but real.
% FOUNDING_PROBLEM: The founding problem is competence decay in low-frequency high-stakes domains. Emergency response skills are rarely used in real events (most jurisdictions experience major disasters infrequently), but when needed, they must be executed flawlessly under high stress. Without regular practice, skills atrophy, coordination breaks down, and institutional memory is lost through personnel turnover. Drills were instituted to maintain operational readiness across long intervals between real emergencies.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live, corroborated by: (1) Emergency response personnel (interviews consistently report that drills maintain skills that would otherwise atrophy). (2) After-action reports from real emergencies (jurisdictions with regular drill programs show better response outcomes than those without). (3) Institutional memory studies (organizations that maintain regular practice retain competence through personnel turnover; those that don't experience knowledge loss). (4) Skill retention research in other low-frequency high-stakes domains (aviation, military, surgery) shows similar decay curves and practice requirements. The corroboration comes from multiple seats: practitioners, outcome data, and cross-domain research, not just from the institutions running the drills.
narrative_ontology:disappearance_verdict(competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(competence_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROTECTED POPULATION (ROPE) — Drills are coordination: they maintain the operational readiness that protects the population during actual emergencies. The population cannot exit the jurisdiction but experiences the constraint as low-extraction coordination. Drills solve a genuine collective action problem (maintaining distributed competence across personnel turnover and long intervals between real events).
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: EMERGENCY RESPONSE PERSONNEL (ROPE) — Drills maintain muscle memory and coordination protocols. Personnel experience drills as necessary practice, not extraction. The constraint coordinates distributed knowledge across shifts, departments, and personnel turnover. Constrained exit (career switching costs) but low experienced extraction — the drills serve their stated function.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COORDINATING INSTITUTIONS (ROPE) — Institutions see drills as coordination infrastructure. The practice maintains interoperability across jurisdictions and preserves institutional memory through personnel transitions. Mobile exit (institutions can relocate or restructure) but choose to maintain drill protocols because they work. Low extraction, genuine coordination function.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, preparedness drills are a coordination solution to the competence decay problem: skills atrophy without practice, personnel turn over, and rare events mean long intervals between real exercises. Drills are the mechanism that maintains distributed operational knowledge. This reading sees the constraint as genuine coordination with minimal extraction — the drill apparatus serves its stated function and does not systematically benefit any party at others' expense.
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
 *   Extractiveness (0.12): Low. Drills impose time and resource costs on personnel and institutions, but these costs are proportional to the coordination function — maintaining operational readiness. No party systematically captures rents from the drill apparatus at others' expense. The slight upward drift (0.08 → 0.12) suggests modest Goodhart pressure as drill protocols become slightly more inspection-optimized, but extraction remains low. Suppression (0.08): Very low. Participation in drills is mandatory for emergency personnel, but this is a coordination requirement, not coercive extraction. Personnel can exit the profession (constrained exit, not trapped). Jurisdictions can modify drill protocols. The constraint does not suppress alternatives — jurisdictions experiment with different drill formats, frequencies, and technologies. Theater ratio (0.15): Low. Most drill activity is functional practice rather than performance. Personnel rehearse actual procedures, test equipment, and coordinate across departments. Some theater exists (drills scheduled for inspector visits, stylized performances for public relations), but it is a small fraction of total drill activity. The upward drift (0.10 → 0.15) indicates increasing performative content but not wholesale degradation into ritual.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all four perspectives classify as Rope. This uniformity is diagnostic: it reflects that the competence_reading interprets the preparedness apparatus as genuine coordination with no significant extraction structure. The protected population (powerless/trapped) sees coordination. Emergency personnel (moderate/constrained) see coordination. Institutions (institutional/mobile) see coordination. The analytical observer (analytical/analytical) sees coordination. The lack of perspectival gap distinguishes this reading from its siblings: husk_reading would show a gap (institutions see Piton, personnel see Snare or Tangled Rope), and hybrid_reading would show mixed types across perspectives. The uniform Rope classification is the competence_reading's structural signature.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents in this reading are beneficiaries. The protected population benefits from maintained emergency response capacity. Emergency personnel benefit from maintained competence and coordination protocols. Coordinating institutions benefit from interoperability and institutional memory. No victims are declared because the constraint does not systematically extract from any party — the costs (time, resources) are coordination costs, not asymmetric extraction. The engine derives low directionality values (d near 0.0) for all agents, producing low or negative effective extraction (chi). This is structurally appropriate: a genuine coordination mechanism should show low chi across all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The competence_reading resolves mandatrophy by asserting that the preparedness apparatus has NOT outlived its function. Drills maintain operational readiness; the mandate (emergency response capacity) remains live; the mechanism (regular practice) serves its stated purpose. This reading does NOT declare mandatrophy_resolved because the constraint's function is intact. The sibling readings (husk_reading and hybrid_reading) would declare mandatrophy_resolved if the drill apparatus persists after its competence-maintenance function has atrophied. The competence_reading's low extractiveness and low theater ratio are consistent with a functioning coordination mechanism, not a degraded ritual. The modest upward drift in both metrics over 10 years suggests early-stage Goodhart pressure but not mandatrophy — the constraint is drifting toward inspection-optimization but has not yet crossed the threshold where performance replaces function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is preparedness persistence a competence maintenance mechanism (this reading), a hollowed-out ritual maintained for institutional legitimacy (husk_reading), or a hybrid where some drills maintain competence while others are performative (hybrid_reading)?',
    'Longitudinal outcome analysis: correlation between drill frequency/quality and actual emergency response effectiveness. Compare jurisdictions with high vs low drill investment. Track personnel competence retention over time with and without regular drills. Measure whether drill protocols map to actual emergency procedures or diverge into stylized performance.',
    'If competence_reading: preparedness is Rope (genuine coordination). If husk_reading: preparedness is Piton (atrophied function, theatrical maintenance). If hybrid_reading: preparedness is Tangled Rope (coordination + extraction, with drill quality determining the mix). The structural element readings differ on: whether the drill apparatus maintains real operational capacity or has degraded into legitimacy theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, empirical, 'Kernel reading disambiguation: competence vs husk vs hybrid').

omega_variable(
    competence_decay_rate,
    'What is the actual decay rate of emergency response competence without regular practice, and does drill frequency match that rate?',
    'Measure skill retention curves for emergency procedures. Compare drill intervals to competence half-lives. Identify whether current drill schedules are calibrated to actual decay rates or to institutional convenience/budget cycles.',
    'If drill frequency matches decay rate: coordination function confirmed. If drills are more frequent than needed: suggests extraction or theater. If less frequent: suggests under-investment or competence gaps.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_decay_rate, empirical, 'Competence decay rate vs drill frequency calibration').

omega_variable(
    drill_protocol_fidelity,
    'Do drill protocols accurately reflect actual emergency procedures, or have they diverged into stylized performances optimized for inspection rather than operational readiness?',
    'Compare drill procedures to actual emergency response protocols. Interview personnel about differences between ''drill mode'' and ''real event mode''. Analyze after-action reports from real emergencies for protocol deviations that were not practiced in drills.',
    'High fidelity: competence_reading confirmed. Low fidelity with divergence toward inspection-optimized performance: evidence for husk_reading or hybrid_reading. Divergence indicates Goodhart drift — the drill has become the target rather than the proxy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_protocol_fidelity, empirical, 'Drill protocol fidelity to actual emergency procedures').

omega_variable(
    cs_framing_underdetermination,
    'Is the preparedness kernel best framed as (a) the drill apparatus itself (kernel = the practice of conducting drills), or (b) the operational readiness commitment that drills are meant to maintain (kernel = the standing commitment to emergency response capacity)?',
    'Examine what authority structures treat as authoritative. If drill compliance is the measure of legitimacy regardless of actual readiness, framing (a) is operative. If readiness outcomes (response times, casualty rates, resource mobilization) are the measure, framing (b) is operative. The framings produce different cs_pattern classifications: (a) treats drills as the kernel with potential codification_collapse if drills persist without maintaining readiness; (b) treats readiness as the kernel with drills as one interpretation layer.',
    'Framing (a): drill apparatus is the kernel; husk_reading becomes more plausible (the kernel itself has atrophied). Framing (b): readiness commitment is the kernel; competence_reading is more plausible (drills are a functioning interpretation layer maintaining the commitment). The choice affects whether degraded drills represent kernel collapse or interpretation-layer failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'CS framing: drill apparatus vs readiness commitment as kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_read_tr_t0, competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comp_read_tr_t3, competence_reading, theater_ratio, 3, 0.12).
narrative_ontology:measurement(comp_read_tr_t6, competence_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement(comp_read_tr_t10, competence_reading, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(comp_read_be_t0, competence_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(comp_read_be_t3, competence_reading, base_extractiveness, 3, 0.09).
narrative_ontology:measurement(comp_read_be_t6, competence_reading, base_extractiveness, 6, 0.11).
narrative_ontology:measurement(comp_read_be_t10, competence_reading, base_extractiveness, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% The competence_reading is one of three sibling readings of the preparedness_persistence kernel. It does not affect other constraints but is structurally linked to husk_reading and hybrid_reading via the kernel. The three readings form a constraint family where the same institutional apparatus (drills, inspections, protocols) is interpreted as functional coordination (competence_reading), atrophied ritual (husk_reading), or mixed coordination-extraction (hybrid_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
