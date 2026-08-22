% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__competence_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Preparedness as Live Competence Retention
 *   domain: disaster_preparedness/institutional_memory/governance
 *
 * SUMMARY:
 *   This constraint instantiates the COMPETENCE READING of the preparedness
 *   kernel: preparedness is live exercised knowledge. Drills and
 *   competence-verification inspections are necessary practices that maintain
 *   operational capacity in disaster response institutions. This reading
 *   treats preparedness as an active coordination function—the solution to
 *   skill atrophy in rare, high-stakes domains—rather than as ritual
 *   performance or institutional memory theater. The competence reading is
 *   distinguished from the husk reading (preparedness as memorial performance
 *   without live competence) and the hybrid reading (competence retained only
 *   in specialized institutions while broader society becomes ceremonial) by
 *   its structural claim: competence is preserved through routine exercise,
 *   resource allocation optimizes for skill retention, and beneficiaries are
 *   population safety and the institutions that maintain capacity. The
 *   measured extractiveness is low (0.18 at interval end) because the
 *   constraint does not concentrate rents on identifiable actors; its costs
 *   are diffuse operational burden and opportunity cost distributed across
 *   institutions. Theater is minimal (0.12) because the functional and
 *   performative components are tightly coupled—the drill IS the competence
 *   rehearsal; ceremony cannot be separated from competence without
 *   destroying the function.
 *
 * KEY AGENTS:
 *   - Disaster response institutions (fire, rescue, hazmat, emergency management): institutional actors that set drill cadence and standards; bear the coordination constraint
 *   - Specialized technical institutions (water boards, engineering authorities): maintain expert pathways through institutionalized exercise regimes
 *   - Operational personnel (responders, technicians): experience drills as professional necessity and skill-maintenance investment
 *   - Population at risk (downstream of hazmat, floods, industrial hazards): ultimate beneficiaries; cannot observe competence directly
 *   - Fiscal oversight bodies (budget offices, auditors): question whether live drills are resource-efficient
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.08).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Preparedness as Live Competence Retention").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "disaster_preparedness/institutional_memory/governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, '8093ded7-a63b-4957-be21-63886144726b').
narrative_ontology:cs_kernel_codification('8093ded7-a63b-4957-be21-63886144726b', distributed).
narrative_ontology:cs_authority_grounding('8093ded7-a63b-4957-be21-63886144726b', practice).
narrative_ontology:cs_interpretation_layer_present('8093ded7-a63b-4957-be21-63886144726b').
narrative_ontology:cs_reading_relation('8093ded7-a63b-4957-be21-63886144726b', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('8093ded7-a63b-4957-be21-63886144726b', preparedness_retention__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('8093ded7-a63b-4957-be21-63886144726b', foundational, competence_requires_live_exercise).
narrative_ontology:cs_axiom_status(competence_requires_live_exercise, holdable).
narrative_ontology:cs_axiom_grounding('8093ded7-a63b-4957-be21-63886144726b', competence_requires_live_exercise, empirically_contingent).
narrative_ontology:cs_axiom('8093ded7-a63b-4957-be21-63886144726b', foundational, ceremony_and_competence_remain_coupled).
narrative_ontology:cs_axiom_status(ceremony_and_competence_remain_coupled, holdable).
narrative_ontology:cs_axiom_grounding('8093ded7-a63b-4957-be21-63886144726b', ceremony_and_competence_remain_coupled, empirically_contingent).
narrative_ontology:cs_reference_frame('8093ded7-a63b-4957-be21-63886144726b', competence_through_live_exercise).
narrative_ontology:cs_drift_state('8093ded7-a63b-4957-be21-63886144726b', contemporary_fiscal_pressure_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('8093ded7-a63b-4957-be21-63886144726b', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, population_safety_outcomes).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, operational_disaster_response_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, specialized_technical_institutions).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, operational_personnel).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, population_at_risk).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, operational_personnel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Emergency management agencies, fire/rescue services, water boards, and civil defense organizations design and execute drills and inspections to maintain operational competence. They decide frequency, scope, and performance standards. They argue that regular exercise of response protocols is the only mechanism preventing skill atrophy and ensuring actual readiness rather than ceremonial compliance. Their constraint is the recurring resource commitment and opportunity cost of conducting live exercises instead of routine operations.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, disaster_response_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Water authorities, structural engineering inspection bodies, and specialized technical agencies benefit from institutionalized drill cycles that keep expert knowledge current and pathways clear for training the next generation. They have maintained robust exercise regimes and can defend resource allocation for drills as essential to competence.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, specialized_technical_institutions, beneficiary,
    institutional, generational, mobile, regional).

% Firefighters, emergency responders, rescue personnel, and hazmat technicians participate in drills and inspections. They benefit from clear protocols, muscle memory, and equipment familiarity that live exercise preserves. They also bear the immediate cost: time spent in drills is time away from other duties, training is attention-intensive, and failure to maintain competence erodes professional identity and career advancement.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, operational_personnel, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, operational_personnel, payer).

% Communities downstream of dams, in flood plains, near hazmat facilities, and in disaster-prone regions depend on competent emergency response. They benefit from drills and inspections that ensure responders can actually execute plans, not just recite them. They have no exit option and cannot directly observe competence; they must trust the institution's commitment to live exercise.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, population_at_risk, beneficiary,
    powerless, immediate, trapped, local).

% Budget offices, auditors, and legislative committees review disaster preparedness spending. They question whether live drills are efficient, whether simulation or classroom training might substitute at lower cost, and whether resources might be redirected to other priorities. They operate from a cost-minimization frame rather than a competence frame.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, fiscal_oversight_bodies, observer,
    institutional, generational, analytical, national).

% An abstraction: the collective outcome of lives saved, injuries prevented, and disasters managed competently. The abstract beneficiary that the entire system's incentive is oriented toward. Not an agent, but named here because the constraint story's vindication is measured against this outcome.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, population_safety_outcomes, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(preparedness_retention__competence_reading, population_safety_outcomes).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of skill atrophy in low-frequency, high-stakes domains: disaster response is rare enough that knowledge degrades between events, yet when it occurs, execution must be immediate and competent. Live drills create a rehearsal mechanism that preserves procedural memory and pattern recognition without waiting for actual catastrophe to test the system.
% TRANSFER_FUNCTION: Moves resources—time, fuel, equipment wear, personnel attention—from routine operations into repeated exercise cycles. The transfer is from agencies and their daily capacity for other services toward the preservation of emergency capacity. No party extracts this; it flows toward the coordination function itself.
% ABSENT_VOICES: Populations in low-hazard regions who receive the same preparedness investment without local risk; marginal cost-benefit analysts who would argue for probabilistic allocation (more drills where hazard risk is highest, fewer where it is negligible); technicians and responders from institutions where competence has degraded and routine drills would expose the atrophy.
% DISAPPEARANCE_RATIONALE: If mandatory drill cycles and competence-verification inspections disappeared, response agencies would face a skill-retention crisis within 2–3 years: institutional memory would decay, personnel would lose pattern recognition for rare event types, and response times to actual disasters would lengthen measurably. Communities would bear elevated risk. The world would rearrange because disaster response capacity is a public good that requires continuous exercise to maintain.
% FOUNDING_PROBLEM: Institutional knowledge in low-frequency, high-stakes domains fades. Disaster response happens rarely; between events, personnel retire, technical knowledge becomes outdated, equipment familiarity deteriorates, and procedural memory attenuates. Yet when disaster strikes, response must be immediate and expert. How do you preserve competence in a domain where real-world performance happens rarely and cannot be scheduled?
% FOUNDING_PROBLEM_CORROBORATION: Disaster response institutions, engineering bodies, and water authorities attest the problem is live and urgent: post-event review boards consistently identify personnel skill gaps, equipment unfamiliarity, and protocol knowledge loss as contributing factors to slower, less effective response. Insurance actuaries and civil protection academies corroborate that response capacity deteriorates measurably without regular exercise. No independent source contests the founding problem's liveness.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__competence_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__competence_reading_tests).
:- end_tests(preparedness_retention__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because no identified actor systematically collects rents from the constraint. Resources flow to the coordination function (skill preservation) rather than to a concentrated beneficiary. Suppression is minimal (0.08) because participation is structurally voluntary at the institutional level—agencies adopt drill regimes because operational competence is their mandate, not because coercion forces compliance. Theater is modest (0.12) because drills are measured against actual competence: failed drills reveal skill gaps, inspection findings drive corrective action, and post-event audits compare predicted performance (from drill records) against actual response. The function is self-validating. The measurement series shows extractiveness and theater creeping upward slightly from t=0 to t=25, then stabilizing—this reflects the historical expansion of formal drill requirements and inspection standardization, a gradual shift toward more structured (and slightly more ceremonial) exercise regimes, but not a fundamental degradation. The near-flat suppression profile indicates the constraint operates through institutional alignment, not enforcement intensity.
 *
 * PERSPECTIVAL GAP:
 *   From the disaster response institution's perspective, this is a pure coordination problem they solve by maintaining exercise cycles—no extraction, no coercion, just functional necessity. From the fiscal oversight perspective, the same resource expenditure looks excessive: simulation, tabletop exercises, and classroom training might be cheaper proxies. From the operational personnel's perspective, drills are both competence-preserving (necessary for their expertise) and time-consuming (opportunity cost against other duties). The engine should compute these divergences from the structural data without the commentary mediating them. The competence reading's claim is that this divergence is smaller than it would be under the husk reading (where agencies would see ritual performance replacing competence, suppression would be higher because the constraint would need active defense against skeptics, and theater would dominate).
 *
 * DIRECTIONALITY LOGIC:
 *   No identified actor is a concentrated target. Disaster response institutions are the agenda-setters (they choose drill cadence) but they are also the primary payers (they bear the resource cost). Operational personnel are beneficiaries (they retain competence and professional standing) and payers (they invest time in drills). The population at risk are pure beneficiaries (safer response outcomes) with no exit option. This symmetric or near-symmetric cost-benefit structure keeps directionality near the neutral zone (d near 0.5 for most seats) rather than producing clear targets or beneficiaries. The constraint is genuinely coordinating, not extractive, because the primary beneficiary (population safety) is abstract and external, not an actor collecting rents.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading frames preparedness as a live coordination problem (skill retention in rare-event domains) rather than a zombie mandate (procedures maintained ceremonially long after the founding problem is solved). The competence reading AVOIDS mandatrophy classification because the founding problem (institutional knowledge decay) remains live and the constraint's operation continues to solve it measurably. The measurement series shows modest drift toward more formalized inspection regimes (slight theater increase), but this is not mandatrophy—it is professionalization and standardization, which can enhance competence if aligned with actual performance criteria. If this reading were to drift into mandatrophy, it would be when theater_ratio exceeded 0.4–0.5 (ceremony becoming dominant) or when drill failures stopped triggering corrective action (functional coupling broke). Neither signal is present at interval end.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_ceremony_decoupling,
    'Can drills and inspections become ceremonial theater while still functioning as competence preservation, or must ceremony and competence remain tightly coupled?',
    'Post-drill performance audits: compare actual disaster response times, failure modes, and personnel knowledge against predicted performance from drill data. If actual response deteriorates while drills remain ceremonially robust, the functions have decoupled.',
    'If decoupling occurs, the constraint should be reclassified toward the husk reading (preparedness as memorial performance). If coupling remains tight, the competence reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_ceremony_decoupling, empirical, 'Whether ceremony and competence preservation remain functionally coupled or degrade independently.').

omega_variable(
    specialized_vs_diffuse_competence_retention,
    'Is competence retention stratified across institutions—concentrated in specialized technical bodies while general emergency management becomes ceremonial—or is it distributed and maintained uniformly?',
    'Comparative institutional audit: evaluate competence retention metrics (response times, skill assessments, equipment familiarity, protocol knowledge) across different institution types and scales. If specialized institutions maintain high competence while general population and municipal services show ceremonial drift, stratification is present.',
    'Evidence of significant stratification would support the hybrid reading (competence retained in specialized institutions only) over the competence reading''s assumption of universal maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specialized_vs_diffuse_competence_retention, empirical, 'Whether competence retention is uniformly maintained or stratified across institution types.').

omega_variable(
    extraction_via_ceremonial_certification,
    'Does the constraint function to preserve competence, or does it function to certify institutional compliance regardless of actual competence, allowing the institutions to extract legitimacy or budget commitment from that certification?',
    'Examine institutional incentives: do agencies face sanctions (budget cuts, leadership replacement, loss of jurisdiction) when drills fail, or are negative drill results absorbed without corrective action? If sanctions are real and corrective action follows failure, competence is the measured outcome. If failures are documented but unacted-upon, certification is the outcome and competence preservation is secondary.',
    'If certification extraction is the primary function, theater_ratio should be reclassified higher and the constraint might be reframed as a weaker form of snare (institutions extract legitimacy from compliance theater). If competence outcomes drive institutional accountability, the competence reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_via_ceremonial_certification, empirical, 'Whether the constraint''s true function is competence preservation or institutional compliance certification.').

omega_variable(
    kernel_reading_frame_stability,
    'Which reading of the preparedness kernel is structurally stable in contemporary governance? Does the competence reading''s assumption that ceremony and competence remain coupled reflect how modern institutions actually operate, or is it an idealization that the hybrid and husk readings better capture?',
    'Long-term institutional ethnography or comparative case studies across jurisdictions with different emphasis on specialized vs. distributed competence retention. Trace how drill regimes evolve under fiscal pressure, workforce turnover, and technological change.',
    'If the competence reading''s structural assumptions are borne out (ceremony and competence tightly coupled, extraction minimal, resource allocation optimized for skill retention), it is the best reading. If evidence shows drift toward the husk or hybrid readings, those become more accurate descriptions of contemporary preparedness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_frame_stability, conceptual, 'Which reading of the preparedness kernel best captures the structural reality of contemporary disaster preparedness institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(prep_tr_t5, preparedness_retention__competence_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__competence_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(prep_tr_t15, preparedness_retention__competence_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__competence_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(prep_tr_t25, preparedness_retention__competence_reading, theater_ratio, 25, 0.13).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__competence_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__competence_reading, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__competence_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(prep_be_t5, preparedness_retention__competence_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__competence_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement(prep_be_t15, preparedness_retention__competence_reading, base_extractiveness, 15, 0.17).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__competence_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(prep_be_t25, preparedness_retention__competence_reading, base_extractiveness, 25, 0.19).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__competence_reading, base_extractiveness, 30, 0.19).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__competence_reading, base_extractiveness, 40, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__competence_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(prep_su_t5, preparedness_retention__competence_reading, suppression_requirement, 5, 0.06).
narrative_ontology:measurement(prep_su_t10, preparedness_retention__competence_reading, suppression_requirement, 10, 0.07).
narrative_ontology:measurement(prep_su_t15, preparedness_retention__competence_reading, suppression_requirement, 15, 0.08).
narrative_ontology:measurement(prep_su_t20, preparedness_retention__competence_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement(prep_su_t25, preparedness_retention__competence_reading, suppression_requirement, 25, 0.08).
narrative_ontology:measurement(prep_su_t30, preparedness_retention__competence_reading, suppression_requirement, 30, 0.08).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__competence_reading, suppression_requirement, 40, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_retention kernel decomposes into three constraint stories corresponding to three distinct readings of what preparedness IS. The competence_reading (this story) frames preparedness as live exercised knowledge—a genuine coordination solution to skill atrophy in rare-event domains. The husk_reading frames the same arrangement as memorial performance without live competence—ceremony that has lost functional coupling. The hybrid_reading frames preparedness as stratified—competence retained in specialized institutions while broader societal memory becomes ceremonial. Each reading instantiates a different epsilon and different type classification. The competence reading claims low extraction (0.18), rope type, and tight ceremony-competence coupling. The husk reading would claim higher extraction (theater-masquerading-as-competence), piton or snare type, and decoupled ceremony. The hybrid reading claims moderate extraction (specialized competence + diffuse ceremony = hidden asymmetry), tangled rope or piton type. All three readings are live, coexistent positions in contemporary preparedness governance. The relationships are documented in cs_structure.reading_relations: the competence reading coexists_with the husk and hybrid readings (they are not logically foreclosed by each other; different institutions and jurisdictions instantiate different readings simultaneously). The competence reading influences both siblings: as institutional competence degrades toward ceremony, the hybrid and husk readings become more empirically accurate descriptions, but the competence reading's structural framework remains alive as a counterfactual and as the target state institutions attempt to maintain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
