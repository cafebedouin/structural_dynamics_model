% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__competence_reading, []).

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
 *   constraint_id: preparedness_persistence__competence_reading
 *   human_readable: Preparedness Persistence via Live Drill Practice (Competence Reading)
 *   domain: disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   Under the competence reading, disaster preparedness — drills,
 *   inspections, equipment maintenance, scenario training — constitutes live
 *   exercise of knowledge essential to operational readiness. This reading
 *   asserts that repeated practice embeds competence in personnel,
 *   procedures, and organizational muscle memory. When a prepared-for hazard
 *   occurs, the trained response activates because readiness was maintained
 *   through continuous live exercise. The constraint is a Rope: genuine
 *   coordination problem (how to maintain readiness across long periods
 *   without actual events to test it), solved through scheduled practice, net
 *   benefit to those who participate (population, responders, institutional
 *   leadership all benefit from reduced disaster impact). Minimal extraction,
 *   no active suppression, low theater — the arrangement is what it claims to
 *   be. This reading contests the husk_reading (drills as memorial
 *   performance masking atrophy) and the hybrid_reading (stratification: some
 *   competence live, others ritualized). The competence reading's core
 *   commitment is that practice transmits competence.
 *
 * KEY AGENTS:
 *   - population_at_risk: Beneficiary of reduced disaster impact from maintained readiness; participates in some drills (evacuation scenarios); identity somewhat locked by residence.
 *   - emergency_responders: Beneficiary of maintained procedural readiness; primary agents of live exercise (regular training, equipment checks, scenario practice); identity locked by profession.
 *   - institutional_leadership: Agenda-setter for preparedness cadence and curriculum; benefits from institutional legitimacy of disaster preparedness; bears opportunity cost of resource allocation to drills versus other priorities.
 *   - regulatory/standards bodies: Observer/weak agenda-setter; establish minimum preparedness standards; provide external accountability that helps sustain the commitment when in-cycle disasters are rare.
 *   - vulnerable populations: Beneficiary class but with differential access to preparedness information; less frequently involved in drills; exit options constrained by geography.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__competence_reading, 0.08).
domain_priors:suppression_score(preparedness_persistence__competence_reading, 0.12).
domain_priors:theater_ratio(preparedness_persistence__competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, 0.06).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__competence_reading, rope).
narrative_ontology:human_readable(preparedness_persistence__competence_reading, "Preparedness Persistence via Live Drill Practice (Competence Reading)").
narrative_ontology:topic_domain(preparedness_persistence__competence_reading, "disaster_preparedness/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__competence_reading, '2ce1570c-19ba-4e09-b2d3-102811d90b27').
narrative_ontology:cs_kernel_codification('2ce1570c-19ba-4e09-b2d3-102811d90b27', implicit).
narrative_ontology:cs_authority_grounding('2ce1570c-19ba-4e09-b2d3-102811d90b27', practice).
narrative_ontology:cs_interpretation_layer_present('2ce1570c-19ba-4e09-b2d3-102811d90b27').
narrative_ontology:cs_reading_relation('2ce1570c-19ba-4e09-b2d3-102811d90b27', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ce1570c-19ba-4e09-b2d3-102811d90b27', preparedness_persistence__hybrid_reading, influences).
narrative_ontology:cs_axiom('2ce1570c-19ba-4e09-b2d3-102811d90b27', foundational, practice_transmits_competence).
narrative_ontology:cs_axiom_status(practice_transmits_competence, holdable).
narrative_ontology:cs_axiom_grounding('2ce1570c-19ba-4e09-b2d3-102811d90b27', practice_transmits_competence, empirically_contingent).
narrative_ontology:cs_axiom('2ce1570c-19ba-4e09-b2d3-102811d90b27', secondary, preparedness_maintenance_requires_continuous_practice).
narrative_ontology:cs_axiom_status(preparedness_maintenance_requires_continuous_practice, holdable).
narrative_ontology:cs_axiom_grounding('2ce1570c-19ba-4e09-b2d3-102811d90b27', preparedness_maintenance_requires_continuous_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('2ce1570c-19ba-4e09-b2d3-102811d90b27', competence_through_live_practice).
narrative_ontology:cs_drift_state('2ce1570c-19ba-4e09-b2d3-102811d90b27', contemporary_post_major_disaster_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('2ce1570c-19ba-4e09-b2d3-102811d90b27', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, population_at_risk).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, emergency_responders).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, institutional_leadership).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__competence_reading_tests).
:- end_tests(preparedness_persistence__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.08) because no party systematically collects rents or uncompensated transfers from the arrangement; the coordination benefit (maintained readiness) accrues diffusely to all participants. Suppression is minimal (0.12) because the constraint's persistence does not depend on preventing exits or alternatives — jurisdictions can choose to de-emphasize preparedness, though few do once a major disaster has occurred. Theater ratio is modest (0.18) because some preparedness activity is ceremonial (annual parade of equipment, budget-cycle demonstrations) but the majority is functionally directed toward maintaining actual competence — drills are repetitive and often unglamorous, which suggests they serve the competence function rather than publicity. Accessibility collapse is high (0.88) because once a disaster occurs, the alternatives to having maintained readiness vanish completely — the collapse is not suppression but physical reality: a hurricane arrives and options resolve to 'we trained for this' or 'we did not.' Resistance is negligible (0.06) because there is no organized opposition to preparedness itself, only occasional friction around specific drill scenarios or resource allocation. The measurement series show extraordinary stability: extractiveness and theater slightly increase as institutional bureaucracy accumulates around preparedness, but within the Rope range. Theater does not rise sharply, which is consistent with the competence reading — if the constraint were degrading into pure performance (husk reading), we would expect theater_ratio to rise toward 0.5+ and extractiveness to remain flat or decline. Neither happens here.
 *
 * PERSPECTIVAL GAP:
 *   From the population's and emergency responders' seats, preparedness drills are straightforward collective action: we all benefit from being ready, we all participate in maintaining readiness through drills, no party extracts and no one is locked in against their will (residence and profession are not preparedness-created). From institutional leadership's seat, drills are an allocation choice: time and budget devoted to preparedness cycles come at the cost of other projects, and the benefit (disaster avoided) is often invisible or attributed to luck rather than preparation. This perspectival gap should compute as low divergence in seat type — both payer and beneficiary are Rope seats — because the asymmetry is one of visibility, not structural extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (population, responders, leadership) have low d: they benefit from maintained readiness and bear the coordination cost (training time, attention, resource allocation) proportionally. The beneficiary role is structural, not earned — you benefit from preparedness by occupying a location or profession that makes you vulnerable to a disaster. No seat is trapped or identity-locked into paying extraction; identity-locked exits (responders staying in the profession) carry no extraction premium — responders choose the profession knowing its preparedness demands. The arrangement does not suppress alternatives or exit; it simply coordinates a genuine collective action problem. Directionality overrides are not needed because the structural derivation from beneficiary declarations + moderate/powerful power levels + mobile-to-constrained exit options yields correct d values across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to maintain operational readiness for low-frequency, high-impact events — remains live. The competence reading claims the founding problem is continuously re-solved through the constraint's operation: each drill re-certifies that personnel remember procedures, equipment is functional, and organizational memory is intact. If the founding problem were dead (we no longer face hazards requiring preparedness) or the constraint had become decoupled from solving it (drills no longer transmit competence), mandatrophy would be in play. The measurement data show theater_ratio staying below 0.25, which suggests the constraint has not yet degraded into Piton-like performance. The competence reading explicitly rejects the mandatrophy condition: the founding problem is live because hazards persist and competence decay is a real cost if drills stop.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_competence_vs_performance,
    'Is preparedness maintained through live exercise of competence (knowledge embedded in repeated practice), or through memorial performance that mimics the form of readiness while competence atrophies?',
    'Post-event assessment: when a prepared-for disaster occurs, do the practiced procedures activate the learned competencies or expose decay? Compare pre-event drill performance (formal pass/fail metrics) against actual-event execution speed, decision quality, and improvisation success.',
    'If competence is live, drills are coordination costs and the constraint remains Rope. If performance masks atrophy, drills are theater and the constraint migrates toward Piton. The other readings (husk_reading, hybrid_reading) capture alternative framings of this core ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_competence_vs_performance, empirical, 'Whether drill participation transmits and maintains real competence or performative form without substance.').

omega_variable(
    competence_decay_detection,
    'What signals distinguish live competence from degraded performance? How long can drills sustain readiness without actual operational pressure?',
    'Longitudinal study of drill-participation effects on actual response performance; comparison of jurisdictions with different drill cadences; post-event debriefs documenting whether personnel reverted to trained procedures or abandoned them under stress.',
    'If signals show competence can be maintained indefinitely through regular drills, the Rope classification holds across time horizons. If competence decay accelerates over intervals between major events, the constraint requires higher maintenance frequency and the Rope-to-Piton drift risk rises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_decay_detection, empirical, 'Temporal dynamics of competence decay in preparedness systems.').

omega_variable(
    institutional_identity_lock_in_preparedness,
    'To what degree does institutional self-concept depend on demonstrable preparedness? Is exit from the preparedness commitment available to organizations that choose to abandon it?',
    'Examine jurisdictions/organizations that have dissolved preparedness programs: what was the cost of exit (legal, reputational, operational) versus the cost of continued maintenance? Are there exit-available actors who choose to leave?',
    'If exit is structurally available (a municipality can decide disaster preparedness is not its role), participants are identity_locked only by choice, and the constraint remains Rope-like. If exit is impossible (national framework mandates preparedness), identity fusion is structural and the constraint may function more like internalized Tangled Rope than pure Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_identity_lock_in_preparedness, empirical, 'Whether preparedness lock-in is structural mandate or chosen identity commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__competence_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__competence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(prep_tr_t5, preparedness_persistence__competence_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(prep_tr_t10, preparedness_persistence__competence_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(prep_tr_t15, preparedness_persistence__competence_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__competence_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__competence_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(prep_be_t5, preparedness_persistence__competence_reading, base_extractiveness, 5, 0.07).
narrative_ontology:measurement(prep_be_t10, preparedness_persistence__competence_reading, base_extractiveness, 10, 0.08).
narrative_ontology:measurement(prep_be_t15, preparedness_persistence__competence_reading, base_extractiveness, 15, 0.08).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__competence_reading, base_extractiveness, 20, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(prep_su_t5, preparedness_persistence__competence_reading, suppression_requirement, 5, 0.11).
narrative_ontology:measurement(prep_su_t10, preparedness_persistence__competence_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(prep_su_t15, preparedness_persistence__competence_reading, suppression_requirement, 15, 0.12).
narrative_ontology:measurement(prep_su_t20, preparedness_persistence__competence_reading, suppression_requirement, 20, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__competence_reading, 0.08).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_persistence kernel has three constraint stories corresponding to three readings of what drills and inspections do. This story (competence_reading) instantiates the reading where practice transmits and maintains real competence; the other readings claim different functional stories (memorial performance, or stratified competence). All three stories have the same named phenomenon (drills, inspections, preparedness programs) but different ε values and structural interpretations because they answer the foundational question differently: is competence live or performance? Readers should examine all three stories and their omegas to understand the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
