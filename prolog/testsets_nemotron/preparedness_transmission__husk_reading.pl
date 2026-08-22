% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__husk_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_transmission__husk_reading
 *   human_readable: Civil Defense Drill Performance as Memorial Ritual
 *   domain: disaster_risk_management/institutional_memory/civil_defense_systems
 *
 * SUMMARY:
 *   National civil defense agencies continue to run quarterly shelter drills,
 *   siren tests, and inspection cycles designed for Cold War threat models.
 *   Compliance rates are near 100% on paper. But after-action reviews from
 *   recent compound disasters (flash floods with simultaneous grid failure,
 *   wildfire-urban interface events with comms blackout) reveal that
 *   responders cannot improvise when the script breaks. The drill regime has
 *   become a memorial ritual: it performs the *form* of preparedness while
 *   the *capability* it once transmitted has hollowed out. The bureaucracy
 *   that administers it extracts legitimacy and budget from the performance;
 *   auditors extract professional validation; contractors extract predictable
 *   procurement. Frontline responders and vulnerable communities pay in lost
 *   adaptive capacity and unmet need. Local emergency managers are
 *   identity-locked — their professional self-concept is constituted through
 *   running the drills.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.68).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.55).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Civil Defense Drill Performance as Memorial Ritual").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "disaster_risk_management/institutional_memory/civil_defense_systems").

domain_priors:requires_active_enforcement(preparedness_transmission__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, '6ef42366-e05a-4faa-9645-ba8caf918a4c').
narrative_ontology:cs_kernel_codification('6ef42366-e05a-4faa-9645-ba8caf918a4c', formalized).
narrative_ontology:cs_authority_grounding('6ef42366-e05a-4faa-9645-ba8caf918a4c', lineage).
narrative_ontology:cs_interpretation_layer_present('6ef42366-e05a-4faa-9645-ba8caf918a4c').
narrative_ontology:cs_reading_relation('6ef42366-e05a-4faa-9645-ba8caf918a4c', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ef42366-e05a-4faa-9645-ba8caf918a4c', preparedness_transmission__hybrid_reading, influences).
narrative_ontology:cs_axiom('6ef42366-e05a-4faa-9645-ba8caf918a4c', foundational, protocol_compliance_equals_preparedness).
narrative_ontology:cs_axiom_status(protocol_compliance_equals_preparedness, holdable).
narrative_ontology:cs_axiom_grounding('6ef42366-e05a-4faa-9645-ba8caf918a4c', protocol_compliance_equals_preparedness, conventional).
narrative_ontology:cs_axiom('6ef42366-e05a-4faa-9645-ba8caf918a4c', foundational, institutional_continuity_requires_ritual_performance).
narrative_ontology:cs_axiom_status(institutional_continuity_requires_ritual_performance, holdable).
narrative_ontology:cs_axiom_grounding('6ef42366-e05a-4faa-9645-ba8caf918a4c', institutional_continuity_requires_ritual_performance, conventional).
narrative_ontology:cs_reference_frame('6ef42366-e05a-4faa-9645-ba8caf918a4c', cold_war_civil_defense_verification).
narrative_ontology:cs_drift_state('6ef42366-e05a-4faa-9645-ba8caf918a4c', compound_hazard_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6ef42366-e05a-4faa-9645-ba8caf918a4c', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, civil_defense_bureaucracy).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, audit_compliance_officers).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, equipment_contractors).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, vulnerable_communities).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, local_emergency_managers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, local_emergency_managers).
narrative_ontology:constraint_vindicates(preparedness_transmission__husk_reading, preparedness_is_demonstrated_by_protocol_compliance).
narrative_ontology:constraint_vindicates(preparedness_transmission__husk_reading, institutional_continuity_requires_ritual_performance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the national drill calendar, inspection protocols, and compliance reporting. Funded by legislative appropriations tied to participation metrics. Maintains the forms because they produce legible output for oversight bodies. Can reassign personnel, revise manuals, or redirect budgets but rarely does — the ritual sustains the bureau's mandate.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, civil_defense_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Evaluate readiness through checklist completion rates and paperwork fidelity. Their professional standing derives from producing clean audits; they have no mandate to test adaptive capacity. A shift to capability-based assessment would invalidate their method and metrics.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, audit_compliance_officers, beneficiary,
    organized, biographical, mobile, national).

% Supply sirens, shelters, communications gear, and PPE to specification. Procurement cycles are triggered by inspection findings that flag equipment age or missing items — not by operational gaps. They benefit from predictable, form-driven demand.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, equipment_contractors, beneficiary,
    powerful, biographical, arbitrage, national).

% Required to participate in quarterly drills that rehearse scripted scenarios. Know the drills do not reflect the compound hazards they actually face (flash flood + power loss + comms failure). Speaking up risks insubordination marks; leaving means abandoning pension and community ties. They carry the cost of performative compliance in lost training time.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, frontline_responders, payer,
    moderate, biographical, constrained, regional).

% Depend on civil defense systems that pass inspections but fail under novel events. No voice in drill design; no exit from the hazard zone. Bear the extraction as unmet need when the ritualized system collapses in a real compound disaster. Their situation is invisible to the compliance metrics.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, vulnerable_communities, payer,
    powerless, immediate, trapped, local).

% Caught between bureau directives and community reality. Held accountable for drill compliance scores; judged by the public on actual response. Their professional identity fuses with the ritual — 'running the drill' is what the job *is*. They cannot advocate for capability-based reform without threatening their own legitimacy. The identity lock is professional: the role has become the performance.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, local_emergency_managers, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__husk_reading, local_emergency_managers, beneficiary).

% Study the gap between exercised protocols and adaptive capacity in compound events. Document the hollowing of operational knowledge. No institutional power to change the system; their work is cited in after-action reports that are filed and forgotten.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, disaster_sociologists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes multi-agency mobilization sequences, equipment checks, and public alert pathways on a fixed calendar so that some baseline readiness is maintained without continuous negotiation.
% TRANSFER_FUNCTION: Moves staff-hours, budget authority, and legitimacy credit from frontline responders and local managers to the central bureaucracy and its audit/compliance ecosystem. The extraction is the diversion of adaptive training capacity into ritual compliance.
% ABSENT_VOICES: Vulnerable communities (trapped, no platform), frontline responders who have left the service (their exit is silent), and would-be reformers in local emergency management who were marginalized for questioning drill realism.
% DISAPPEARANCE_RATIONALE: If the drill regime vanished overnight, the bureaucracy would lose its primary legibility mechanism and appropriations anchor. Contractors would lose predictable procurement. Frontline responders would reclaim training time for scenario-based exercises. Vulnerable communities would lose the *illusion* of protection but gain honesty about actual gaps. The system would reorganize around either capability-based assessment or naked neglect.
% FOUNDING_PROBLEM: Post-war civil defense needed a scalable, auditable way to ensure every jurisdiction maintained baseline shelter, warning, and mobilization capacity against a known threat model (nuclear attack, riverine flood). The drill calendar solved the coordination problem of verifying readiness across thousands of localities without deploying inspectors everywhere.
% FOUNDING_PROBLEM_CORROBORATION: Cold War civil defense historians and retired emergency managers outside the current bureaucracy attest the original threat model (mass nuclear warning, uniform sheltering) is obsolete. The bureaucracy's own strategic documents (unclassified) acknowledge the shift to compound, novel hazards but maintain the drill framework because 'it is what we can measure.' No independent attestation supports the claim that the founding problem remains live.
narrative_ontology:disappearance_verdict(preparedness_transmission__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(preparedness_transmission__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__husk_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the ritual diverts substantial staff time and budget from capability-building into compliance theater. The extraction is not zero-sum theft — it is the opportunity cost of adaptive training replaced by scripted repetition. Theater ratio (0.72) is the defining feature: most drill activity is performative maintenance of the bureau's legibility to oversight. Suppression (0.55) is moderate — the constraint persists through professional identity lock and budget dependency, not overt coercion. Accessibility collapse (0.38) is low because alternatives (scenario-based training, capability audits) exist and are known; they are simply not adopted because they don't produce the compliance metrics the bureau needs. Resistance (0.32) is low because the identity-locked local managers and trapped communities cannot effectively resist; the bureaucracy faces no organized opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the bureaucracy's seat, the constraint is a scaffold (transitional, sunset-able) or even a rope (genuine coordination of baseline readiness). From the frontline responder and vulnerable community seats, it is a snare (extraction without coordination benefit). From the local emergency manager seat, it is a piton (atrophied function maintained by identity-locked performance). The engine computes this divergence from the structural data — the claimed_type 'piton' reflects the *system-level* reality: the primary coordination function has atrophied, the constraint persists by institutional inertia and theatrical maintenance, and no concentrated beneficiary captures enough to maintain it (the bureaucracy *could* change it but the cost to fix exceeds what it bears).
 *
 * DIRECTIONALITY LOGIC:
 *   The civil defense bureaucracy is the structural beneficiary (d ~ 0.15): it collects budget authority, legislative legitimacy, and organizational survival from the ritual. Audit officers and contractors are secondary beneficiaries (d ~ 0.2-0.3). Frontline responders are primary targets (d ~ 0.85): constrained exit, bear the time cost, know the fiction. Vulnerable communities are full targets (d ~ 0.95): trapped, no voice, bear the consequence when the theater fails. Local emergency managers are identity-locked targets (d ~ 0.8): their professional identity fuses with the ritual, making exit cognitively unavailable even though formal exit exists. Disaster sociologists are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (verifiable baseline readiness against a known, uniform threat model) is dead. The threat model shifted to compound, novel hazards; the drill regime did not adapt. The arrangement persists because the bureaucracy's mandate, the auditors' method, the contractors' revenue, and the local managers' professional identity all depend on the ritual continuing. Mandatrophy is resolved: the mandate has outlived its function, but the constraint remains because the cost of fixing (redesigning assessment, retraining auditors, breaking contractor cycles, shattering professional identity) exceeds what any single seat bears. The theater_ratio trajectory confirms the drift: extraction accumulates as the coordination function decays.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_competence_boundary,
    'Is the drill regime a hollow ritual (husk_reading) or does it still transmit latent capability that activates under stress (competence_reading)?',
    'Controlled comparison of response effectiveness in novel compound events between jurisdictions with high drill compliance but low adaptive training vs. jurisdictions that shifted to scenario-based exercises. Measure improvisation success rate, time-to-effective-action, and casualty reduction.',
    'If competence_reading is validated, the constraint reclassifies toward rope/scaffold (genuine coordination function persists). If husk_reading is validated, piton/snare classification holds and the ritual is extractive theater. The hybrid_reading predicts a split: engineering drills (competence) vs. civilian coordination drills (husk).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(husk_vs_competence_boundary, empirical, 'Whether operational knowledge is truly hollowed out or latent.').

omega_variable(
    identity_lock_mechanism,
    'Is the local emergency manager''s identity lock professional (role fusion), ideological (belief in the ritual''s necessity), or institutional (career path dependence)?',
    'Longitudinal interviews with managers who attempted reform and were marginalized vs. those who exited. Trace whether the lock breaks when formal incentives change (e.g., new capability-based promotion criteria).',
    'If professional/ideological, the lock persists even if bureaucracy reforms — the constraint becomes self-sustaining. If institutional, reforming incentives could unlock the seat and enable change from within.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Type of identity fusion binding local managers to the ritual.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression experienced by frontline responders and vulnerable communities structural (budget rules, regulatory mandates) or internalized (they believe the drills are the best possible preparation)?',
    'Post-exit interviews with responders who left the service: does the sense of futility persist? Community surveys in vulnerable areas: do residents trust the sirens/shelters or see them as theater?',
    'If internalized, effective suppression is higher than structural measure — the target carries the suppression after formal exit. If structural, suppression drops when mandates change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism of suppression in the interpersonal/institutional constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1990, preparedness_transmission__husk_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(prep_tr_t1995, preparedness_transmission__husk_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(prep_tr_t2000, preparedness_transmission__husk_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(prep_tr_t2005, preparedness_transmission__husk_reading, theater_ratio, 2005, 0.45).
narrative_ontology:measurement(prep_tr_t2010, preparedness_transmission__husk_reading, theater_ratio, 2010, 0.52).
narrative_ontology:measurement(prep_tr_t2015, preparedness_transmission__husk_reading, theater_ratio, 2015, 0.6).
narrative_ontology:measurement(prep_tr_t2020, preparedness_transmission__husk_reading, theater_ratio, 2020, 0.68).
narrative_ontology:measurement(prep_tr_t2025, preparedness_transmission__husk_reading, theater_ratio, 2025, 0.72).

% Extraction over time
narrative_ontology:measurement(prep_be_t1990, preparedness_transmission__husk_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(prep_be_t1995, preparedness_transmission__husk_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(prep_be_t2000, preparedness_transmission__husk_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(prep_be_t2005, preparedness_transmission__husk_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(prep_be_t2010, preparedness_transmission__husk_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(prep_be_t2015, preparedness_transmission__husk_reading, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement(prep_be_t2020, preparedness_transmission__husk_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(prep_be_t2025, preparedness_transmission__husk_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1990, preparedness_transmission__husk_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(prep_su_t1995, preparedness_transmission__husk_reading, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement(prep_su_t2000, preparedness_transmission__husk_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(prep_su_t2005, preparedness_transmission__husk_reading, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(prep_su_t2010, preparedness_transmission__husk_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(prep_su_t2015, preparedness_transmission__husk_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(prep_su_t2020, preparedness_transmission__husk_reading, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement(prep_su_t2025, preparedness_transmission__husk_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__hybrid_reading).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, disaster_funding_allocation_formula).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, emergency_management_accreditation_standards).

% DUAL FORMULATION NOTE:
% The preparedness_transmission kernel decomposes into three readings with distinct ε and structural profiles. competence_reading: low extraction (ε≈0.2), genuine coordination, claimed rope. hybrid_reading: stratified extraction (engineering ε≈0.15, civilian ε≈0.65), claimed tangled_rope. husk_reading (this story): high extraction (ε=0.68), high theater, claimed piton. The readings compete for the same institutional space — the bureaucracy's drill calendar cannot simultaneously be a live capability validator, a stratified system, and a hollow ritual.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_transmission__husk_reading, moderate, 0.8).
constraint_indexing:directionality_override(preparedness_transmission__husk_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
