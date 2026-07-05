% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Flood Preparedness Drills as Memorial Ritual (Husk Reading)
 *   domain: disaster_risk_management/institutional_memory
 *
 * SUMMARY:
 *   This story instantiates the husk reading of the preparedness_transmission
 *   kernel: the drills and inspections continue to be performed exactly as
 *   codified, compliance rates remain high, and certification continues to
 *   flow — but the operational knowledge the ritual was supposed to encode
 *   and re-validate has hollowed out. The scripted flood scenarios no longer
 *   track observed hydrology; responders privately recognize the drift but
 *   perform the choreography because deviation would fail the audit, not
 *   because it would fail a real flood. Organizational memory (the checklist,
 *   the calendar, the certificate) persists with high fidelity; operational
 *   competence (the capacity to actually execute a novel evacuation under
 *   conditions the checklist never anticipated) has decayed underneath it.
 *   This is a distinct constraint from the competence_reading (where the
 *   drills are live-exercised and each cycle re-validates real capability)
 *   and the hybrid_reading (where engineering competence remains high but
 *   civilian coordination knowledge specifically has decayed) — each reading
 *   has its own beneficiary structure and its own epsilon and is authored as
 *   a separate file, linked only by kernel identity.
 *
 * KEY AGENTS:
 *   - drill_administration_office: administers the ritual calendar, does not meaningfully profit, cannot easily redesign it
 *   - compliance_certification_bodies: collects certification fees against a stale rubric
 *   - floodplain_residents: bears undetected risk behind a reassuring certificate
 *   - frontline_emergency_responders: knows the script is stale, has no channel to change it
 *   - municipal_insurers: prices risk off the certificate without independent verification
 *   - flood_risk_researchers: documents the gap from outside the benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.58).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.42).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.81).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Flood Preparedness Drills as Memorial Ritual (Husk Reading)").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "disaster_risk_management/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, 'fe9c2b7c-144b-4636-9ffe-ae3aaaab9ef8').
narrative_ontology:cs_kernel_codification('fe9c2b7c-144b-4636-9ffe-ae3aaaab9ef8', formalized).
narrative_ontology:cs_authority_grounding('fe9c2b7c-144b-4636-9ffe-ae3aaaab9ef8', practice).
narrative_ontology:cs_interpretation_layer_present('fe9c2b7c-144b-4636-9ffe-ae3aaaab9ef8').
narrative_ontology:cs_reading_relation('fe9c2b7c-144b-4636-9ffe-ae3aaaab9ef8', preparedness_transmission__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('fe9c2b7c-144b-4636-9ffe-ae3aaaab9ef8', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('fe9c2b7c-144b-4636-9ffe-ae3aaaab9ef8', foundational, ritual_persistence_implies_capability_decay).
narrative_ontology:cs_axiom_status(ritual_persistence_implies_capability_decay, holdable).
narrative_ontology:cs_axiom_grounding('fe9c2b7c-144b-4636-9ffe-ae3aaaab9ef8', ritual_persistence_implies_capability_decay, empirically_contingent).
narrative_ontology:cs_axiom('fe9c2b7c-144b-4636-9ffe-ae3aaaab9ef8', secondary, certification_compliance_is_not_evidence_of_operational_readiness).
narrative_ontology:cs_axiom_status(certification_compliance_is_not_evidence_of_operational_readiness, holdable).
narrative_ontology:cs_axiom_grounding('fe9c2b7c-144b-4636-9ffe-ae3aaaab9ef8', certification_compliance_is_not_evidence_of_operational_readiness, empirically_contingent).
narrative_ontology:cs_reference_frame('fe9c2b7c-144b-4636-9ffe-ae3aaaab9ef8', post_incident_codified_drill_protocol).
narrative_ontology:cs_drift_state('fe9c2b7c-144b-4636-9ffe-ae3aaaab9ef8', contemporary_climate_shifted_hydrology, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fe9c2b7c-144b-4636-9ffe-ae3aaaab9ef8', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, drill_administration_office).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, compliance_certification_bodies).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, floodplain_residents).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, frontline_emergency_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, municipal_insurers).
narrative_ontology:constraint_vindicates(preparedness_transmission__husk_reading, institutional_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Schedules and certifies the annual drill and inspection cycle against a checklist written decades ago. Administers the calendar, issues completion certificates, and reports compliance rates upward. Could redesign the drills around current flood modeling but the cost of retraining inspectors and rewriting the certification rubric is treated as prohibitive against the visible benefit of a clean compliance report. Does not profit from the arrangement beyond continued budget line survival.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, drill_administration_office, agenda_setter,
    institutional, generational, constrained, regional).

% Audits the drill records and issues the certifications that satisfy insurance and regulatory requirements upstream. Certifies against the same pre-specified checklist the drills were designed to satisfy, so a jurisdiction that performs the ritual perfectly passes regardless of whether personnel could actually execute a novel evacuation. Collects fees for certification review.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, compliance_certification_bodies, beneficiary,
    institutional, biographical, constrained, national).

% Live inside the certified flood zone and are told the jurisdiction is drill-compliant and prepared. Have no visibility into whether the drills tested anything resembling the flood patterns actually occurring under changed hydrology. Bear the full consequence if an actual event departs from the scripted scenario the drills rehearse.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, floodplain_residents, payer,
    powerless, biographical, trapped, local).

% Execute the scripted drill each cycle and privately know the choreography no longer matches river behavior, road conditions, or population density in the zone. Have raised concerns in after-action reports that go unread past the compliance summary line. Their operational judgment is not solicited in redesigning the drill; they perform it as scripted because deviation would fail the certification audit.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, frontline_emergency_responders, payer,
    moderate, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__husk_reading, frontline_emergency_responders, excluded).

% Price flood risk partly off the jurisdiction's compliance certificate, treating certified status as a genuine risk-reduction signal. Benefit from a stable, legible compliance metric regardless of whether it tracks real capability, since it lets them write policy without commissioning independent hazard assessment.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, municipal_insurers, beneficiary,
    organized, biographical, mobile, national).

% Study historical drill records against actual flood event outcomes and document the growing gap between scripted scenarios and observed hydrology. Publish findings that circulate in academic and some regulatory channels but do not by themselves trigger a redesign of the certification rubric.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, flood_risk_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: standardize evacuation procedure and inspection of flood defenses so that municipalities, insurers, and residents share a common, verifiable baseline of readiness rather than each actor guessing at capability.
% TRANSFER_FUNCTION: Moves the appearance of preparedness — in the form of certification status — from the drill administration and certifying bodies to insurers and upstream regulators, while moving the residual undetected risk onto residents and the responders who know the script no longer fits.
% ABSENT_VOICES: Frontline responders' after-action reports documenting scenario drift are structurally routed into a summary line that certification bodies do not read in detail; hydrology researchers documenting the widening gap between drilled scenarios and observed flood behavior are cited in academic literature but not integrated into the certification rubric's revision cycle.
% DISAPPEARANCE_RATIONALE: Insurers and regulators who rely on the certificate as a proxy would need to find or build a new readiness signal, which would visibly disrupt pricing and reporting in the short term (world_rearranges from their seat). Floodplain residents' actual safety would arguably be unchanged or even clarified, since the ritual currently masks rather than reduces their exposure to novel flood scenarios (world_unchanged from their seat) — hence contested rather than a single verdict.
% FOUNDING_PROBLEM: Recurring flood events with no standardized municipal evacuation or defense-inspection procedure left responders improvising each time and gave insurers and regulators no way to compare readiness across jurisdictions.
% FOUNDING_PROBLEM_CORROBORATION: Flood risk researchers, publishing independently of the certification bodies, attest that the original problem — standardized, comparable readiness signal — has been formally solved but the underlying operational competence the signal was meant to track has decayed; frontline responders corroborate this from inside the drill cycle. The drill administration office and certification bodies, who benefit from continued certification fees and budget continuity, attest the problem remains live and the drills remain functionally adequate — this is the account of the benefiting parties themselves and should be weighted accordingly.
narrative_ontology:disappearance_verdict(preparedness_transmission__husk_reading, contested).
narrative_ontology:founding_problem_status(preparedness_transmission__husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__husk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_transmission__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__husk_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Theater ratio is authored high and rising (0.22 to 0.81) because this is precisely the diagnostic signature of the husk reading: the performative activity (scheduled drills, issued certificates) persists and even intensifies as a compliance artifact while the functional activity (actually rehearsing capability against current flood risk) shrinks. Extractiveness is moderate and rising (0.30 to 0.58) rather than extreme, because the constraint's primary failure mode is not active predation but institutional inertia riding on a compliance metric nobody has incentive to revise. Suppression is moderate (0.42): there is no active coercion preventing redesign, but there is real friction — responders who flag scenario drift see reports absorbed into unread summaries, which is soft suppression through bureaucratic routing rather than overt coercion. Accessibility collapse is low-moderate (0.35) because the alternative — a redesigned, hydrologically current drill — is not conceptually hidden, just organizationally expensive to enact. Resistance is low (0.28): responders grumble privately but do not organize against the certification regime because their livelihoods depend on passing it.
 *
 * PERSPECTIVAL GAP:
 *   From the drill administration office's seat, the arrangement looks like successful institutional continuity — the calendar runs, the certificates issue, the budget survives. From frontline responders' seat, the same structure is a known-hollow ritual they must perform correctly to keep their jobs and their jurisdiction's insurance rates, despite knowing it does not test what a real event would demand. From floodplain residents' seat, the certificate is read as a genuine safety signal they have no means to independently verify or contest. The engine computing these seats separately is exactly the point: no single metric captures both the administrator's continuity story and the resident's misplaced reliance.
 *
 * DIRECTIONALITY LOGIC:
 *   Drill administration office and compliance certification bodies sit near the beneficiary end: they collect continuity, budget survival, and certification fees from the arrangement without bearing the downside risk if the scripted scenarios fail to match a real flood. Municipal insurers similarly benefit from a stable, legible signal cheaper than independent hazard assessment. Floodplain residents and frontline responders sit near the target end: residents are trapped in the geography and bear the consequence of undetected preparedness failure; responders are trapped by professional dependence on certification continuity even though they possess the operational knowledge that the ritual has hollowed. Flood risk researchers are analytical observers with no stake in either direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as pure extraction (a snare) because there genuinely was, and nominally still is, a coordination function — a standardized readiness signal that once solved a real comparability problem for insurers and regulators. It equally prevents mislabeling it as a healthy rope, because the metric that was supposed to track the coordination function (certification against the drill checklist) has decoupled from the underlying capability it was meant to represent. Piton captures this precisely: no concentrated beneficiary is extracting rents at scale (this is not a snare), the administering office has no clear path to profit from continuing the ritual beyond survival (this is not tangled_rope requiring named victims paying a named beneficiary through active enforcement), and the cost of redesigning the drill exceeds what any single party bears from its current hollow form. The husk reading specifically documents WHERE this piton signature lives on the kernel: memory (the checklist, the schedule) persists with fidelity while competence (the capacity it was built to encode) is absent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_hybrid_extent_of_decay,
    'Has operational knowledge hollowed out broadly across both civilian coordination and physical/engineering competence (husk_reading), or is the decay stratified with engineering competence remaining intact (hybrid_reading)?',
    'Independent post-incident review comparing actual infrastructure performance (levee integrity, pump function, structural response) against actual civilian evacuation coordination during a real flood event that departs from the scripted drill scenario. If infrastructure performs to spec while coordination fails, hybrid_reading is the better empirical fit; if both fail, husk_reading is corroborated.',
    'If the hybrid_reading is empirically correct rather than the husk_reading, this story''s beneficiary/victim structure and extraction trajectory would not apply to the engineering-competence component of preparedness — the classification here is scoped specifically to the broad-decay claim and should not be read as covering infrastructure competence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_hybrid_extent_of_decay, empirical, 'Whether decay is broad (husk) or stratified toward civilian coordination only (hybrid).').

omega_variable(
    certification_signal_naturalization,
    'Is the certification-as-readiness-signal treated as an inevitable proxy (a natural feature of how large bureaucracies must certify readiness) or as a constructed artifact that could be redesigned to track actual capability?',
    'Comparative study of jurisdictions that have redesigned their certification rubric around scenario-adaptive assessment versus those that retained static checklists, measuring whether redesign is organizationally feasible at comparable cost.',
    'If redesign proves organizationally feasible at modest cost elsewhere, the ''prohibitive redesign cost'' premise underlying the piton classification weakens, and the constraint looks more like an unaddressed but fixable coordination failure rather than genuine institutional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_signal_naturalization, conceptual, 'Whether the certification-checklist coupling is inertial necessity or addressable choice.').

omega_variable(
    committer_framing_selection,
    'Given that the same observable pattern (drills continue, compliance is high, some responders report scenario drift) could be read as competence_reading, husk_reading, or hybrid_reading depending on which evidentiary weight is given to responder testimony versus official certification records, what governed the selection of husk_reading as the framing for this file?',
    'This selection was guided by the corroboration asymmetry documented in founding_problem_corroboration: the parties attesting decay (researchers, responders) sit outside the certifying and administering institutions, while the parties attesting continued adequacy are the institutions with a direct stake in the certificate''s continued validity. A framing audit weighting only official compliance records would favor competence_reading; weighting only outside corroboration favors husk_reading or hybrid_reading.',
    'If the corroboration asymmetry is judged insufficient to establish broad decay (e.g., if responder testimony is itself unrepresentative or researchers have selection bias toward publishing failure cases), the classification here overstates decay relative to competence_reading, and the extraction/theater trajectory would need revision downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_selection, conceptual, 'What evidentiary weighting selected this reading over its siblings, and its defeasibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__husk_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(prep_tr_t8, preparedness_transmission__husk_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(prep_tr_t16, preparedness_transmission__husk_reading, theater_ratio, 16, 0.5).
narrative_ontology:measurement(prep_tr_t24, preparedness_transmission__husk_reading, theater_ratio, 24, 0.64).
narrative_ontology:measurement(prep_tr_t32, preparedness_transmission__husk_reading, theater_ratio, 32, 0.74).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__husk_reading, theater_ratio, 40, 0.81).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__husk_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(prep_be_t8, preparedness_transmission__husk_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(prep_be_t16, preparedness_transmission__husk_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(prep_be_t24, preparedness_transmission__husk_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(prep_be_t32, preparedness_transmission__husk_reading, base_extractiveness, 32, 0.53).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__husk_reading, base_extractiveness, 40, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_transmission__husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__husk_reading, 0.1).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'flood preparedness drill and inspection regime' under the preparedness_transmission kernel. competence_reading claims the drills remain live-exercised and capability-validating (low epsilon, rope-like). husk_reading (this file) claims broad decay of operational knowledge beneath persistent ritual form (moderate-rising epsilon, piton). hybrid_reading claims stratified decay limited to civilian coordination while engineering competence remains high (intermediate epsilon, likely tangled_rope or piton depending on which function is evaluated). Each carries its own epsilon and stakeholder structure; they are linked here as kernel siblings, not merged into one constraint with an observable-dependent epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
