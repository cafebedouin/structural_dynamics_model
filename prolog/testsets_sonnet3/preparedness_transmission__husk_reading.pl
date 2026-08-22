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
 *   A regional civil defense system has continuously performed flood drills
 *   and inspections for decades, maintaining unbroken organizational memory
 *   of the drill schedule, checklist, and certification process. But the
 *   drilled scenarios were fixed early and never substantially revised
 *   against evolving flood dynamics (compound levee overtop plus road-network
 *   isolation events now common under intensified rainfall). The husk reading
 *   holds that the ritual form persists while the operational substance —
 *   actual capacity to respond to a novel flood scenario — has hollowed out
 *   beneath it. Compliance with protocol form stays high; adaptive capacity
 *   under novel scenarios is low; inspection routines detect only the
 *   pre-specified failure modes they were built to detect and are
 *   structurally blind to anything else. This is one of three readings of the
 *   preparedness_transmission kernel: the competence_reading holds the drills
 *   remain live exercised knowledge; the hybrid_reading holds engineering
 *   competence remains high while civilian coordination knowledge has
 *   decayed. This story instantiates only the husk_reading — a single, stable
 *   ε assessed by this reading's own lights, describing the standing
 *   drill-and-inspection arrangement as it is, not as a redesigned
 *   scenario-realistic regime would look.
 *
 * KEY AGENTS:
 *   - civil_defense_agency: agenda_setter (institutional/constrained) — administers drill calendar, could redesign but redesign cost exceeds discretionary capacity
 *   - regional_inspectorate: beneficiary/agenda_setter (institutional/constrained) — certifies against its own checklist, budget justified by pass rates not demonstrated capability
 *   - frontline_evacuation_wardens: payer (moderate/trapped) — drilled on scripted scenario, bear improvisation cost when real event diverges
 *   - floodplain_residents: payer (powerless/trapped) — rely on certification they cannot independently verify, no exit short of relocation
 *   - emergency_management_researchers: excluded (analytical/analytical) — document the gap, findings cited but rarely change curriculum
 *   - municipal_budget_office: observer (institutional/analytical) — renews funding on certification alone, no independent capability test
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.58).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.44).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.81).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Flood Preparedness Drills as Memorial Ritual (Husk Reading)").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "disaster_risk_management/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, '212cf911-329e-449a-8d86-8f575330cc5a').
narrative_ontology:cs_kernel_codification('212cf911-329e-449a-8d86-8f575330cc5a', formalized).
narrative_ontology:cs_authority_grounding('212cf911-329e-449a-8d86-8f575330cc5a', practice).
narrative_ontology:cs_interpretation_layer_present('212cf911-329e-449a-8d86-8f575330cc5a').
narrative_ontology:cs_reading_relation('212cf911-329e-449a-8d86-8f575330cc5a', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('212cf911-329e-449a-8d86-8f575330cc5a', preparedness_transmission__hybrid_reading, influences).
narrative_ontology:cs_axiom('212cf911-329e-449a-8d86-8f575330cc5a', foundational, protocol_compliance_is_not_operational_capability).
narrative_ontology:cs_axiom_status(protocol_compliance_is_not_operational_capability, holdable).
narrative_ontology:cs_axiom_grounding('212cf911-329e-449a-8d86-8f575330cc5a', protocol_compliance_is_not_operational_capability, empirically_contingent).
narrative_ontology:cs_axiom('212cf911-329e-449a-8d86-8f575330cc5a', secondary, unrevised_checklists_structurally_cannot_detect_novel_failure_modes).
narrative_ontology:cs_axiom_status(unrevised_checklists_structurally_cannot_detect_novel_failure_modes, holdable).
narrative_ontology:cs_axiom_grounding('212cf911-329e-449a-8d86-8f575330cc5a', unrevised_checklists_structurally_cannot_detect_novel_failure_modes, empirically_contingent).
narrative_ontology:cs_reference_frame('212cf911-329e-449a-8d86-8f575330cc5a', post_founding_drill_curriculum).
narrative_ontology:cs_drift_state('212cf911-329e-449a-8d86-8f575330cc5a', contemporary_intensified_rainfall_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('212cf911-329e-449a-8d86-8f575330cc5a', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, floodplain_residents).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, frontline_evacuation_wardens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, regional_inspectorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the annual drill calendar and inspection checklist inherited from a prior flood-response reorganization. Could redesign the drills to test novel scenarios but the cost of rebuilding scenario libraries, retraining inspectors, and revalidating protocols against current climate data exceeds the agency's discretionary budget and political appetite; it is easier and cheaper to keep running the existing checklist and certify pass rates.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, civil_defense_agency, agenda_setter,
    institutional, generational, constrained, regional).

% Practice the same scripted evacuation route and radio-check sequence every year, passing inspection each time, but have no rehearsed procedure for the compound failure modes now common under intensified rainfall (simultaneous levee overtop plus road-network isolation). When an actual event diverges from the drilled scenario, they bear the cost of the gap in improvised, high-stakes conditions.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, frontline_evacuation_wardens, payer,
    moderate, immediate, trapped, local).

% Rely on the evacuation system being competent because the agency's certification says it is current and inspected. They cannot independently verify whether the drills tested anything relevant to the flood pattern that will actually threaten them; their exposure is set by whatever gap exists between the drilled scenario and the real one, and they have no way to exit the jurisdiction's protection system short of relocating.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, floodplain_residents, payer,
    powerless, biographical, trapped, local).

% Certifies drills against a checklist it also authored, so passing rates stay high without anyone measuring whether the checklist still maps to current flood risk. Its continued existence and budget line are justified by producing compliance certifications, not by demonstrated adaptive capacity, so it has a structural incentive to keep grading against the familiar rubric.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, regional_inspectorate, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__husk_reading, regional_inspectorate, agenda_setter).

% Publish post-event analyses showing repeated drills failed to prepare responders for scenarios that combined known individual hazards in new ways. Their findings are cited in after-action reports but rarely change the drill design cycle, since no single actor is positioned or funded to redesign the curriculum from scratch.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, emergency_management_researchers, excluded,
    analytical, civilizational, analytical, national).

% Approves the preparedness line item each cycle based on the inspectorate's pass-rate report, without an independent capability test. It has no mechanism to distinguish a genuinely capable system from one performing well-rehearsed theater, and treats a clean certification as sufficient justification to renew funding at the same level.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, municipal_budget_office, observer,
    institutional, biographical, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The drill-and-inspection cycle originally solved a real coordination problem: synchronizing wardens, agencies, and residents on a shared evacuation script so that, during an actual flood, everyone would know their role without needing real-time improvisation.
% TRANSFER_FUNCTION: The arrangement moves confidence and certification legitimacy from the inspectorate and agency to residents and budget authorities, in exchange for residents' and wardens' unexamined trust that the certified competence corresponds to actual capability under the flood conditions they will face.
% ABSENT_VOICES: Emergency management researchers who have documented the capability gap are cited in reports but structurally outside the certification loop; residents who would demand scenario-realistic testing have no standing in the inspection process at all.
% DISAPPEARANCE_RATIONALE: If the drills and inspections vanished entirely, the agency's own view is that chaos would follow — no scripted coordination at all. The researchers' view is that little would change in an actual novel-flood event, since the existing drills do not test for the failure modes that matter; the appearance of readiness would simply become visible as absence rather than remaining hidden as false confidence.
% FOUNDING_PROBLEM: Early flood-response coordination failures — wardens and residents acting on inconsistent information during evacuations — created the need for a standardized, inspected drill regime that everyone could rely on to behave the same way under stress.
% FOUNDING_PROBLEM_CORROBORATION: The civil defense agency and regional inspectorate attest the founding problem remains live and the drills remain necessary. Emergency management researchers, publishing independently of both bodies, attest that the specific coordination failure the drills were built to solve (basic route/script consistency) is largely solved, while a different and undrilled problem (compound novel-scenario coordination) has emerged and gone unaddressed — corroboration from outside the certifying parties supports the husk reading.
narrative_ontology:disappearance_verdict(preparedness_transmission__husk_reading, contested).
narrative_ontology:founding_problem_status(preparedness_transmission__husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Theater ratio is authored high and rising (0.42 to 0.81) because an increasing share of drill and inspection activity produces certification artifacts (pass rates, compliance reports) rather than tested capability against current flood risk — the classic piton signature of performative maintenance replacing function. Extractiveness is moderate and rising (0.28 to 0.58): the arrangement extracts confidence and funding continuity from residents and budget authorities without a corresponding rise in actual protective capacity, but this is diffuse rather than concentrated — no single actor is capturing rents in the way a snare would show. Suppression is moderate (0.44): there is no active coercive enforcement suppressing alternative drill designs, but institutional inertia and the sunk cost of the existing checklist function as a soft barrier to revision. Accessibility collapse is fairly high (0.62) because once the certification apparatus is understood, the alternative (a rebuilt, scenario-realistic drill regime) requires resources and coordination that no single stakeholder controls alone. Resistance is low (0.35): researchers document the gap but there is no organized push to force redesign, since none of the seats with power to fix it bears the cost of the gap in a way that motivates urgent correction.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting seats (agency, inspectorate) experience the arrangement as functioning coordination — drills happen, checklists are met, certifications issue on schedule. The payer seats (wardens, residents) experience the same structure as an unverifiable promise whose actual protective value they cannot assess until a flood arrives that tests something the drills never tested. The engine should compute this divergence from the structural data: agenda-setters have constrained but non-trapped exit and institutional time horizons that let them treat the annual cycle as adequate; payers have trapped exit and immediate/biographical horizons that make the gap existentially significant in a way the agenda-setters' seat does not register.
 *
 * DIRECTIONALITY LOGIC:
 *   The regional inspectorate is the nearest thing to a beneficiary — it derives budget justification and institutional continuity from producing clean certifications, without needing the certifications to track real capability. It is not a concentrated extractive beneficiary in the snare sense; it does not profit from residents' exposure so much as it is insulated from the consequences of the gap. Floodplain residents and frontline wardens are the targets: they bear the cost of the capability gap directly and cannot exit the jurisdiction's protection system without physically relocating. The civil defense agency sits as agenda_setter with the power to redesign the drills but with the cost of doing so falling on it directly while the benefit of doing so is diffuse and delayed (residents' safety in a future, not-yet-occurred flood) — this is exactly the piton asymmetry: the administrator could change it, but the cost to fix exceeds what the administrator itself bears from leaving it unchanged.
 *
 * MANDATROPHY ANALYSIS:
 *   The piton classification prevents two mislabeling errors. First, it prevents reading the constraint as a pure rope (coordination working as intended) simply because compliance rates are high and no one is being coerced — the founding_problem answers show the ORIGINAL coordination problem (script consistency) is largely solved while a NEW, undrilled problem (compound novel scenarios) has emerged unaddressed, which is a founding_problem_status of 'contested' precisely because the arrangement's justification has drifted from its original function. Second, it prevents reading the constraint as a snare, because there is no concentrated beneficiary profiting from residents' exposure — the inspectorate's institutional self-interest in continuity is diffuse and self-protective, not extractive rent-collection. What remains is inertial: an arrangement no one is captured by and no one is quite responsible for fixing, which is the piton signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_competence_referent_ambiguity,
    'Is the observed pattern (high compliance, low novel-scenario adaptability) a genuine hollowing-out of operational knowledge, or is it the ordinary and defensible limit of any drill regime, which cannot rehearse every possible future scenario and should not be judged a husk merely for having a scope boundary?',
    'Compare post-event after-action performance against drilled-scenario baseline across multiple actual flood events: if performance systematically degrades specifically on the axes documented as novel (compound failure modes) while holding on drilled axes, the husk reading is corroborated; if performance holds across both, the competence reading is better supported.',
    'If the husk reading is wrong, this constraint is closer to a rope with a bounded and reasonable scope, not a piton with hollowed function — the classification would shift and the extractiveness/theater trajectory should be read as stable rather than as evidence of decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_competence_referent_ambiguity, empirical, 'Whether the documented gap constitutes genuine hollowing or a defensible scope limit — the central contest between the husk and competence readings.').

omega_variable(
    kernel_disagreement_locus,
    'Where exactly do the husk, competence, and hybrid readings of preparedness_transmission disagree — is it about whether ANY operational knowledge has decayed, or specifically about WHICH SUBSYSTEM (civilian coordination vs. physical infrastructure) has decayed?',
    'Domain-disaggregated capability audits: separately assess engineering/infrastructure maintenance competence versus civilian evacuation coordination competence, since the hybrid reading''s claim is precisely that these two decay at different rates.',
    'If infrastructure competence is independently verified as high while coordination competence is low, the hybrid reading is the more structurally accurate account and this husk reading (which treats the whole system as hollowed) overstates the scope of decay; if both are found low, the husk reading is vindicated as the more general account.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_disagreement_locus, conceptual, 'The husk, competence, and hybrid readings of the preparedness_transmission kernel disagree about whether decay is total or stratified by subsystem; this omega documents that the disagreement is located in subsystem-level decomposition, not in the top-level compliance metrics all three readings would observe identically.').

omega_variable(
    diffuse_gain_attribution,
    'Is the gain_flow genuinely diffuse (no seat captures the value of the compliance theater), or does the regional inspectorate''s budget-continuity benefit rise to the level of a concentrated capture that would make this a snare rather than a piton?',
    'Track whether inspectorate budget or headcount is more sensitive to certification pass-rate than to any independent capability measure; a strong sensitivity to pass-rate alone, decoupled from capability, would indicate a more concentrated captured benefit than the diffuse reading assumes.',
    'If the inspectorate''s institutional self-interest is shown to be a strong, budget-linked driver of checklist rigidity, the classification should move from piton toward tangled_rope or snare, since a concentrated beneficiary would then exist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_gain_attribution, empirical, 'Tests whether the inspectorate''s self-preservation interest constitutes diffuse institutional inertia or a concentrated captured benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__husk_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(prep_tr_t4, preparedness_transmission__husk_reading, theater_ratio, 4, 0.51).
narrative_ontology:measurement(prep_tr_t8, preparedness_transmission__husk_reading, theater_ratio, 8, 0.6).
narrative_ontology:measurement(prep_tr_t12, preparedness_transmission__husk_reading, theater_ratio, 12, 0.68).
narrative_ontology:measurement(prep_tr_t16, preparedness_transmission__husk_reading, theater_ratio, 16, 0.74).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__husk_reading, theater_ratio, 20, 0.78).
narrative_ontology:measurement(prep_tr_t24, preparedness_transmission__husk_reading, theater_ratio, 24, 0.81).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__husk_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(prep_be_t4, preparedness_transmission__husk_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(prep_be_t8, preparedness_transmission__husk_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(prep_be_t12, preparedness_transmission__husk_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(prep_be_t16, preparedness_transmission__husk_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__husk_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(prep_be_t24, preparedness_transmission__husk_reading, base_extractiveness, 24, 0.58).

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
% This story is the husk_reading of the preparedness_transmission kernel: the same drill-and-inspection arrangement, read as memorial ritual with hollowed operational substance. The competence_reading (sibling) reads the identical drills as live exercised knowledge with genuine re-validation each cycle — same institutional text, opposite empirical claim about what the drills actually test. The hybrid_reading (sibling) reads the arrangement as stratified: infrastructure competence high, civilian coordination competence decayed — a middle position that this husk_reading, if taken as a total-system claim, is in some tension with. Each reading carries its own ε assessed by its own lights: this story's ε (0.58 at interval end) describes substantial but diffuse extraction of false confidence; a competence-reading sibling would author much lower ε for the same arrangement, and that difference is the data, not an error to reconcile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
