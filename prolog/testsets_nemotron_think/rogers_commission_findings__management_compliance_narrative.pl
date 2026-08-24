% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__management_compliance_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__management_compliance_narrative, []).

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
 *   constraint_id: rogers_commission_findings__management_compliance_narrative
 *   human_readable: Rogers Commission Management Compliance Narrative
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   The Rogers Commission findings on the Challenger disaster were
 *   ambiguously framed: they recommended improved 'management systems,'
 *   'communication,' and 'safety organizations' without specifying whether
 *   technical safety thresholds must be met or merely documented. NASA
 *   management adopted a compliance reading — demonstrating documented risk
 *   awareness and mitigation efforts became sufficient to proceed. This
 *   reading preserved management launch authority while creating a paper
 *   trail that served as both coordination mechanism and extraction cover.
 *   The constraint is the standing arrangement: a documentation-driven launch
 *   decision process that replaced engineering veto with management
 *   rationale.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.65).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.6).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.65).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Rogers Commission Management Compliance Narrative").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, '281c89a1-7415-42fd-9f10-b6be3e15ee37').
narrative_ontology:cs_kernel_codification('281c89a1-7415-42fd-9f10-b6be3e15ee37', formalized).
narrative_ontology:cs_authority_grounding('281c89a1-7415-42fd-9f10-b6be3e15ee37', extraction).
narrative_ontology:cs_interpretation_layer_present('281c89a1-7415-42fd-9f10-b6be3e15ee37').
narrative_ontology:cs_reading_relation('281c89a1-7415-42fd-9f10-b6be3e15ee37', rogers_commission_findings__engineering_absolute_threshold, forecloses).
narrative_ontology:cs_reading_relation('281c89a1-7415-42fd-9f10-b6be3e15ee37', rogers_commission_findings__actuarial_risk_acceptance, coexists_with).
narrative_ontology:cs_axiom('281c89a1-7415-42fd-9f10-b6be3e15ee37', foundational, documented_process_suffices_for_authorization).
narrative_ontology:cs_axiom_status(documented_process_suffices_for_authorization, holdable).
narrative_ontology:cs_axiom_grounding('281c89a1-7415-42fd-9f10-b6be3e15ee37', documented_process_suffices_for_authorization, conventional).
narrative_ontology:cs_axiom('281c89a1-7415-42fd-9f10-b6be3e15ee37', secondary, management_authority_preserved_through_documentation).
narrative_ontology:cs_axiom_status(management_authority_preserved_through_documentation, holdable).
narrative_ontology:cs_axiom_grounding('281c89a1-7415-42fd-9f10-b6be3e15ee37', management_authority_preserved_through_documentation, conventional).
narrative_ontology:cs_reference_frame('281c89a1-7415-42fd-9f10-b6be3e15ee37', post_challenger_compliance_framework).
narrative_ontology:cs_drift_state('281c89a1-7415-42fd-9f10-b6be3e15ee37', columbia_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('281c89a1-7415-42fd-9f10-b6be3e15ee37', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, nasa_management).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, contractor_management).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, political_stakeholders).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, engineering_safety_organizations).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, astronaut_corps).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, contractor_management).
narrative_ontology:constraint_vindicates(rogers_commission_findings__management_compliance_narrative, institutional_procedure_legitimizes_risk_acceptance).
narrative_ontology:constraint_vindicates(rogers_commission_findings__management_compliance_narrative, documented_awareness_substitutes_for_technical_resolution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains launch decision authority by requiring only documented risk awareness and mitigation paperwork rather than technical resolution. The compliance process lets management proceed on schedule while creating a paper trail that shields them from retrospective blame. They control the process definition, the documentation standards, and the final go/no-go decision.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, nasa_management, agenda_setter,
    institutional, generational, arbitrage, global).

% Lost effective veto authority over launch decisions. Their technical objections are now channeled into a documentation process where concerns are recorded but can be overridden by management's documented rationale. Engineers who persist face career pressure; those who comply see their expertise reduced to a checkbox exercise. Exit means leaving NASA or the contractor base entirely.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, engineering_safety_organizations, payer,
    organized, biographical, constrained, national).

% Thiokol and other contractors benefit from contract continuity and schedule adherence — the compliance narrative lets them avoid costly redesign delays. But they also bear liability exposure and internal engineering dissent. Their engineers face the same veto erosion as NASA's, while management captures the schedule and budget benefits.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, contractor_management, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__management_compliance_narrative, contractor_management, payer).

% Bear the physical risk of launches authorized under the compliance process. Their professional identity is fused to the flight program — leaving means abandoning their life's work and the astronaut identity itself. They have no operational authority over the risk acceptance process; their input is advisory and can be documented and overridden.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, astronaut_corps, payer,
    organized, biographical, identity_locked, national).

% Conducts hearings and receives testimony after failures. Has budget and confirmation leverage but no real-time operational authority. Relies on the same documentation the compliance process produces — the paper trail becomes the oversight record. Their structural position is retrospective, not preventive.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, congressional_oversight, observer,
    institutional, generational, analytical, national).

% Funds the program and suffers the consequences of failure but has no seat in the risk acceptance process. The compliance narrative is performed partly for public consumption — the documented process creates an appearance of due diligence that substitutes for actual technical resolution. No mechanism exists for public input into launch decisions.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, public, excluded,
    powerless, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a standardized process for documenting risk awareness and mitigation efforts so that launch decisions can proceed on schedule without requiring technical consensus or engineering veto. Solves the coordination problem of aligning multiple organizational actors (NASA centers, contractors, oversight) around a single go/no-go decision framework.
% TRANSFER_FUNCTION: Transfers launch authority from engineering safety organizations (who lose veto power) to NASA management (who retain decision authority with only a documentation burden). Also transfers risk from the organization to the astronaut corps, who bear the physical consequences of risks documented but not resolved.
% ABSENT_VOICES: Front-line engineers who refused to sign off on the Challenger launch (Boisjoly, Thompson, etc.) were structurally excluded from the post-Rogers compliance framework — their dissent was the catalyst for the Commission but the resulting process institutionalized the very override they opposed. Future engineers who might refuse face the same exclusion.
% DISAPPEARANCE_RATIONALE: If the compliance process vanished, NASA would either revert to engineering veto authority (delaying launches until technical resolution) or adopt a more explicit risk-acceptance framework (quantified probability thresholds). The current schedule-driven launch cadence depends on the documentation substitute for technical resolution.
% FOUNDING_PROBLEM: Post-Challenger, NASA needed to restore launch capability while satisfying political demand for 'fixed' processes. The Rogers Commission recommendations were ambiguous enough to be read as a documentation requirement rather than a technical mandate. Management needed a framework that preserved launch authority while appearing to implement Commission findings.
% FOUNDING_PROBLEM_CORROBORATION: The Rogers Commission report itself (Chapter VI, Recommendations) emphasizes 'management systems' and 'communication' over technical thresholds — Commission members have attested this was intentional flexibility. NASA management contemporaneously celebrated the 'return to flight' on schedule. Engineering organizations (ASAP, NASA safety centers) contemporaneously warned the process replaced technical rigor with paperwork. Columbia Accident Investigation Board (2003) validated the engineering warning: 'organizational causes' traced to the same compliance-substitution pattern.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rogers_commission_findings__management_compliance_narrative, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__management_compliance_narrative, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__management_compliance_narrative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rogers_commission_findings__management_compliance_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects management's retained authority to override technical dissent through documentation. Suppression (0.60) captures how engineering concerns are channeled into process rather than resolved — the Flight Readiness Review became a ritual where objections are recorded but not dispositive. Theater ratio (0.55) measures the growing gap between the compliance performance (extensive documentation, formal reviews) and the actual risk resolution (foam shedding, O-ring erosion persisted). Accessibility collapse (0.50) shows alternatives (redesign, delay) existed but were procedurally difficult. Resistance (0.55) reflects sustained but ultimately ineffective engineering pushback. The claimed type tangled_rope captures the dual nature: genuine coordination (standardized risk documentation across centers/contractors) AND asymmetric extraction (management authority preserved at engineering's expense).
 *
 * PERSPECTIVAL GAP:
 *   From management's seat, the constraint is a rope: it coordinates complex multi-center launch decisions through a common process. From engineering's seat, it is a snare: the coordination story is cover for extracting their veto authority. From astronauts' seat, it is a snare with identity_locked exit — they cannot leave the risk. The engine computes this divergence from the structural data; the authored claim (tangled_rope) acknowledges both functions exist simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   NASA management is the structural beneficiary (d near 0.0) — they collect schedule adherence, budget predictability, and political cover while controlling the process. Contractor management is a secondary beneficiary (d ~0.2) — they gain contract continuity but absorb some liability. Engineering safety organizations are primary targets (d ~0.85) — their veto power is extracted, their expertise reduced to advisory input. Astronaut corps are trapped targets (d ~0.9, identity_locked) — they bear physical risk with no decision authority. Congressional oversight and public are analytical/excluded seats — they observe the theater but cannot intervene in real time.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (restore flight while appearing to fix the process) is contested — management says live, engineers say dead (the technical risks were never resolved, only documented). The mandate has atrophied: the compliance process persists but the original justification (Challenger lessons) has been inverted — the process now enables the same normalization of deviance it was meant to prevent. Columbia confirmed the mandatrophy: the same foam-risk documentation process that 'worked' for 17 flights failed catastrophically on the 18th.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the documented risk awareness process a genuine coordination mechanism that improves collective risk understanding, or is the coordination function entirely a cover for management authority extraction?',
    'Compare risk communication quality and cross-organizational learning before vs. after Rogers. If engineers report better awareness of each other''s concerns and management decisions reflect integrated risk pictures, coordination is real. If documentation is performative and decisions ignore documented dissent, extraction dominates.',
    'If coordination is genuine, the constraint is a rope with moderate extraction overhead. If coordination is cover, it is a snare masquerading as tangled_rope. The difference changes the remediation: improve the process vs. restore engineering veto.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether the constraint''s coordination function is structurally real or performative cover.').

omega_variable(
    engineering_veto_ever_real,
    'Did engineering organizations ever possess a genuine structural veto over launch decisions, or was the pre-Challenger ''veto'' always informal and management-discretionary?',
    'Historical analysis of pre-1986 Flight Readiness Reviews: count instances where engineering objection stopped a launch vs. where management overruled. Interview surviving participants about perceived authority.',
    'If veto was never structural, the Rogers Commission didn''t extract it — it formalized the pre-existing management authority. The constraint is a snare from inception. If veto was real and lost, the extraction is a measurable transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineering_veto_ever_real, empirical, 'Whether the victim''s lost authority was ever structurally real.').

omega_variable(
    documented_awareness_vs_mitigation,
    'Does the compliance process distinguish between documenting awareness of an unresolved risk and documenting actual mitigation, or does it treat them as equivalent?',
    'Content analysis of Flight Readiness Review records 1988-2003: code entries for ''risk acknowledged,'' ''mitigation implemented,'' ''mitigation planned,'' ''risk accepted.'' Measure frequency of launch proceed decisions with only ''acknowledged'' entries.',
    'If awareness and mitigation are treated equivalently, the process is pure extraction theater. If mitigation is required and verified, the coordination function has teeth. The distinction determines whether the theater_ratio measures performance gap or structural fraud.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(documented_awareness_vs_mitigation, empirical, 'Whether the process''s central distinction (awareness vs. mitigation) is enforced or collapsed.').

omega_variable(
    kernel_framing_alternatives,
    'Does the Rogers Commission text genuinely support the management compliance reading, or does that reading require selective interpretation that forecloses the engineering threshold reading?',
    'Textual analysis of Commission recommendations (Chapter VI) mapped to the three readings. Identify which recommendations each reading claims as support and which recommendations each reading must explain away.',
    'If the management reading requires suppressing Commission language about ''safety organizations independent of management'' and ''technical resolution before flight,'' the forecloses relation to engineering_absolute_threshold is structural, not just political. If the text is genuinely ambiguous, coexists_with is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternatives, conceptual, 'Whether the kernel text structurally favors one reading or is genuinely polysemic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 1987, 2003).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t1987, rogers_commission_findings__management_compliance_narrative, theater_ratio, 1987, 0.35).
narrative_ontology:measurement(roge_tr_t1990, rogers_commission_findings__management_compliance_narrative, theater_ratio, 1990, 0.42).
narrative_ontology:measurement(roge_tr_t1993, rogers_commission_findings__management_compliance_narrative, theater_ratio, 1993, 0.48).
narrative_ontology:measurement(roge_tr_t1996, rogers_commission_findings__management_compliance_narrative, theater_ratio, 1996, 0.51).
narrative_ontology:measurement(roge_tr_t1999, rogers_commission_findings__management_compliance_narrative, theater_ratio, 1999, 0.53).
narrative_ontology:measurement(roge_tr_t2003, rogers_commission_findings__management_compliance_narrative, theater_ratio, 2003, 0.55).

% Extraction over time
narrative_ontology:measurement(roge_be_t1987, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 1987, 0.45).
narrative_ontology:measurement(roge_be_t1990, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(roge_be_t1993, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 1993, 0.58).
narrative_ontology:measurement(roge_be_t1996, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 1996, 0.61).
narrative_ontology:measurement(roge_be_t1999, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 1999, 0.63).
narrative_ontology:measurement(roge_be_t2003, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 2003, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t1987, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 1987, 0.4).
narrative_ontology:measurement(roge_su_t1990, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 1990, 0.48).
narrative_ontology:measurement(roge_su_t1993, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 1993, 0.52).
narrative_ontology:measurement(roge_su_t1996, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 1996, 0.55).
narrative_ontology:measurement(roge_su_t1999, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 1999, 0.58).
narrative_ontology:measurement(roge_su_t2003, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 2003, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__management_compliance_narrative, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rogers_commission_findings__management_compliance_narrative, 0.1).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__actuarial_risk_acceptance).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, nasa_flight_readiness_review_process).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, contractor_safety_certification_process).

% DUAL FORMULATION NOTE:
% Part of the Rogers Commission findings constraint family. This reading (management_compliance_narrative) decomposes the Commission's ambiguous recommendations into a documentation-driven compliance process. The engineering_absolute_threshold reading decomposes the same text into a technical safety boundary (Mountain). The actuarial_risk_acceptance reading decomposes it into a quantified risk threshold (Rope/Tangled Rope). All three are linked because the Commission report is cited as authority by all three institutional positions. This reading's extraction (management authority) depends on the engineering reading's authority being displaced — they are in structural tension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rogers_commission_findings__management_compliance_narrative, organized, 0.85).
constraint_indexing:directionality_override(rogers_commission_findings__management_compliance_narrative, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
