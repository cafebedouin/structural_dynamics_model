% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__engineering_absolute_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__engineering_absolute_threshold, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: rogers_commission_findings__engineering_absolute_threshold
 *   human_readable: Rogers Commission Engineering Absolute Threshold Reading
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   The Rogers Commission investigation of the Challenger disaster identified
 *   O-ring failure as the proximate cause and produced findings that have
 *   been read three different ways by NASA, engineers, contractors, and
 *   analysts. This constraint story instantiates the ENGINEERING ABSOLUTE
 *   THRESHOLD reading: Rogers findings establish that flight operations must
 *   cease until the O-ring is redesigned. Under this reading, the findings
 *   produce a veto gate held by engineering analysis — no launch is
 *   permissible until the material failure mode is corrected. This is
 *   structurally different from alternative readings that emphasize risk
 *   quantification (actuarial_risk_acceptance) or documented compliance
 *   processes (management_compliance_narrative). The constraint I author
 *   describes the state of affairs under the engineering reading's own
 *   lights: it is a structural safety boundary, not an enforcement mechanism.
 *
 * KEY AGENTS:
 *   - Engineering analysis community: holds veto authority; determines certification of redesign
 *   - Flight crew: protected beneficiary; cannot exit the constraint once committed to launch
 *   - Launch cadence operators and programme sponsors: bear the cost of suppressed operations during redesign
 *   - Rogers Commission authority: source of the contested findings; multiple readings coexist
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__engineering_absolute_threshold, 0.18).
domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, 0.92).
domain_priors:theater_ratio(rogers_commission_findings__engineering_absolute_threshold, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, extractiveness, 0.18).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__engineering_absolute_threshold, mountain).
narrative_ontology:human_readable(rogers_commission_findings__engineering_absolute_threshold, "Rogers Commission Engineering Absolute Threshold Reading").
narrative_ontology:topic_domain(rogers_commission_findings__engineering_absolute_threshold, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:emerges_naturally(rogers_commission_findings__engineering_absolute_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__engineering_absolute_threshold, '719e76ec-afae-42a0-91ba-3f4268763abe').
narrative_ontology:cs_kernel_codification('719e76ec-afae-42a0-91ba-3f4268763abe', formalized).
narrative_ontology:cs_authority_grounding('719e76ec-afae-42a0-91ba-3f4268763abe', expertise).
narrative_ontology:cs_interpretation_layer_present('719e76ec-afae-42a0-91ba-3f4268763abe').
narrative_ontology:cs_reading_relation('719e76ec-afae-42a0-91ba-3f4268763abe', rogers_commission_findings__management_compliance_narrative, forecloses).
narrative_ontology:cs_reading_relation('719e76ec-afae-42a0-91ba-3f4268763abe', rogers_commission_findings__actuarial_risk_acceptance, forecloses).
narrative_ontology:cs_axiom('719e76ec-afae-42a0-91ba-3f4268763abe', foundational, engineering_analysis_holds_launch_veto).
narrative_ontology:cs_axiom_status(engineering_analysis_holds_launch_veto, holdable).
narrative_ontology:cs_axiom_grounding('719e76ec-afae-42a0-91ba-3f4268763abe', engineering_analysis_holds_launch_veto, deontological).
narrative_ontology:cs_axiom('719e76ec-afae-42a0-91ba-3f4268763abe', foundational, material_redesign_is_prerequisite_not_negotiable).
narrative_ontology:cs_axiom_status(material_redesign_is_prerequisite_not_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('719e76ec-afae-42a0-91ba-3f4268763abe', material_redesign_is_prerequisite_not_negotiable, empirically_contingent).
narrative_ontology:cs_reference_frame('719e76ec-afae-42a0-91ba-3f4268763abe', engineering_absolute_safety_threshold).
narrative_ontology:cs_drift_state('719e76ec-afae-42a0-91ba-3f4268763abe', contemporary_space_launch_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('719e76ec-afae-42a0-91ba-3f4268763abe', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, flight_crew_safety_margin).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, flight_crew).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, programme_sponsors).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, launch_cadence_operators).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, programme_sponsors).
narrative_ontology:constraint_vindicates(rogers_commission_findings__engineering_absolute_threshold, material_properties_determine_safe_operating_envelope).
narrative_ontology:constraint_vindicates(rogers_commission_findings__engineering_absolute_threshold, engineering_redesign_precondition_for_operation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines the technical boundary: O-ring material properties, temperature sensitivity, failure modes. Holds authority to certify redesign as safe. The constraint is their analysis rendered binding: a technical fact becomes a veto gate over launch operations.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, engineering_analysis_community, agenda_setter,
    institutional, generational, analytical, global).

% Protected by the redesign requirement: no launch until the known failure mode is materially corrected. Their survival depends on the constraint holding; they cannot exit the vehicle once it launches.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, flight_crew, beneficiary,
    organized, immediate, trapped, global).

% Bear the cost: no launches until redesign is certified, no partial operations, no probabilistic arguments. The constraint suppresses their operational freedom absolutely. They cannot proceed without material change to the flight system.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, launch_cadence_operators, payer,
    institutional, biographical, constrained, global).

% Pay in delayed mission timeline and deferred objectives while redesign occurs. Also benefit from avoiding a catastrophic failure that would end the programme entirely. The constraint redirects resources from launch cadence to engineering verification.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, programme_sponsors, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__engineering_absolute_threshold, programme_sponsors, beneficiary).

% An alternative reading of the Rogers findings that interprets them as a process requirement (documented risk awareness and mitigation efforts) rather than an absolute technical threshold. This reading would permit launch with robust documentation and informed acceptance of known risk. It is structurally excluded from this constraint's framing.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, management_compliance_narrative_reading, excluded,
    analytical, biographical, analytical, global).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__engineering_absolute_threshold, management_compliance_narrative_reading).

% An alternative reading that interprets Rogers findings as requiring quantified risk and informed decision authority (acceptable to fly if failure probability is documented and accepted by competent authorities). This reading permits calibrated acceptance of known risks. It is excluded from the engineering absolute threshold reading.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, actuarial_risk_acceptance_reading, excluded,
    analytical, biographical, analytical, global).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__engineering_absolute_threshold, actuarial_risk_acceptance_reading).

% The institutional source whose investigation produced the findings. Multiple readings exist within the commission's own language; this constraint instantiates the strongest, most engineer-derived reading.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_authority, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Engineering analysis produces a technical safety boundary by rendering material properties and failure modes determinative: flight operations must not proceed until the identified failure mechanism is materially corrected. This concentrates authority over launch gates in the technical domain, preventing operational or managerial override of physics constraints.
% TRANSFER_FUNCTION: The constraint moves launch authority from operational/managerial discretion to engineering verification. It transfers launch timeline delay and engineering resource allocation from optional optimization to mandatory prerequisite. Crew safety is the coordinated good; launch cadence is the cost.
% ABSENT_VOICES: Programme scheduling interests, cost-efficiency advocates, and risk-acceptance framings that would permit probabilistic operations are structurally excluded. Alternative NASA readings and contractor perspectives that emphasize managed-risk compliance rather than absolute redesign are not participants in this reading's authority structure.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared (i.e., if the absolute redesign requirement were dropped), shuttle operations would resume on a schedule constrained only by the original risk management framework — probabilistic acceptance of known O-ring vulnerability, documented procedures, and managerial sign-off. The crew safety margin would depend on probability and procedure rather than material correction. The operational world reorganizes around risk acceptance rather than risk elimination.
% FOUNDING_PROBLEM: The Challenger disaster revealed that known failure modes (O-ring temperature sensitivity) were present in the design, acceptable in the management framework, and catastrophic in operation. The constraint was founded to prevent repetition: establish an absolute technical boundary that cannot be overridden by schedule or cost pressure.
% FOUNDING_PROBLEM_CORROBORATION: Engineering and independent safety analysis communities affirm the founding problem persists: material properties remain unchanged until redesign is complete; schedule pressure creates chronic temptation to normalize risk. Launch programme and contractor records from the Challenger era and subsequent programme cycles (STS-26 onwards) document repeated tension between the absolute redesign gate and operational cadence demands. Independent historical review (e.g., Columbia Accident Investigation Board, engineering literature post-1986) corroborates that the founding problem — known failure modes under operational conditions — remains structurally present until the material correction is in place.
narrative_ontology:disappearance_verdict(rogers_commission_findings__engineering_absolute_threshold, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__engineering_absolute_threshold, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__engineering_absolute_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rogers_commission_findings__engineering_absolute_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__engineering_absolute_threshold, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__engineering_absolute_threshold_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, ExtMetricName, E),
    domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(rogers_commission_findings__engineering_absolute_threshold),
    narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(rogers_commission_findings__engineering_absolute_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint does not extract value from anyone — it redirects timeline and resources toward safety verification. Suppression is extremely high (0.92) because the constraint absolutely prevents launch operations: no probabilistic argument, no schedule pressure, no managerial override can proceed without material change. Theater ratio is minimal (0.08) because the constraint is almost entirely functional verification and redesign, not performative maintenance. Accessibility collapse is very high (0.89) because once the failure mechanism is understood (O-ring vulnerability at low temperature), the alternative (flying with known unrepaired failure mode) is cognitively inaccessible — it is not a real option once the engineering analysis is public. Resistance is moderate (0.34) because while operators and sponsors chafe at the suppression, they do not actively resist the constraint itself; resistance comes from the management_compliance_narrative reading, which proposes an alternative interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The engineering community and flight crew see this as a binding technical boundary — a safety margin that cannot be negotiated. The launch cadence operators and programme sponsors see suppressed operations and delayed missions. The Rogers Commission's own authority accommodates multiple readings, so the gatekeeping seat (the commission's interpretation authority) is contested. The engine should compute different types across seats: the engineer seat and crew seat should compute this as mountain or close approach; the operator seat should compute this as forced extraction under an alternative reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Flight crew: d near 0 (full beneficiary — the constraint subsidizes their safety). Engineering community: d near 0.5 (symmetric — they hold authority and certify, but they also bear responsibility for the verification being complete and correct). Launch operators: d near 1.0 (full target — the constraint extracts their operational freedom absolutely until redesign is certified). Programme sponsors: d near 0.7 (target, but with secondary benefit — they recover mission capability post-redesign, so the cost is front-loaded but not permanent). The constraint is read AS a boundary only by seats that accept the engineering reading; the management_compliance_narrative reading produces a different directionality for the same agents, treating the constraint as negotiable process rather than absolute gate.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading instantiates the engineering reading's claim that the Rogers findings establish an ABSOLUTE TECHNICAL BOUNDARY. The alternative readings (management_compliance_narrative, actuarial_risk_acceptance) preserve a managerial discretion gate that permits probabilistic acceptance or process-based approval. The mandatrophy question is: does the engineering reading correctly capture the Rogers Commission's intent, or does it overstate the authority of engineering analysis relative to management authority? The answer determines whether this constraint should compute as mountain (if engineering analysis truly holds veto) or tangled_rope (if management retains override authority clothed in documentation). The contest is not resolved by the constraint's own metrics — it is a structural fact about authority allocation that the commission's findings themselves leave ambiguous.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rogers_authority_scope_ambiguity,
    'Did the Rogers Commission intend to establish an absolute technical veto held by engineering analysis, or a strengthened but ultimately managerial decision process supported by engineering input?',
    'Close textual analysis of the Rogers Commission Report, testimony during the investigation, and the Commission''s own stated findings and recommendations regarding launch authority. Compare the Commission''s language to NASA''s post-Challenger implementation decisions (STS-26 onwards) and their stated rationale for those decisions.',
    'If the engineering reading is correct (absolute veto), this constraint should compute as mountain or near-mountain for all seats. If the management reading is correct (strengthened process), the constraint distributes authority differently and may compute as tangled_rope with management retaining final authority. The entire seat divergence depends on resolving this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rogers_authority_scope_ambiguity, conceptual, 'Whether Rogers findings establish absolute engineering veto or strengthened management process.').

omega_variable(
    redesign_completion_as_constraint_termination,
    'Does this constraint persist only until the O-ring redesign is certified, or does it encode a permanent structural priority of engineering safety veto in the flight programme?',
    'Observe post-redesign operations: do subsequent flights proceed under a return to pre-Challenger management authority structures, or does the engineering veto gate persist? Do NASA''s Flight Readiness Review procedures post-STS-26 preserve engineering authority indefinitely or transition it back to management discretion once the known failure mode is corrected?',
    'If the constraint terminates with redesign certification, it is a temporary structure (scaffold candidate). If it persists structurally, it represents a permanent authority reallocation (mountain or roof-level norm that outlasts the specific O-ring issue). The distinction affects long-term classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(redesign_completion_as_constraint_termination, empirical, 'Whether the engineering veto is O-ring-specific or a structural authority reallocation.').

omega_variable(
    natural_law_vs_constructed_authority,
    'Does this constraint emerge as a natural necessity of physics (material properties determine safe operations) or as a constructed institutional authority structure (engineering profession exercises veto power)?',
    'Distinguish the claim that O-ring failure is a physical fact (true, unchallengeable, natural) from the claim that engineers hold binding authority to prevent operations (institutional, contingent, constructed by NASA''s post-Challenger decisions). Both are true but are categorically different — one is a property of the material, the other is a property of the authority structure.',
    'If this is a natural law, the constraint should compute as mountain across all seats. If it is a constructed authority structure (albeit justified by physics), it is a social construction and may compute as rope or tangled_rope depending on whether the authority is seen as legitimate or extractive. The beneficiaries (flight crew safety) vindicate the engineering authority, but that vindication does not make the authority natural — it makes it institutionally settled and consensual.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_authority, conceptual, 'Whether the constraint is a physical necessity or an institutional authority structure justified by physics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__engineering_absolute_threshold, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(roge_tr_t0, observed).
narrative_ontology:measurement(roge_tr_t3, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 3, 0.07).
narrative_ontology:measurement_basis(roge_tr_t3, observed).
narrative_ontology:measurement(roge_tr_t6, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 6, 0.07).
narrative_ontology:measurement_basis(roge_tr_t6, observed).
narrative_ontology:measurement(roge_tr_t12, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 12, 0.08).
narrative_ontology:measurement_basis(roge_tr_t12, observed).
narrative_ontology:measurement(roge_tr_t18, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 18, 0.08).
narrative_ontology:measurement_basis(roge_tr_t18, observed).
narrative_ontology:measurement(roge_tr_t24, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 24, 0.08).
narrative_ontology:measurement_basis(roge_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(roge_be_t0, observed).
narrative_ontology:measurement(roge_be_t3, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 3, 0.16).
narrative_ontology:measurement_basis(roge_be_t3, observed).
narrative_ontology:measurement(roge_be_t6, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 6, 0.17).
narrative_ontology:measurement_basis(roge_be_t6, observed).
narrative_ontology:measurement(roge_be_t12, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 12, 0.18).
narrative_ontology:measurement_basis(roge_be_t12, observed).
narrative_ontology:measurement(roge_be_t18, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 18, 0.18).
narrative_ontology:measurement_basis(roge_be_t18, observed).
narrative_ontology:measurement(roge_be_t24, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 24, 0.18).
narrative_ontology:measurement_basis(roge_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0, 0.91).
narrative_ontology:measurement_basis(roge_su_t0, observed).
narrative_ontology:measurement(roge_su_t3, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 3, 0.92).
narrative_ontology:measurement_basis(roge_su_t3, observed).
narrative_ontology:measurement(roge_su_t6, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 6, 0.92).
narrative_ontology:measurement_basis(roge_su_t6, observed).
narrative_ontology:measurement(roge_su_t12, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 12, 0.92).
narrative_ontology:measurement_basis(roge_su_t12, observed).
narrative_ontology:measurement(roge_su_t18, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 18, 0.92).
narrative_ontology:measurement_basis(roge_su_t18, observed).
narrative_ontology:measurement(roge_su_t24, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 24, 0.92).
narrative_ontology:measurement_basis(roge_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__engineering_absolute_threshold, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__management_compliance_narrative).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Rogers Commission findings kernel. The engineering_absolute_threshold reading emphasizes material safety as a binding veto; the management_compliance_narrative reading emphasizes documented process as sufficient for managerial approval; the actuarial_risk_acceptance reading emphasizes quantified risk and informed decision-maker acceptance. All three readings claim authority from the Rogers Commission investigation. Each is a structurally distinct constraint with different ε (extractiveness of launch suppression), different beneficiary/victim structures, and different authority allocation. They are linked via network.affects_constraints because the reading contest is live: whichever reading institutionalizes as NASA policy determines the actual constraint. The engineering reading is authored here; sibling readings should be authored separately to model the institutional uncertainty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
