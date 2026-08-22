% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__management_compliance_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: rogers_commission_findings__management_compliance_narrative
 *   human_readable: Rogers Commission Management Compliance Narrative
 *   domain: aerospace/organizational/regulatory
 *
 * SUMMARY:
 *   The Rogers Commission investigation of the 1986 Challenger disaster
 *   identified inadequate documentation and escalation of known O-ring
 *   hazards as the root cause. Three structurally distinct constraints can be
 *   extracted from the Commission's findings, corresponding to three
 *   different readings of what 'compliance' means: (1)
 *   management_compliance_narrative (THIS story): documented risk awareness
 *   and mitigation planning suffice to authorize launch; (2)
 *   engineering_absolute_threshold: flight operations must cease until the
 *   known hazard (O-ring) is engineered away; (3) actuarial_risk_acceptance:
 *   quantified risk acceptance by informed decision-makers is the standard.
 *   This story instantiates the management_compliance_narrative reading:
 *   compliance means demonstrating that management KNEW the hazards and
 *   proposed mitigation. This reading permits launch to proceed over
 *   engineering objection if documentation is adequate. The constraint is
 *   substantially extractive (ε=0.68): it shifts decision authority from
 *   engineering expertise to management documentation, allowing programs to
 *   continue even when technical hazards remain unresolved. It requires
 *   active enforcement (suppression=0.72) to prevent engineering from
 *   reverting to a safety veto. Theater is substantial (0.58) because
 *   documentation compliance can become decoupled from actual mitigation
 *   efficacy—the constraint rewards the appearance of risk awareness, not its
 *   operational reality.
 *
 * KEY AGENTS:
 *   - program_management: agenda-setter, institutional power, benefits from launch continuity via documented-risk authority
 *   - engineering_safety_authority: payer, powerful, loses veto power and identity-locks to safety expertise that is redefined as 'risk documentation'
 *   - nasa_leadership: agenda-setter and beneficiary, institutional, enables program continuity
 *   - astronauts_and_crew: payer, trapped, bear physical risk on management's documented rationale
 *   - public_safety_interest: excluded, powerless, bears residual unmitigated risk
 *   - rogers_commission: observer, institutional, authority ground for the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.68).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.72).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.68).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Rogers Commission Management Compliance Narrative").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "aerospace/organizational/regulatory").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, '42b148bc-59d4-4830-bca1-dbaa6bb6ab9d').
narrative_ontology:cs_kernel_codification('42b148bc-59d4-4830-bca1-dbaa6bb6ab9d', fixed_text).
narrative_ontology:cs_authority_grounding('42b148bc-59d4-4830-bca1-dbaa6bb6ab9d', lineage).
narrative_ontology:cs_interpretation_layer_present('42b148bc-59d4-4830-bca1-dbaa6bb6ab9d').
narrative_ontology:cs_reading_relation('42b148bc-59d4-4830-bca1-dbaa6bb6ab9d', rogers_commission_findings__engineering_absolute_threshold, forecloses).
narrative_ontology:cs_reading_relation('42b148bc-59d4-4830-bca1-dbaa6bb6ab9d', rogers_commission_findings__actuarial_risk_acceptance, coexists_with).
narrative_ontology:cs_axiom('42b148bc-59d4-4830-bca1-dbaa6bb6ab9d', foundational, documented_risk_awareness_authorizes_launch).
narrative_ontology:cs_axiom_status(documented_risk_awareness_authorizes_launch, holdable).
narrative_ontology:cs_axiom_grounding('42b148bc-59d4-4830-bca1-dbaa6bb6ab9d', documented_risk_awareness_authorizes_launch, deontological).
narrative_ontology:cs_axiom('42b148bc-59d4-4830-bca1-dbaa6bb6ab9d', secondary, management_accountability_through_documentation).
narrative_ontology:cs_axiom_status(management_accountability_through_documentation, holdable).
narrative_ontology:cs_axiom_grounding('42b148bc-59d4-4830-bca1-dbaa6bb6ab9d', management_accountability_through_documentation, conventional).
narrative_ontology:cs_reference_frame('42b148bc-59d4-4830-bca1-dbaa6bb6ab9d', management_documented_risk_authority).
narrative_ontology:cs_drift_state('42b148bc-59d4-4830-bca1-dbaa6bb6ab9d', contemporary_post_commission_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('42b148bc-59d4-4830-bca1-dbaa6bb6ab9d', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, program_management).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, launch_schedule_continuity).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, engineering_safety_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, nasa_leadership).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, astronauts_and_crew).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains launch authority by demonstrating documented risk awareness and mitigation efforts. Sets the compliance narrative: if management can show it KNEW the risks and took mitigation steps, the constraint is satisfied and launch may proceed. Collects the benefit of schedule continuity and mission execution. Faces pressure to document rationale post-hoc and to frame engineering concerns as 'mitigatable' rather than disqualifying.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, program_management, agenda_setter,
    institutional, biographical, constrained, national).

% Loses the authority to veto launch on safety grounds alone. Engineers can raise concerns and demand documentation, but if management documents awareness and proposes mitigation, the constraint allows launch to proceed over engineering objection. Their professional identity is fused with the claim that unresolved safety issues should block flight; the constraint redefines safety authority as 'documented risk acceptance' rather than 'absence of known hazards.'
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, engineering_safety_authority, payer,
    powerful, biographical, identity_locked, national).

% Issued the findings that ground the constraint. The Commission's written text is read by management as authorizing the compliance-narrative path and by engineers as endorsing the absolute-threshold path. The constraint is one reading of the Commission's authority; the Commission itself is not a party collecting from the arrangement.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, rogers_commission, observer,
    institutional, biographical, analytical, national).

% Benefits from the constraint's permission to continue the Space Shuttle program on a documented-risk basis. Faces political pressure to demonstrate continued launch capability and mission readiness. Can justify launch decisions by pointing to management compliance documentation.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, nasa_leadership, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__management_compliance_narrative, nasa_leadership, agenda_setter).

% Bear the physical risk if documented mitigation proves insufficient. They are not seated at the decision table that authorizes launch; their participation is limited to informed consent after the fact. The constraint allows launch to proceed on management's documented-risk rationale even if they object.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, astronauts_and_crew, payer,
    moderate, immediate, trapped, national).

% Bears residual risk from failures that documented mitigation did not prevent. The public cannot veto launch and has no seat in the compliance evaluation; they can only learn the outcome after launch.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, public_safety_interest, excluded,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__management_compliance_narrative, program_management).
narrative_ontology:fixing_cost_class(rogers_commission_findings__management_compliance_narrative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared framework for risk governance: management must demonstrate it KNOWS the hazards and has proposed mitigation, creating accountability and transparency around launch decisions.
% TRANSFER_FUNCTION: Moves decision authority from engineering (technical veto) to management (documented-risk acceptance). Engineers retain a voice in identifying hazards, but management retains authority to authorize launch if it documents awareness and mitigation plans.
% ABSENT_VOICES: The astronauts have limited voice (informed consent, not veto). The public has no voice at all. Alternative safety readings (engineering absolute threshold, actuarial risk acceptance) compete for control of what 'compliance' means but are not seated in the decision structure.
% DISAPPEARANCE_RATIONALE: If this compliance-narrative constraint vanished, engineering safety concerns would revert to a veto power; launches could not proceed without unqualified engineering sign-off. The Shuttle program would either implement O-ring redesign before flight or remain grounded until redesign was complete. Management would lose the authority to launch on a documented-risk basis.
% FOUNDING_PROBLEM: The Rogers Commission found that the Space Shuttle program had a culture of normalized risk acceptance without adequate documentation or escalation of known hazards. The O-ring vulnerability was known but not centrally managed or communicated as a disqualifying issue.
% FOUNDING_PROBLEM_CORROBORATION: The Rogers Commission Report itself documents the founding problem: the 1986 Challenger accident resulted from a known technical hazard (O-ring performance in cold) that was not treated as disqualifying by decision-makers. Subsequent independent analysis of NASA's engineering culture and decision-making processes (e.g., The Right Stuff literature, NASA internal reviews) corroborate that inadequate documentation and escalation of known hazards was the structural problem. The Commission's recommendation—that documented risk awareness be a management responsibility—addresses this.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rogers_commission_findings__management_compliance_narrative, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__management_compliance_narrative, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68 at interval end) because the constraint transfers decision authority from engineering (who must be proven right by technical evidence) to management (who must be proven aware, a much lower bar). The measurement series tracks a gradual increase in extractiveness over the interval as the documented-risk narrative becomes institutionalized and engineering objections are routinized as 'concerns to be documented' rather than disqualifying hazards. Suppression is high (0.72) because the constraint's persistence depends on actively preventing engineering from reasserting veto authority; without continuous enforcement of the 'documented risk' standard, engineers would revert to 'show-me-it-is-fixed' authority. Theater is substantial (0.58) because compliance documentation can become performative: the ritual of writing down that a hazard is known and mitigation is planned can substitute for actual mitigation completion. The measurement trajectory shows rising theater as the constraint matures—early in the interval, documentation correlates with actual mitigation efforts; by the end, compliance paperwork increasingly exists independent of mitigation progress. Accessibility_collapse is moderate (0.62): engineers know they can raise objections and demand documentation, but once documentation exists, their exit from the launch-authority structure is nearly total. Resistance is high (0.71): engineers mount continuous resistance through formal objection, memos, and appeals, but resistance is channeled into documentation requirements rather than being permitted to veto launch.
 *
 * PERSPECTIVAL GAP:
 *   From management's seat, the constraint is genuine coordination: it creates accountability, transparency, and documented rationale for risk acceptance—a real improvement over pre-Commission ad-hoc decision-making. From engineering's seat, the constraint is extraction: it strips away their authority to veto on safety grounds and replaces it with a requirement to document objections that management can then authorize around. The engine computes this divergence from the stakeholder roles and directionality: management has low d (benefits from the arrangement, retains authority), engineering has high d (pays through loss of veto, identity-locked to safety expertise that is redefined as 'risk documentation'). The beneficiary/victim declarations feed directionality; the computed per-seat types should diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   Program management is the structural beneficiary: the constraint permits them to authorize launch by demonstrating awareness and proposing mitigation, which is a much weaker bar than engineering must clear (actually solve the hazard). d for management is near beneficiary end (~0.15–0.25): they collect the benefit of continued program execution and retain authority to decide. Engineering is the structural victim: they lose the authority to veto based on unresolved hazards; instead they are required to document objections, which management can then authorize around. d for engineering is high (~0.75–0.85): they bear the cost of lost authority and are identity-locked (their professional identity fuses with the belief that unresolved safety hazards should block flight). Astronauts are secondary payers: they are not at the decision table but bear physical risk. NASA leadership sits as beneficiary/agenda-setter: they collect the benefit of program continuity while sharing authority with program management. The public is excluded: they have no seat at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (inadequate documentation and escalation of known hazards) is LIVE and ongoing at t=12. The constraint exists to address it by institutionalizing documented-risk acceptance as the standard. However, there is a risk that the constraint's persistence could outlast the founding problem's salience. If engineering practices improve dramatically and O-ring hazards are fully mitigated, the constraint could become a vestigial theater: documentation would exist without corresponding hazard. At that point the constraint would cease to be tangled_rope (coordination + asymmetric extraction) and become piton (inertial performance). The measurement series shows rising theater_ratio, which is a leading indicator of this drift. The classification holds as tangled_rope as long as the founding problem remains live (unresolved engineering hazards exist and management continues to authorize over engineering objection), but mandatrophy monitoring is warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the Rogers Commission Report authorize documented-risk acceptance by management, or does it require engineering resolution of known hazards, or does it require probabilistic risk quantification as the standard?',
    'Textual analysis of the Commission Report''s actual language on decision authority and compliance standards; expert exegesis by Commission members or their successors; comparison of post-Challenger Space Shuttle flight authorization practices against each reading.',
    'Different readings emit different constraints with different ε values and victim sets. If the management_compliance_narrative reading is the authorized one, the constraint stands as authored. If the engineering_absolute_threshold reading is correct, this constraint is a misreading and should be reclassified as snare (extraction disguised as authorization). If the actuarial reading is correct, this constraint is a partial reading that misses the quantification requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Kernel reading ambiguity: what does Rogers authorize?').

omega_variable(
    documentation_mitigation_decoupling,
    'Does documented risk awareness and mitigation planning, in practice, correlate with actual hazard mitigation, or does documentation become decoupled from operational safety over time?',
    'Historical analysis of Space Shuttle flight data post-1986: track correlation between documented mitigation plans and actual in-flight system performance; gap analysis showing where documented mitigations were not implemented or became superseded.',
    'If documentation remains coupled to mitigation, theater_ratio stays moderate and the constraint functions as intended (management accountability). If decoupling occurs (documentation exists without corresponding mitigation), theater_ratio rises toward 1.0 and the constraint drifts toward piton (performative compliance without safety effect). This affects mandatrophy classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_mitigation_decoupling, empirical, 'Whether documentation compliance correlates with actual hazard mitigation.').

omega_variable(
    engineering_identity_lock_stability,
    'How deep and stable is the identity-lock of aerospace engineers to the belief that unresolved safety hazards should block flight? Will engineers accept redefinition of safety authority as ''documented risk acceptance,'' or will the identity-lock resist and eventually force a reversion to veto authority?',
    'Post-Challenger engineering culture studies; interviews with aerospace safety engineers about their professional identity and authority under the documented-risk regime; historical tracking of whether engineering objections ever successfully blocked launches despite management documentation.',
    'If identity-lock is stable, engineering suppression can be maintained indefinitely and the constraint persists. If identity-lock fractures (engineers repudiate the documented-risk framing and demand veto restoration), suppression costs rise sharply and the constraint may become unsustainable. This affects the durability of the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(engineering_identity_lock_stability, empirical, 'Stability of engineering identity-lock to safety expertise under the documented-risk regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__management_compliance_narrative, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(roge_tr_t0, observed).
narrative_ontology:measurement(roge_tr_t3, rogers_commission_findings__management_compliance_narrative, theater_ratio, 3, 0.48).
narrative_ontology:measurement_basis(roge_tr_t3, observed).
narrative_ontology:measurement(roge_tr_t6, rogers_commission_findings__management_compliance_narrative, theater_ratio, 6, 0.53).
narrative_ontology:measurement_basis(roge_tr_t6, observed).
narrative_ontology:measurement(roge_tr_t9, rogers_commission_findings__management_compliance_narrative, theater_ratio, 9, 0.56).
narrative_ontology:measurement_basis(roge_tr_t9, observed).
narrative_ontology:measurement(roge_tr_t12, rogers_commission_findings__management_compliance_narrative, theater_ratio, 12, 0.58).
narrative_ontology:measurement_basis(roge_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(roge_be_t0, observed).
narrative_ontology:measurement(roge_be_t3, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 3, 0.62).
narrative_ontology:measurement_basis(roge_be_t3, observed).
narrative_ontology:measurement(roge_be_t6, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 6, 0.65).
narrative_ontology:measurement_basis(roge_be_t6, observed).
narrative_ontology:measurement(roge_be_t9, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 9, 0.67).
narrative_ontology:measurement_basis(roge_be_t9, observed).
narrative_ontology:measurement(roge_be_t12, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 12, 0.68).
narrative_ontology:measurement_basis(roge_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(roge_su_t0, observed).
narrative_ontology:measurement(roge_su_t3, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 3, 0.68).
narrative_ontology:measurement_basis(roge_su_t3, observed).
narrative_ontology:measurement(roge_su_t6, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 6, 0.7).
narrative_ontology:measurement_basis(roge_su_t6, observed).
narrative_ontology:measurement(roge_su_t9, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 9, 0.71).
narrative_ontology:measurement_basis(roge_su_t9, observed).
narrative_ontology:measurement(roge_su_t12, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 12, 0.72).
narrative_ontology:measurement_basis(roge_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__management_compliance_narrative, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct constraints derived from the Rogers Commission findings on the Challenger disaster. All three share the same kernel text (the Commission Report) but represent different readings of what the Commission authorizes as the compliance standard. This reading (management_compliance_narrative) authorizes management to proceed on documented risk awareness; engineering_absolute_threshold authorizes cessation until redesign; actuarial_risk_acceptance authorizes informed probabilistic acceptance. The three constraints have different ε values, different beneficiary/victim sets, and different authority-grounding structures. They form a constraint family linked by the contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rogers_commission_findings__management_compliance_narrative, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
