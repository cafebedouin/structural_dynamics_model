% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__management_compliance_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Rogers Commission Compliance Narrative: Documented Risk Acceptance Process
 *   domain: organizational/safety/regulatory
 *
 * SUMMARY:
 *   The Rogers Commission investigated the 1986 Challenger disaster and
 *   issued findings on O-ring thermal performance under cold-weather launch
 *   conditions. The constraint story concerns how those findings were
 *   interpreted into an operational rule. This reading instantiates the
 *   'management compliance narrative': Rogers findings establish a compliance
 *   process—demonstrate documented risk awareness and mitigation efforts
 *   sufficient to proceed. Under this reading, the Commission's findings
 *   become a permission structure for management to resume operations by
 *   satisfying a procedural standard (documented awareness) rather than a
 *   mandate to halt until technical problems are solved. Management retains
 *   launch authority; engineering judgment becomes one input that management
 *   may override if documentation exists. The constraint is claimed as
 *   tangled_rope: it coordinates a decision process (resolves the question of
 *   whether to resume flights) while extracting authority from the
 *   technical-safety domain and redistributing it to management prerogative.
 *   The measurement series tracks extraction accumulation over the
 *   operational interval as the compliance narrative normalizes and theater
 *   rises (documentation activity increases while underlying technical
 *   concerns persist).
 *
 * KEY AGENTS:
 *   - nasa_program_management: agenda-setter, institutional power, constrained exit — sets and administers the compliance standard, retains launch authority
 *   - engineering_safety_authority: payer, organized power, identity-locked exit — issues technical recommendations that are subordinated to management's documented-acceptance frame
 *   - contractor_operational_continuity: beneficiary, powerful — benefits from program continuation without hardware redesign halt
 *   - external_oversight_bodies: payer, moderate power — authority to investigate post-hoc but structurally bypassed by forward-authorized compliance frame
 *   - accident_investigation_authority: observer, institutional power — post-hoc examiner of whether compliance procedures were followed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.68).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.71).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.68).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Rogers Commission Compliance Narrative: Documented Risk Acceptance Process").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "organizational/safety/regulatory").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, 'b50431cc-bcbb-4c4a-b95e-695e1557398f').
narrative_ontology:cs_kernel_codification('b50431cc-bcbb-4c4a-b95e-695e1557398f', fixed_text).
narrative_ontology:cs_authority_grounding('b50431cc-bcbb-4c4a-b95e-695e1557398f', extraction).
narrative_ontology:cs_interpretation_layer_present('b50431cc-bcbb-4c4a-b95e-695e1557398f').
narrative_ontology:cs_reading_relation('b50431cc-bcbb-4c4a-b95e-695e1557398f', rogers_commission_findings__engineering_absolute_threshold, forecloses).
narrative_ontology:cs_reading_relation('b50431cc-bcbb-4c4a-b95e-695e1557398f', rogers_commission_findings__actuarial_risk_acceptance, coexists_with).
narrative_ontology:cs_axiom('b50431cc-bcbb-4c4a-b95e-695e1557398f', foundational, management_prerogative_over_technical_judgment).
narrative_ontology:cs_axiom_status(management_prerogative_over_technical_judgment, holdable).
narrative_ontology:cs_axiom_grounding('b50431cc-bcbb-4c4a-b95e-695e1557398f', management_prerogative_over_technical_judgment, conventional).
narrative_ontology:cs_axiom('b50431cc-bcbb-4c4a-b95e-695e1557398f', foundational, documented_awareness_sufficient_for_proceeding).
narrative_ontology:cs_axiom_status(documented_awareness_sufficient_for_proceeding, holdable).
narrative_ontology:cs_axiom_grounding('b50431cc-bcbb-4c4a-b95e-695e1557398f', documented_awareness_sufficient_for_proceeding, instrumental).
narrative_ontology:cs_reference_frame('b50431cc-bcbb-4c4a-b95e-695e1557398f', rogers_commission_imperative_cease_until_redesigned).
narrative_ontology:cs_drift_state('b50431cc-bcbb-4c4a-b95e-695e1557398f', post_1986_operational_normalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b50431cc-bcbb-4c4a-b95e-695e1557398f', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, nasa_program_management).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, contractor_operational_continuity).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, engineering_safety_authority).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, external_oversight_bodies).
narrative_ontology:constraint_vindicates(rogers_commission_findings__management_compliance_narrative, organizational_risk_tolerance_doctrine).
narrative_ontology:constraint_vindicates(rogers_commission_findings__management_compliance_narrative, management_prerogative_over_technical_dissent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and interprets the compliance standard: 'documented risk awareness and mitigation efforts sufficient to proceed.' Retains authority to launch based on whether management documentation demonstrates awareness of risks and credible mitigation claims. The rule allows continuation of operations by satisfying a procedural hurdle (documentation) rather than resolving the underlying technical question. Collects the benefit of program continuity and maintains organizational authority over launch decisions.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, nasa_program_management, agenda_setter,
    institutional, biographical, constrained, national).

% Identifies critical safety defects (O-ring erosion under cold conditions) and issues technical recommendations to halt operations until redesign. Under the compliance narrative, their authority is reduced to one input among several: a documented risk that management may choose to accept. Their professional mandate (do not operate systems known to fail catastrophically) is subordinated to a management prerogative to proceed despite documented danger. Exit is identity-locked: they cannot leave engineering roles without abandoning their professional accountability claim.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, engineering_safety_authority, payer,
    organized, biographical, identity_locked, national).

% Benefits from program continuation because halting operations for redesign would impose substantial schedule and cost penalties. The compliance narrative permits launch despite known risks if management documents awareness. They have leverage to influence what counts as 'documented risk awareness and sufficient mitigation'—a process can be procedurally complete (boxes checked) without being technically sound.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, contractor_operational_continuity, beneficiary,
    powerful, biographical, arbitrage, national).

% Congressional committees, safety review boards, and accident investigation bodies have authority to examine decisions post-hoc but are bypassed by the forward-looking compliance narrative: they see documented management rationales that satisfy the process standard, not the technical trade-offs. Their ex-post scrutiny power is weak against a pre-authorized decision frame that encoded acceptance into the launch logic.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, external_oversight_bodies, payer,
    moderate, generational, constrained, national).

% Institutional norms around safety decision-making—the understanding that 'safety is non-negotiable' or 'engineers stop operations when systems fail'—are subordinated to the compliance narrative's assertion that management risk acceptance is a legitimate decision-frame. The culture shift is not a concrete actor but an excluded voice: would object if present that proceeding despite documented defects violates the epistemic foundation of safety culture, but is not seated at the decision table.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, general_workforce_safety_culture, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__management_compliance_narrative, general_workforce_safety_culture, excluded).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__management_compliance_narrative, general_workforce_safety_culture).

% Post-accident, investigates whether decisions were defensible by the standards in force at the time. Under the compliance narrative, they examine whether documentation existed and compliance procedures were followed—not whether the decisions were technically sound. The narrative frame shifts what 'defensible' means: from 'did we avoid a known catastrophic failure' to 'did we document our awareness of it.'
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, accident_investigation_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__management_compliance_narrative, nasa_program_management).
narrative_ontology:fixing_cost_class(rogers_commission_findings__management_compliance_narrative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a decision process for managing known technical risks in large operational systems: rather than halting at every identified defect (which would paralyze operations), permit continuation if decision-makers document awareness of the risk and articulate mitigation efforts, allowing management authority to trade off safety against operational continuity with explicit rationale.
% TRANSFER_FUNCTION: Transfers veto power from technical-safety authority (engineering judgment that operations must cease until defects are fixed) to management authority (judgment that documented risk awareness permits proceeding). Also transfers accountability: from 'ensure the system is safe before launch' to 'ensure the decision to launch despite known risks was documented and rationally considered.'
% ABSENT_VOICES: Crew and mission-critical personnel whose lives depend on the system would object to proceeding despite known catastrophic-failure modes; they have no seat at the decision table and no exit—they learn of documented risks post-hoc, if at all. External safety advocates (academic researchers, retired engineers from other programs, international space agencies with different safety cultures) are structurally excluded; they would argue for the engineering-absolute-threshold reading but are not positioned to influence the compliance narrative.
% DISAPPEARANCE_RATIONALE: If the compliance narrative disappeared and the engineering-absolute-threshold reading took hold—operations cease until defects are redesigned—the schedule would compress around hardware redesign cycles instead of management documentation cycles. Resource allocation would shift from managing procedural compliance to managing technical remediation. Contractor costs would increase and timeline would extend, triggering reorganization of dependencies and stakeholder relationships across the program.
% FOUNDING_PROBLEM: Following the Rogers Commission findings on the Challenger disaster, the space program faced a choice: either halt all operations indefinitely while O-ring thermal performance was fully re-understood and certified (an open-ended pause), or establish a decision-making process that permits operations to resume under conditions of documented risk awareness and articulated mitigation—allowing management to make informed trade-offs between risk and mission criticality instead of treating any identified defect as a veto.
% FOUNDING_PROBLEM_CORROBORATION: NASA program management and contractors attest the founding problem is still live: operations cannot pause indefinitely and engineering conservatism must be balanced against mission continuity. Independent safety researchers and the eventual Space Shuttle Columbia accident investigation board (CAIB) attest the founding problem was incorrectly resolved: the compliance narrative permitted acceptance of known defects under a procedural facade, and the space shuttle program never achieved the technical redesign the engineering-absolute-threshold reading would have mandated. The Rogers Commission itself issued ambiguous findings—both readings cite the Commission's work as justification.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderately high (0.68 at interval end, rising from 0.52) because the constraint systematically privileges management authority over engineering safety judgment, permitting operations despite documented defects. Suppression is similarly high (0.71) because engineering dissent is structurally subordinated—engineers cannot veto; they can only document risks that management may choose to accept. Theater is moderate-low (0.42) but rising: the compliance process involves genuine documentation and review, but an increasing share of the documentation activity serves to theatricalize acceptance rather than resolve technical questions. Accessibility_collapse is moderate (0.58) because the compliance frame itself is the constraint—once accepted, alternatives (halt-until-redesign, independent-safety-veto) collapse because the procedural hurdle has been shifted from 'fix the defect' to 'document awareness of it.' Resistance is high (0.72) because engineering organizations, safety advocates, and eventual accident investigators all mounted active resistance to this reading, though they were structurally outweighed during the operational period. The measurement series shows monotonic extraction and theater increase over the 25-year interval: the compliance narrative normalized, veto authority stayed suppressed, and the documented-acceptance process became routine.
 *
 * PERSPECTIVAL GAP:
 *   The program management and contractor seats will compute the constraint as near-coordination: a legitimate risk decision framework that balances safety and mission. The engineering and oversight seats will compute it as near-extraction: a decision frame that substitutes procedural compliance for technical safety and systematically advantages operational continuation. The divergence is not observer-relative interpretation—it follows from structural position. Management has authority to decide; engineers have authority only to recommend. Once management sets the standard ('documented awareness is sufficient'), engineers are in the subordinated position regardless of the technical merit of their safety concerns. The engine computes this asymmetry from the power atoms and exit constraints: engineering's identity-locked exit and organized-but-not-institutional power means they cannot leave and cannot easily override; management's institutional power and constrained-but-institutional exit means they can set terms and absorb the cost of dissent.
 *
 * DIRECTIONALITY LOGIC:
 *   NASA program management is a beneficiary (d near 0.0): they collect the benefit of program continuity and retain decision authority. Contractor operational continuity is a beneficiary (d near 0.0): they avoid redesign halts and schedule impacts. Engineering safety authority is a target (d near 1.0): their technical judgment is subordinated to a management standard, their exit is identity-locked (they cannot leave engineering without abandoning their professional claim), and they bear the cost of suppressed dissent. External oversight bodies are targets (d near 0.8): they are structurally bypassed by the forward-authorized frame; they can investigate post-hoc but cannot prevent forward decisions. The compliance narrative redistributes directionality: it moves from 'engineers assess safety and decide whether to operate' (engineering near d=0.0) to 'management assesses whether documented awareness exists and decides whether to operate' (management near d=0.0, engineering near d=1.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandate tension by redefining what 'safety mandate' means. The original mandate (post-Challenger): 'ensure catastrophic failures do not recur, understand O-ring thermal behavior, redesign if necessary.' The compliance narrative's mandate: 'ensure decision-makers are aware of identified risks and have articulated mitigation efforts; proceed unless management judges risks unacceptable.' The redefined mandate is thinner and more procedurally bounded—it asks only that awareness be documented, not that defects be fixed. This redefinition is a classic mandatrophy signature: the founding problem (prevent another catastrophic failure) has not been solved by the constraint (documented awareness does not prevent failures), but the constraint has normalized and persists because it solves a different problem (permit program continuity despite unsolved technical questions). The Rogers Commission findings themselves are ambiguous—they can be read as supporting either mandate—which permits this particular reading to claim the Commission's authority while operating under a thinned mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_redefinition_structural_ambiguity,
    'Is the compliance narrative''s procedural standard (documented risk awareness and mitigation articulation) a legitimate interpretation of the Rogers Commission''s findings, or a reframing that substitutes procedural compliance for technical remediation?',
    'Textual analysis of the Rogers Commission''s actual language and intent, cross-checked against the Commission members'' post-Challenger statements and the documented decision-making process (e.g., Thiokol engineer memos, management rationales).',
    'If the Commission intended documented awareness to be sufficient, the management_compliance_narrative reading is the correct interpretation and the constraint is legitimate. If the Commission intended technical redesign to be necessary before resumption, the reading is a reframing and the constraint extracts by substituting procedural theater for safety redesign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_redefinition_structural_ambiguity, empirical, 'Whether the compliance narrative is Rogers Commission intent or a subsequent reframing.').

omega_variable(
    engineering_identity_lock_internalization,
    'Is engineering''s subordination to the compliance narrative structural (they cannot exit their professional roles and thus cannot veto management) or internalized (they come to accept that documented awareness is a legitimate decision standard)?',
    'Post-constraint departure analysis: if engineers who leave the aerospace program subsequently advocate for the engineering-absolute-threshold reading, suppression was primarily structural. If they adapt to risk-acceptance framing after departure, suppression was partially internalized.',
    'If primarily structural, the constraint''s effective suppression is stable—it persists because exit is locked. If partially internalized, the constraint carries forward through culture even after structural constraints relax, making it more persistent but also more vulnerable to cultural re-education.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(engineering_identity_lock_internalization, empirical, 'Whether engineering suppression is structural-exit-lock or internalized cultural acceptance.').

omega_variable(
    reading_contradiction_with_actuarial_reading,
    'The management_compliance_narrative reading permits launching if ''documented awareness and mitigation efforts'' exist—but does not require quantifying failure probability or obtaining informed decision-maker acceptance of a specific risk level. The actuarial_risk_acceptance reading requires both quantification and decision-maker acceptance. Are these readings coexistent or do they foreclose each other?',
    'Examine whether the compliance narrative explicitly rejects quantification/acceptance (forecloses the actuarial reading) or merely omits it (coexists). If management documentation can satisfy both standards simultaneously, they coexist; if the compliance standard explicitly permits launching with awareness but without quantified acceptance, they foreclose each other.',
    'If they coexist, the constraint space permits hybrid readings (some launches fully quantified-and-accepted, others merely documented-and-aware). If they foreclose, the organizational choice is binary—either management must quantify and get explicit acceptance, or mere documentation suffices. Binary choice would clarify authority and accountability; hybrid permits drift between standards.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contradiction_with_actuarial_reading, conceptual, 'Whether management_compliance_narrative and actuarial_risk_acceptance readings logically coexist or foreclose each other.').

omega_variable(
    documented_awareness_as_cover_story,
    'Can the compliance standard (documented awareness and articulated mitigation efforts) be satisfied by documentation that does not genuinely reflect management''s actual risk assessment—i.e., can the process be gamed by performative compliance?',
    'Comparison of documented mitigation plans with actual resource allocation, testing schedules, and redesign priority in the program''s budget and schedule. If documentation promises mitigation that never materializes, or mitigation is chronically deferred, the standard is vulnerable to theatrical compliance.',
    'If the standard can be gamed through procedural theater, extraction is higher than nominal: the constraint transfers both veto authority AND accountability, permitting management to document awareness while deferring or avoiding actual remediation. This is a piton dynamic—the theater_ratio rises and eventual failure (like Columbia) exposes the gap between documented plans and actual execution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(documented_awareness_as_cover_story, empirical, 'Whether the compliance standard is vulnerable to performative/theatrical satisfaction without genuine mitigation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__management_compliance_narrative, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(roge_tr_t0, observed).
narrative_ontology:measurement(roge_tr_t3, rogers_commission_findings__management_compliance_narrative, theater_ratio, 3, 0.28).
narrative_ontology:measurement_basis(roge_tr_t3, observed).
narrative_ontology:measurement(roge_tr_t6, rogers_commission_findings__management_compliance_narrative, theater_ratio, 6, 0.33).
narrative_ontology:measurement_basis(roge_tr_t6, observed).
narrative_ontology:measurement(roge_tr_t12, rogers_commission_findings__management_compliance_narrative, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(roge_tr_t12, observed).
narrative_ontology:measurement(roge_tr_t18, rogers_commission_findings__management_compliance_narrative, theater_ratio, 18, 0.41).
narrative_ontology:measurement_basis(roge_tr_t18, observed).
narrative_ontology:measurement(roge_tr_t25, rogers_commission_findings__management_compliance_narrative, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(roge_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(roge_be_t0, observed).
narrative_ontology:measurement(roge_be_t3, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 3, 0.58).
narrative_ontology:measurement_basis(roge_be_t3, observed).
narrative_ontology:measurement(roge_be_t6, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 6, 0.62).
narrative_ontology:measurement_basis(roge_be_t6, observed).
narrative_ontology:measurement(roge_be_t12, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(roge_be_t12, observed).
narrative_ontology:measurement(roge_be_t18, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 18, 0.67).
narrative_ontology:measurement_basis(roge_be_t18, observed).
narrative_ontology:measurement(roge_be_t25, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(roge_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(roge_su_t0, observed).
narrative_ontology:measurement(roge_su_t3, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 3, 0.62).
narrative_ontology:measurement_basis(roge_su_t3, observed).
narrative_ontology:measurement(roge_su_t6, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 6, 0.65).
narrative_ontology:measurement_basis(roge_su_t6, observed).
narrative_ontology:measurement(roge_su_t12, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 12, 0.69).
narrative_ontology:measurement_basis(roge_su_t12, observed).
narrative_ontology:measurement(roge_su_t18, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 18, 0.7).
narrative_ontology:measurement_basis(roge_su_t18, observed).
narrative_ontology:measurement(roge_su_t25, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(roge_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__management_compliance_narrative, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rogers_commission_findings__management_compliance_narrative, 0.12).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% The Rogers Commission findings instantiate three structurally distinct constraints depending on which reading is adopted. This story is the management_compliance_narrative reading—management retains launch authority if documented risk awareness exists. The sibling readings are engineering_absolute_threshold (veto in engineering hands until redesign certified) and actuarial_risk_acceptance (launch permitted if failure probability quantified and accepted). All three readings cite the same kernel (the Commission's findings) but extract different authority structures. The management_compliance_narrative reading influences both siblings by establishing the procedural norm that management participation in risk acceptance is legitimate; this downstream pressure shifted the Shuttle program's actual operations toward this reading despite the engineering-absolute-threshold reading's technical merit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rogers_commission_findings__management_compliance_narrative, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
