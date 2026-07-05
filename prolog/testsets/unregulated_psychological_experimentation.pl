% ============================================================================
% CONSTRAINT STORY: unregulated_psychological_experimentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unregulated_psychological_experimentation, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unregulated_psychological_experimentation
 *   human_readable: Unregulated Psychological Experimentation on Employees
 *   domain: organizational_psychology/epistemology
 *
 * SUMMARY:
 *   Psychological intervention research is conducted on employees by
 *   organizational leadership without IRB oversight, informed consent
 *   protocols, or professional licensing. The interventions are framed as
 *   organizational development or performance management rather than human
 *   subjects research, placing them outside the jurisdiction of research
 *   ethics review boards. Employees undergo behavioral and emotional
 *   interventions designed by untrained peers, with no right to decline
 *   without professional penalty and no adverse event reporting mechanisms.
 *   The constraint extracts psychological data and behavioral compliance from
 *   powerless employees to institutional leadership, who collect research
 *   output and career advancement while bearing none of the intervention
 *   risks.
 *
 * KEY AGENTS:
 *   - research_leadership: Primary agenda-setter (institutional/mobile) — designs interventions, collects data, advances career through research output
 *   - organizational_management: Beneficiary (institutional/mobile) — receives workforce optimization insights without ethics compliance costs
 *   - research_subjects_employees: Primary victim (powerless/identity_locked) — undergo interventions without consent, cannot decline without professional penalty
 *   - vulnerable_subordinates: Secondary victim (powerless/trapped) — experience compounded harm due to economic precarity, visa dependency, or trauma history
 *   - professional_psychology_community: Excluded (institutional/analytical) — would require IRB review, informed consent, licensed oversight
 *   - institutional_review_boards: Excluded (institutional/analytical) — jurisdiction bypassed by framing interventions as organizational development
 *   - regulatory_authorities: Observer (institutional/analytical) — lack prospective authority over psychological research methods in organizational settings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unregulated_psychological_experimentation, 0.78).
domain_priors:suppression_score(unregulated_psychological_experimentation, 0.81).
domain_priors:theater_ratio(unregulated_psychological_experimentation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unregulated_psychological_experimentation, extractiveness, 0.78).
narrative_ontology:constraint_metric(unregulated_psychological_experimentation, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(unregulated_psychological_experimentation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unregulated_psychological_experimentation, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(unregulated_psychological_experimentation, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unregulated_psychological_experimentation, snare).
narrative_ontology:human_readable(unregulated_psychological_experimentation, "Unregulated Psychological Experimentation on Employees").
narrative_ontology:topic_domain(unregulated_psychological_experimentation, "organizational_psychology/epistemology").

domain_priors:requires_active_enforcement(unregulated_psychological_experimentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unregulated_psychological_experimentation, research_leadership).
narrative_ontology:constraint_beneficiary(unregulated_psychological_experimentation, organizational_management).
narrative_ontology:constraint_victim(unregulated_psychological_experimentation, research_subjects_employees).
narrative_ontology:constraint_victim(unregulated_psychological_experimentation, vulnerable_subordinates).
narrative_ontology:constraint_vindicates(unregulated_psychological_experimentation, organizational_learning_imperative).
narrative_ontology:constraint_vindicates(unregulated_psychological_experimentation, innovation_requires_experimentation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers psychological interventions on employees without IRB oversight or professional licensing. Frames the work as organizational development or performance optimization rather than human subjects research. Collects data on behavioral and emotional responses, publishes findings internally or externally, and advances career standing through the research output. Can terminate the research at will and faces no external review of methods or participant welfare.
narrative_ontology:constraint_stakeholder(unregulated_psychological_experimentation, research_leadership, agenda_setter,
    institutional, biographical, mobile, local).

% Benefits from behavioral data and intervention outcomes without bearing research ethics compliance costs. The unregulated structure allows rapid iteration on workforce optimization techniques that would require months of IRB review and informed consent protocols if conducted as formal research. Management receives actionable insights while the research leadership absorbs any reputational risk from adverse outcomes.
narrative_ontology:constraint_stakeholder(unregulated_psychological_experimentation, organizational_management, beneficiary,
    institutional, biographical, mobile, local).

% Undergo psychological interventions designed and administered by untrained peers without informed consent, right to withdraw, or adverse event reporting mechanisms. The interventions are presented as mandatory professional development or performance management rather than research participation. Declining participation or raising concerns about methods risks being labeled uncooperative or resistant to organizational culture. The identity lock operates through professional identity fusion: their self-concept as a competent team member depends on participating in what leadership frames as growth opportunities, making refusal psychologically costly even when no formal penalty exists.
narrative_ontology:constraint_stakeholder(unregulated_psychological_experimentation, research_subjects_employees, payer,
    powerless, immediate, identity_locked, local).

% Experience the same interventions as other employees but with compounded vulnerability: economic precarity, visa status dependency, caregiving responsibilities, or prior trauma history that makes psychological manipulation more harmful. They cannot decline without risking employment, have no access to independent mental health support to process intervention effects, and carry the psychological costs long after the research concludes. Their trapped status is structural: exit means loss of income, healthcare, or immigration status.
narrative_ontology:constraint_stakeholder(unregulated_psychological_experimentation, vulnerable_subordinates, payer,
    powerless, immediate, trapped, local).

% Would object that the interventions constitute human subjects research requiring IRB approval, informed consent, licensed practitioner oversight, and adverse event monitoring under professional ethics codes and federal regulations. They are structurally excluded because the research is conducted outside academic or clinical institutions where their authority applies, and organizational settings are not required to submit to external review boards for internal development activities.
narrative_ontology:constraint_stakeholder(unregulated_psychological_experimentation, professional_psychology_community, excluded,
    institutional, generational, analytical, national).

% Federal regulations require IRB review for research involving human subjects, but the definition of research excludes quality improvement and organizational development activities. The constraint exploits this boundary: by framing psychological interventions as performance management rather than research, leadership places the work outside IRB jurisdiction. IRBs would require informed consent, right to withdraw, risk assessment, and ongoing monitoring if the same interventions were submitted as a research protocol.
narrative_ontology:constraint_stakeholder(unregulated_psychological_experimentation, institutional_review_boards, excluded,
    institutional, generational, analytical, national).

% Occupational safety regulators and labor standards agencies have jurisdiction over workplace conditions but lack specific authority over psychological research methods. They investigate only after documented harm, and the diffuse nature of psychological injury makes adverse outcomes difficult to attribute to specific interventions. The regulatory gap allows the constraint to persist: no agency has clear mandate to review methods prospectively.
narrative_ontology:constraint_stakeholder(unregulated_psychological_experimentation, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuine. The claimed coordination function is organizational learning and performance optimization, but this does not require unregulated experimentation on employees — the same learning could occur through voluntary participation with informed consent, external IRB review, and licensed practitioner oversight.
% TRANSFER_FUNCTION: Extracts psychological data, behavioral compliance, and emotional labor from employees to research leadership and organizational management. Employees bear the risks of untested interventions, potential psychological harm, and loss of autonomy; leadership collects research output, career advancement, and actionable workforce optimization insights.
% ABSENT_VOICES: Professional psychology community and IRBs are structurally excluded. They would require informed consent, right to withdraw, risk assessment, licensed oversight, and adverse event reporting. Their exclusion is maintained by framing the interventions as organizational development rather than research, placing the work outside their jurisdictional reach.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight and all psychological interventions on employees required IRB approval and informed consent, research leadership would either submit protocols for external review (substantially slowing iteration and requiring methodological rigor) or cease the interventions entirely. Employees would regain the right to decline participation without professional penalty. The organizational learning model would shift from captive experimentation to voluntary participation or external research partnerships.
% FOUNDING_PROBLEM: Organizations need to adapt and optimize workforce performance in rapidly changing environments, and traditional academic research timelines are too slow for operational decision-making.
% FOUNDING_PROBLEM_CORROBORATION: Research leadership and organizational management attest the problem is live and that rapid iteration requires bypassing formal research ethics review. Professional psychology community, IRBs, and labor advocates attest that the speed imperative does not justify eliminating informed consent and participant protections — that the founding problem is real but the constraint is not the least-harmful solution. Independent ethics scholarship and regulatory testimony from outside the benefiting organizations support the alternative-solutions-exist reading.
narrative_ontology:disappearance_verdict(unregulated_psychological_experimentation, world_rearranges).
narrative_ontology:founding_problem_status(unregulated_psychological_experimentation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unregulated_psychological_experimentation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(unregulated_psychological_experimentation, 'none', 1).
narrative_ontology:epsilon_provenance(unregulated_psychological_experimentation, 0.78, 'claude-sonnet-4-5-20250929', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unregulated_psychological_experimentation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unregulated_psychological_experimentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unregulated_psychological_experimentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78) because employees bear psychological risks, loss of autonomy, and potential harm from untested interventions while leadership collects research output and career advancement with no reciprocal cost. Suppression is higher still (0.81) because the constraint's persistence depends on actively preventing employees from declining participation: the interventions are presented as mandatory professional development, and raising concerns risks being labeled uncooperative. Theater ratio is moderate (0.42): some genuine organizational learning occurs, but a growing share of activity is performative framing of research as development to avoid ethics review. Accessibility collapse is moderate-low (0.48): alternatives exist (voluntary participation with informed consent, external research partnerships, licensed practitioner oversight) but are actively suppressed by the framing that speed requires bypassing protections. Resistance is substantial (0.67): employees object when they recognize the interventions as research rather than development, and external professional communities challenge the ethics, but the structural exclusion of review authority prevents the resistance from stopping the practice.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute radically differently. From research leadership's position, the arrangement is legitimate organizational learning that would be slowed by unnecessary bureaucratic review. From the powerless employee seats, the same structure operates as coerced participation in psychological experimentation without informed consent or right to withdraw. The engine computes this divergence from the structural data: institutional actors with mobile exit options who collect research output versus powerless actors with identity-locked or trapped exit who bear intervention risks. The claimed type (snare) reflects the victim seats' structural experience; the metrics describe the extractive, suppressive operation that maintains the arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Research leadership is the primary beneficiary and agenda-setter: they design the interventions, control participation, collect the data, and advance careers through the research output. Their directionality is near the beneficiary end (d ≈ 0.1-0.15). Organizational management is a secondary beneficiary: they receive actionable insights without bearing compliance costs, placing them also near the beneficiary end (d ≈ 0.2). Research subjects employees are the primary targets: they undergo interventions without consent, bear psychological risks, and cannot decline without professional penalty. Their identity-locked exit (professional identity fused with participation) places them near the full-target end (d ≈ 0.85-0.9). Vulnerable subordinates are secondary targets with trapped exit (economic or visa dependency), placing them at the full-target end (d ≈ 0.95-1.0). Professional psychology community and IRBs are excluded rather than coordinated — their exclusion is the enforcement object itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mislabeled coordination. The claimed coordination function (organizational learning and performance optimization) does not require unregulated experimentation on captive employees. The same learning could occur through voluntary participation with informed consent, external IRB review, and licensed practitioner oversight. The speed imperative (rapid iteration for operational decision-making) is real, but it does not justify eliminating participant protections — it is a preference for efficiency over ethics, not a structural necessity. The constraint persists because it extracts value (research output, behavioral data, career advancement) from employees who cannot decline without professional penalty, not because it solves a coordination problem that has no less-harmful solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    research_vs_development_boundary,
    'Is the distinction between human subjects research (requiring IRB review) and organizational development (exempt from review) a principled boundary or a loophole that allows psychological experimentation to evade ethics oversight?',
    'Regulatory clarification defining when psychological interventions on employees constitute research regardless of organizational framing, or case law establishing that informed consent and right to withdraw are required for any systematic behavioral intervention collecting data.',
    'If the boundary is a loophole, closing it would require IRB review for all psychological interventions on employees, eliminating the constraint''s jurisdictional bypass. If the boundary is principled, the constraint would remain outside ethics review authority and persist as organizational prerogative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(research_vs_development_boundary, conceptual, 'Whether organizational development framing legitimately exempts psychological interventions from research ethics review.').

omega_variable(
    identity_lock_mechanism,
    'Is the identity lock binding employees to participation structural (professional penalties for declining) or internalized (employees believe they should participate because good team members embrace growth opportunities)?',
    'Post-exit interviews with former employees who declined participation: if suppression persists after leaving the organization (ongoing belief they were wrong to decline), the lock is partially internalized. If suppression disappears after exit (recognition they were coerced), the lock is structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — employees carry the suppression with them after exit and may replicate the pattern in future organizations. If structural only, exit breaks the constraint''s hold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether suppression is structural or internalized in identity-locked employees.').

omega_variable(
    harm_attribution_gap,
    'Can adverse psychological outcomes from unregulated interventions be reliably attributed to the interventions, or does the diffuse nature of psychological injury create an attribution gap that prevents accountability?',
    'Longitudinal studies comparing psychological outcomes for employees who underwent interventions versus matched controls who did not, or regulatory investigation with subpoena power to access intervention protocols and participant outcomes.',
    'If attribution is reliable, documented harm would trigger regulatory intervention and liability exposure, creating external pressure to adopt informed consent and adverse event reporting. If attribution is structurally difficult, the constraint persists because harm cannot be proven even when it occurs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_attribution_gap, empirical, 'Whether psychological harm from interventions can be attributed reliably enough to trigger accountability.').

omega_variable(
    professional_licensing_necessity,
    'Is professional licensing and training necessary to conduct psychological interventions safely, or can untrained peers administer interventions without elevated harm risk if they follow protocols?',
    'Comparative outcome studies of interventions administered by licensed psychologists versus untrained organizational leaders, controlling for intervention type and participant characteristics.',
    'If licensing is necessary for safety, the constraint''s operation without licensed oversight constitutes negligent harm and would support mandatory credentialing requirements. If protocols alone suffice, the licensing requirement is professional gatekeeping rather than participant protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(professional_licensing_necessity, empirical, 'Whether professional training is necessary to administer psychological interventions safely.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unregulated_psychological_experimentation, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unre_tr_t0, unregulated_psychological_experimentation, theater_ratio, 0, 0.28).
narrative_ontology:measurement(unre_tr_t4, unregulated_psychological_experimentation, theater_ratio, 4, 0.32).
narrative_ontology:measurement(unre_tr_t8, unregulated_psychological_experimentation, theater_ratio, 8, 0.35).
narrative_ontology:measurement(unre_tr_t12, unregulated_psychological_experimentation, theater_ratio, 12, 0.38).
narrative_ontology:measurement(unre_tr_t16, unregulated_psychological_experimentation, theater_ratio, 16, 0.4).
narrative_ontology:measurement(unre_tr_t20, unregulated_psychological_experimentation, theater_ratio, 20, 0.41).
narrative_ontology:measurement(unre_tr_t24, unregulated_psychological_experimentation, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(unre_be_t0, unregulated_psychological_experimentation, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(unre_be_t4, unregulated_psychological_experimentation, base_extractiveness, 4, 0.67).
narrative_ontology:measurement(unre_be_t8, unregulated_psychological_experimentation, base_extractiveness, 8, 0.71).
narrative_ontology:measurement(unre_be_t12, unregulated_psychological_experimentation, base_extractiveness, 12, 0.74).
narrative_ontology:measurement(unre_be_t16, unregulated_psychological_experimentation, base_extractiveness, 16, 0.76).
narrative_ontology:measurement(unre_be_t20, unregulated_psychological_experimentation, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(unre_be_t24, unregulated_psychological_experimentation, base_extractiveness, 24, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(unre_su_t0, unregulated_psychological_experimentation, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(unre_su_t4, unregulated_psychological_experimentation, suppression_requirement, 4, 0.72).
narrative_ontology:measurement(unre_su_t8, unregulated_psychological_experimentation, suppression_requirement, 8, 0.75).
narrative_ontology:measurement(unre_su_t12, unregulated_psychological_experimentation, suppression_requirement, 12, 0.77).
narrative_ontology:measurement(unre_su_t16, unregulated_psychological_experimentation, suppression_requirement, 16, 0.79).
narrative_ontology:measurement(unre_su_t20, unregulated_psychological_experimentation, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(unre_su_t24, unregulated_psychological_experimentation, suppression_requirement, 24, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unregulated_psychological_experimentation, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
