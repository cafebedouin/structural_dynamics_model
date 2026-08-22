% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Non-Derogable Absolute Prohibition on Torture and Degrading Treatment
 *   domain: international_humanitarian_law/human_rights
 *
 * SUMMARY:
 *   Common Article 3 of the Geneva Conventions establishes that torture and
 *   degrading treatment are absolutely prohibited in all circumstances — not
 *   subject to security exception, operational necessity, or threat level.
 *   This constraint instantiates the absolute_prohibition reading of the
 *   humane_treatment_standard kernel, asserting categorical non-derogability:
 *   detainees retain full rights-holder status regardless of their custody
 *   status or security risk. The constraint's claimed type is mountain
 *   (appearing as fundamental natural law, emerging from human dignity
 *   itself), yet it declares beneficiaries (detainees), creating a FSM
 *   candidate: do powerless detainees benefit from a natural law, or do
 *   institutional actors benefit from maintaining the fiction of absolute
 *   protection while practice remains systematically violated? The
 *   measurement series tracks extractiveness and theater_ratio to detect
 *   whether the constraint's performance is mounting (rising theater
 *   indicating growing cover-story function) or stable. The competing
 *   readings (contextual_necessity asserting security can override,
 *   proportionality_balancing asserting some degrading treatment is
 *   proportionally justified) are excluded from this reading's interpretation
 *   but institutionally active in state practice.
 *
 * KEY AGENTS:
 *   - detainees_and_prisoners: powerless, trapped — protected persons in custody, structurally dependent on the constraint's enforcement for protection
 *   - state_security_apparatus: institutional, constrained — intelligence and military interrogation operations, bearing the cost of absolute method restriction
 *   - state_political_authority: institutional, mobile — formal treaty ratifier, sets the legal framework but operationally distant from interrogation enforcement
 *   - international_humanitarian_law_institutions: organized, analytical — treaty bodies, courts, and humanitarian organizations that advance and monitor the absolute reading
 *   - competing_security_frameworks: institutional, trapped — excluded doctrines asserting security exception, institutionally held by some state actors despite formal prohibition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.18).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.22).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.18).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, mountain).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Non-Derogable Absolute Prohibition on Torture and Degrading Treatment").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_humanitarian_law/human_rights").

domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, '58ae876a-f9d6-40a5-b13e-dd77153e9b09').
narrative_ontology:cs_kernel_codification('58ae876a-f9d6-40a5-b13e-dd77153e9b09', formalized).
narrative_ontology:cs_authority_grounding('58ae876a-f9d6-40a5-b13e-dd77153e9b09', lineage).
narrative_ontology:cs_interpretation_layer_present('58ae876a-f9d6-40a5-b13e-dd77153e9b09').
narrative_ontology:cs_reading_relation('58ae876a-f9d6-40a5-b13e-dd77153e9b09', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('58ae876a-f9d6-40a5-b13e-dd77153e9b09', humane_treatment_standard__proportionality_balancing, influences).
narrative_ontology:cs_axiom('58ae876a-f9d6-40a5-b13e-dd77153e9b09', foundational, humane_treatment_categorically_non_derogable).
narrative_ontology:cs_axiom_status(humane_treatment_categorically_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('58ae876a-f9d6-40a5-b13e-dd77153e9b09', humane_treatment_categorically_non_derogable, deontological).
narrative_ontology:cs_axiom('58ae876a-f9d6-40a5-b13e-dd77153e9b09', foundational, detainee_status_preserves_rights_holder_standing).
narrative_ontology:cs_axiom_status(detainee_status_preserves_rights_holder_standing, holdable).
narrative_ontology:cs_axiom_grounding('58ae876a-f9d6-40a5-b13e-dd77153e9b09', detainee_status_preserves_rights_holder_standing, deontological).
narrative_ontology:cs_reference_frame('58ae876a-f9d6-40a5-b13e-dd77153e9b09', categorical_protection_absolute_floor).
narrative_ontology:cs_drift_state('58ae876a-f9d6-40a5-b13e-dd77153e9b09', contemporary_armed_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('58ae876a-f9d6-40a5-b13e-dd77153e9b09', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detainees_and_prisoners).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, protected_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, state_security_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons in state custody or detention. The constraint establishes that their status as captive does not erase their entitlement to freedom from torture and degrading treatment, regardless of the state's security interests or interrogation objectives. Exit is physically blocked; the constraint is their sole protection against abuse.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detainees_and_prisoners, beneficiary,
    powerless, biographical, trapped, universal).

% Military, intelligence, and law enforcement agencies conducting interrogation and detention operations. The constraint absolutely forbids certain interrogation methods (torture, degrading treatment) regardless of security threat severity, time pressure, or tactical advantage. The cost is borne in intelligence yield constraints and operational method limitations.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, state_security_apparatus, payer,
    institutional, generational, constrained, national).

% The formal authority that ratifies Common Article 3 and bears international legal responsibility for its implementation. Sets the legal framework that binds the security apparatus; can interpret, enforce, or violate the standard, but cannot legally derogate it under any circumstance.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, state_political_authority, agenda_setter,
    institutional, generational, mobile, national).

% Red Cross/Red Crescent movement, treaty bodies, international courts, academic community. Monitor compliance, interpret the standard, and advance the reading that humane treatment is a categorical right, not a context-dependent policy variable.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_humanitarian_law_institutions, observer,
    organized, generational, analytical, universal).

% Doctrines asserting that security imperatives can override humane treatment (contextual_necessity reading) or that proportionality permits some degrading treatment when balanced against threat (proportionality_balancing reading). These readings are formally excluded from this constraint's interpretation, though institutionally championed by some state actors and security theorists.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, competing_security_frameworks, excluded,
    institutional, generational, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__absolute_prohibition, diffuse).
narrative_ontology:fixing_cost_class(humane_treatment_standard__absolute_prohibition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, non-derogable minimum standard for all parties to armed conflict and detaining powers: detainees retain categorical dignity and protection from torture/degrading treatment regardless of status, threat level, or interrogation objective. Solves the collective-action problem of unilateral race-to-the-bottom in detention practices by binding all parties to a fixed floor, not sliding negotiated terms.
% TRANSFER_FUNCTION: Transfers the cost of compliance from detainees (who would otherwise bear torture/abuse risk) to state security apparatuses (who must conduct interrogation within the prohibited-method boundary). No material or status transfer occurs; the transfer is of protective obligation.
% ABSENT_VOICES: States that classify detainees as non-persons or security objects rather than rights-holders; security agencies that advocate for contextual exception clauses; framings that treat humane treatment as a privilege earned by cooperation rather than a right held by status.
% DISAPPEARANCE_RATIONALE: If the absolute prohibition vanished, state detaining practices would immediately stratify by regime intensity and interrogation tolerance; torture would re-emerge as a normalized interrogation tool in conflict zones; detainees would lose their sole categorical protection and fall into regime-specific (or non-existent) safeguards. The humanitarian order depends structurally on this non-derogable floor.
% FOUNDING_PROBLEM: Detention and interrogation without humane constraint enabled systematic torture and disappearance during and after armed conflict; captive populations had no legal recourse or protection. Common Article 3 was established to create an absolute floor that no party could legally breach, eliminating the excuse that security necessity justifies torture.
% FOUNDING_PROBLEM_CORROBORATION: Independent human rights organizations document ongoing torture violations and detainee abuse in contemporary conflicts; international courts and treaty bodies repeatedly affirm that the founding problem (systematic torture under security cover) remains an active threat requiring the absolute prohibition. Detaining states that violate the standard do not dispute the prohibition's existence — they interpret it more narrowly or claim factual exemption, but cannot openly assert that the founding problem no longer justifies the rule.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__absolute_prohibition, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, ExtMetricName, E),
    domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint appears to operate without a concentrated beneficiary capturing rents — the benefit (detainee protection) goes to powerless captives, not to an organized actor wielding the constraint for institutional gain. Suppression is similarly low (0.22) because the constraint persists not through active coercion of detainees but through formal international legal commitment and monitoring structures. Theater_ratio is very low (0.08) because the constraint's enforcement is primarily functional (actual legal proceedings, documented compliance reviews) rather than performative — where violations occur, they are covered by interpretive narrow-readings ('enhanced interrogation is not torture'), not by theatrical displays of commitment. Accessibility_collapse is very high (0.92) because once the constraint's non-derogability is understood, no legal or security alternative appears available to the detaining power without formally violating international law. Resistance is high (0.71) because states regularly encounter resistance from security agencies and threat perceptions that push against the absolute standard, generating ongoing institutional friction. The low extractiveness and suppression profile support the mountain claim; the beneficiary declaration and FSM omega challenge whether the constraint is a genuine natural law or an institutional protection framework maintaining the appearance of absolute safety while practice remains contested.
 *
 * PERSPECTIVAL GAP:
 *   From the detainee's perspective trapped in custody, the constraint is a categorical legal entitlement that should shield them from abuse — world_arranged so that no security exception permits torture. From the state security apparatus's perspective, the constraint imposes an operational cost: interrogation methods are constrained regardless of threat severity, time pressure, or intelligence value. From the international institutional perspective, the constraint is a fundamental norm that has survived (barely) institutional commitment despite repeated violations. The engine computes per-seat classification from these divergent structural positions: detainees (powerless, trapped) see a protective law; security agencies (institutional, constrained exit) see an operational limit; institutional observers (analytical) see a contested norm. The 0.18 extractiveness reflects that the arrangement is not structured to transfer value to a dominant beneficiary — but the presence of beneficiaries declared (detainees) triggers FSM evaluation: is this a mountain maintained by its own force, or a constructed legal instrument that benefits powerless detainees while institutional actors manage violation through narrow interpretation?
 *
 * DIRECTIONALITY LOGIC:
 *   Detainees and protected persons are declared beneficiaries (d near 0.0 — the constraint subsidizes them with absolute protection). State security apparatus is declared payer (d near 1.0 — the constraint extracts operational method constraints from them). State political authority sits near symmetric (d ~ 0.5): it benefits from international legitimacy and normalized relations that flow from formal commitment, but bears diplomatic cost when violations are exposed. International humanitarian law institutions are observers (analytical seat, d not applicable to directionality computation). Competing security frameworks are excluded (they would argue for higher d from security agencies, but are structurally locked out of this reading's interpretation). The declared beneficiaries are powerless and trapped (lowest exit_options, no arbitrage), which amplifies their directionality-to-extraction transduction in the engine's computation: beneficiaries with no exit get the full subsidy (low effective extraction), while payers with constrained exit bear the full cost. This structural asymmetry is intentional: the constraint is designed to protect the powerless by binding the powerful.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (systematic torture under security cover) is live: contemporary conflicts document ongoing detainee abuse despite the absolute prohibition. The disappearance verdict is world_rearranges: detention practices would immediately regress to regime-specific or non-existent safeguards if the constraint disappeared. This alignment (live problem, world-dependent outcome) indicates the constraint remains functionally necessary and is not mandatrophic (the founding problem has not been superseded). However, the FSM omega raises a critical ambiguity: institutional data (international court cases, compliance reviews, state policy audits) should reveal whether the constraint operates as genuine protection (mandatory enforcement of non-derogability) or as managed theater (violations covered by narrow interpretation while formal commitment persists). If violations are widespread and enforcement is weak, the constraint becomes a mandatrophic false summit: the founding problem persists but the institutional response has devolved into performative commitment rather than functional protection. The measurements track theater_ratio (low but non-zero, suggesting some performative component) and resistance (high, indicating ongoing institutional friction against the absolute reading). This profile suggests the constraint is not yet fully mandatrophic but carries the risk of sliding toward institutional theater if enforcement continues to weaken relative to violations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_protection,
    'Is the absolute prohibition on torture a fundamental law of human dignity that emerges naturally from rational ethics, or is it a constructed legal instrument designed to protect powerless detainees from abuse by states that have identified beneficiaries (security apparatus) and victims (detainees)?',
    'Examine whether the constraint persists because it reflects an unbreakable structural reality (natural law candidate) or because institutional commitment to the absolute reading prevents reversion to contextual exception even when security pressure mounts (constructed legal instrument). Track: do states that violate the standard claim the standard is wrong, or claim factual exemption while affirming the standard''s legitimacy?',
    'If the constraint is truly natural law, it should show near-zero beneficiary capture and persist across institutional variation. If it is a constructed protection framework, beneficiary presence (detainees/protected persons) and asymmetric enforcement (security apparatus bears higher compliance cost) should be evident, triggering FSM evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_protection, conceptual, 'Whether humane treatment is an inherent natural law or an institutional construction protecting powerless detainees from state abuse.').

omega_variable(
    security_exception_foreclosure,
    'Do the absolute prohibition reading and the contextual_necessity reading (which asserts security can override humane treatment) logically foreclose each other, or can they coexist as different institutional positions held by different state actors?',
    'Test whether a single state can hold both readings simultaneously in policy (e.g., official law states absolute prohibition while operational doctrine permits enhanced interrogation under threat conditions). If simultaneous holding is structurally possible, they coexist; if internal contradiction forces choice, they foreclose.',
    'If forecloses: this reading''s axiom (humane_treatment_categorically_non_derogable) logically eliminates the security-exception reading as incoherent. If coexists_with: both readings persist as institutional positions with different constituencies, and the enforcement gap between law and practice becomes the measurable conflict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_exception_foreclosure, conceptual, 'Whether absolute prohibition and contextual security exception logically foreclose each other or coexist as institutional positions.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.22) primarily structural (external enforcement by international monitoring bodies, domestic legal systems) or internalized (detainees have absorbed the belief that they deserve harsh treatment, or internalized the powerlessness that makes resistance appear futile)?',
    'Post-release narrative analysis: do released detainees report understanding their rights during detention, or internalized narratives of deserved punishment? Interviews with survivors of legal vs. illegal detention regimes: does the absolute prohibition framework change detainees'' own sense of entitlement to protection?',
    'If suppression is primarily structural, removing the constraints (state violation) would expose the underlying entitlement claim and likely generate resistance. If partially internalized, detainees may not claim their rights even after release, and the constraint''s protective effect is weaker than the structural enforcement suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of detainee resistance is structural or partially internalized.').

omega_variable(
    false_summit_detainee_benefit,
    'Do detainees genuinely benefit from the absolute prohibition, or is the constraint maintained by institutional actors (IHL institutions, state legal systems) that benefit from the rules framework while detainees remain systematically violated?',
    'Compare violation rates and enforcement action rates: do states that ratify Common Article 3 show lower torture incidence than pre-ratification baselines? Do international courts and treaty bodies impose consequences that deter violations, or remain performative? Are detainees'' own accounts of protection-in-practice aligned with the constraint''s stated non-derogability?',
    'If genuine benefit is distributed to detainees, the beneficiary declaration is accurate. If violations persist despite the constraint and no meaningful enforcement follows, the constraint becomes a false summit: maintained by institutional actors who benefit from the appearance of absolute protection while actual abuse continues under interpretive cover (e.g., ''enhanced interrogation is not torture'').',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_detainee_benefit, empirical, 'Whether the absolute prohibition materially protects detainees or functions as institutional theater masking systemic violation.').

omega_variable(
    contextual_reading_kernel_ambiguity,
    'This constraint instantiates the absolute_prohibition reading of the humane_treatment_standard kernel. The sibling contextual_necessity reading asserts that Common Article 3 ''sets baseline but permits enhanced interrogation when national security imperatives override.'' Is this sibling reading present within institutional frameworks (state doctrine, security policy) even while this absolute_prohibition reading holds formal legal status?',
    'Institutional document analysis: audit official state policy, court decisions, military doctrine, and security agency guidance for explicit or implicit acknowledgment of security-exception language. Map the institutional split: where does absolute_prohibition hold and where does contextual_necessity operate (same state, different branches or operational levels)?',
    'If contextual_necessity is actively held within institutional practice despite absolute_prohibition being the formal law, the enforcement gap becomes the real constraint — states manage compliance by narrow definition (''our methods are not torture'') rather than by genuine adherence to non-derogability. This elevates theater_ratio and reveals the constraint as managed violation, not universal protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contextual_reading_kernel_ambiguity, empirical, 'Whether the contextual-necessity competing reading is institutionally active despite absolute-prohibition''s formal legal status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__absolute_prohibition, theater_ratio, 0, 0.06).
narrative_ontology:measurement(huma_tr_t10, humane_treatment_standard__absolute_prohibition, theater_ratio, 10, 0.07).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__absolute_prohibition, theater_ratio, 20, 0.08).
narrative_ontology:measurement(huma_tr_t30, humane_treatment_standard__absolute_prohibition, theater_ratio, 30, 0.09).
narrative_ontology:measurement(huma_tr_t40, humane_treatment_standard__absolute_prohibition, theater_ratio, 40, 0.08).
narrative_ontology:measurement(huma_tr_t50, humane_treatment_standard__absolute_prohibition, theater_ratio, 50, 0.08).
narrative_ontology:measurement(huma_tr_t60, humane_treatment_standard__absolute_prohibition, theater_ratio, 60, 0.08).
narrative_ontology:measurement(huma_tr_t75, humane_treatment_standard__absolute_prohibition, theater_ratio, 75, 0.08).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__absolute_prohibition, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__absolute_prohibition, base_extractiveness, 10, 0.17).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__absolute_prohibition, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(huma_be_t30, humane_treatment_standard__absolute_prohibition, base_extractiveness, 30, 0.19).
narrative_ontology:measurement(huma_be_t40, humane_treatment_standard__absolute_prohibition, base_extractiveness, 40, 0.18).
narrative_ontology:measurement(huma_be_t50, humane_treatment_standard__absolute_prohibition, base_extractiveness, 50, 0.19).
narrative_ontology:measurement(huma_be_t60, humane_treatment_standard__absolute_prohibition, base_extractiveness, 60, 0.18).
narrative_ontology:measurement(huma_be_t75, humane_treatment_standard__absolute_prohibition, base_extractiveness, 75, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__absolute_prohibition, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__absolute_prohibition, suppression_requirement, 10, 0.21).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__absolute_prohibition, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(huma_su_t30, humane_treatment_standard__absolute_prohibition, suppression_requirement, 30, 0.23).
narrative_ontology:measurement(huma_su_t40, humane_treatment_standard__absolute_prohibition, suppression_requirement, 40, 0.22).
narrative_ontology:measurement(huma_su_t50, humane_treatment_standard__absolute_prohibition, suppression_requirement, 50, 0.22).
narrative_ontology:measurement(huma_su_t60, humane_treatment_standard__absolute_prohibition, suppression_requirement, 60, 0.22).
narrative_ontology:measurement(huma_su_t75, humane_treatment_standard__absolute_prohibition, suppression_requirement, 75, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(humane_treatment_standard__absolute_prohibition, 0.12).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% The humane_treatment_standard kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading of Common Article 3. absolute_prohibition (this story) asserts categorical non-derogability; contextual_necessity asserts security can override; proportionality_balancing asserts balancing is required. The three readings have different ε values (this reading is low-extraction, appearing mountain-like; contextual_necessity would be higher-extraction, permitting security exception; proportionality_balancing is intermediate). They are linked by affects_constraints to indicate they are sibling readings of the same kernel, not separate independent constraints. The network also enables contention detection: where two readings coexist institutionally in the same state or body, the engine detects the contradiction and flags the enforcement gap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
