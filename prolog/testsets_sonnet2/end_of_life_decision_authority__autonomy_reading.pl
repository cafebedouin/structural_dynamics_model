% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__autonomy_reading, []).

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
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: Prohibition/Gatekeeping of Medical Aid in Dying (Autonomy Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This story instantiates the autonomy reading of the end-of-life decision
 *   authority kernel: competent individuals possess sovereign authority over
 *   the timing and manner of their own death, and the constraint under
 *   examination is the prohibition/gatekeeping regime that denies or heavily
 *   restricts this authority through medical licensing, criminal law, and
 *   institutional risk aversion. Under this reading, patients enduring
 *   prolonged, unwanted suffering are the victim class, physicians willing to
 *   honor patient requests are recast as facilitators criminalized or
 *   professionally endangered for doing so, and the slippery-slope coercion
 *   risk emphasized by the vulnerability-protection reading is treated as an
 *   externalized policy-design concern rather than grounds for denying the
 *   underlying sovereignty claim. The referent for extractiveness is the
 *   standing prohibition/gatekeeping arrangement as it operates today, not
 *   the permissive regime this reading would install.
 *
 * KEY AGENTS:
 *   - terminally_ill_patients_denied_access: primary target (powerless/trapped) — bears extraction through prolonged suffering and foreclosed choice
 *   - physicians_willing_to_assist: facilitator-agent under professional and legal jeopardy (moderate/constrained)
 *   - medical_licensing_boards: primary institutional beneficiary (institutional/arbitrage) — retains gatekeeping discretion
 *   - religious_institutional_authorities: normative beneficiary (institutional/arbitrage) — projects doctrinal commitments into secular policy
 *   - bioethics_scholars_and_courts: analytical observer — adjudicates the competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.62).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.71).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "Prohibition/Gatekeeping of Medical Aid in Dying (Autonomy Reading)").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, 'acd44bda-7932-4e0d-83de-2a9efe70f2f6').
narrative_ontology:cs_kernel_codification('acd44bda-7932-4e0d-83de-2a9efe70f2f6', distributed).
narrative_ontology:cs_authority_grounding('acd44bda-7932-4e0d-83de-2a9efe70f2f6', distributed).
narrative_ontology:cs_reading_relation('acd44bda-7932-4e0d-83de-2a9efe70f2f6', end_of_life_decision_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('acd44bda-7932-4e0d-83de-2a9efe70f2f6', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('acd44bda-7932-4e0d-83de-2a9efe70f2f6', foundational, individual_sovereignty_over_death_timing).
narrative_ontology:cs_axiom_status(individual_sovereignty_over_death_timing, holdable).
narrative_ontology:cs_axiom_grounding('acd44bda-7932-4e0d-83de-2a9efe70f2f6', individual_sovereignty_over_death_timing, deontological).
narrative_ontology:cs_axiom('acd44bda-7932-4e0d-83de-2a9efe70f2f6', secondary, physician_role_is_facilitative_not_independently_evaluative).
narrative_ontology:cs_axiom_status(physician_role_is_facilitative_not_independently_evaluative, holdable).
narrative_ontology:cs_axiom_grounding('acd44bda-7932-4e0d-83de-2a9efe70f2f6', physician_role_is_facilitative_not_independently_evaluative, conventional).
narrative_ontology:cs_reference_frame('acd44bda-7932-4e0d-83de-2a9efe70f2f6', common_law_bodily_autonomy_doctrine).
narrative_ontology:cs_drift_state('acd44bda-7932-4e0d-83de-2a9efe70f2f6', post_assisted_dying_legalization_wave, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('acd44bda-7932-4e0d-83de-2a9efe70f2f6', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, medical_licensing_boards).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, religious_institutional_authorities).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, risk_averse_health_systems).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, terminally_ill_patients_denied_access).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, patients_with_prolonged_suffering).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, families_bearing_prolonged_dying_costs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, physicians_willing_to_assist).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__autonomy_reading, state_interest_in_preserving_life_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Competent, terminally diagnosed individuals who wish to end unbearable suffering on their own timeline but face jurisdictions or institutional policies that prohibit or heavily restrict medical aid in dying. Their exit options are limited to enduring the disease course, self-directed and often violent or unreliable means, or traveling to a permissive jurisdiction if resources and health allow — an option foreclosed for the immobile, poor, or rapidly declining.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, terminally_ill_patients_denied_access, payer,
    powerless, immediate, trapped, national).

% Patients whose dying process is extended by the prohibition beyond what they would have chosen, experiencing pain, loss of dignity, and diminishing capacity that may eventually disqualify them from ever exercising the choice at all — a foreclosure the prohibition itself produces by delay.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, patients_with_prolonged_suffering, payer,
    powerless, immediate, trapped, national).

% Family members and caregivers who absorb the financial, emotional, and logistical costs of an extended dying process that the patient did not choose to prolong. They have no formal standing to authorize the patient's request and can only advocate, relocate the patient, or endure.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, families_bearing_prolonged_dying_costs, payer,
    moderate, biographical, constrained, national).

% Clinicians who, under this reading, are properly understood as facilitators of a patient's sovereign choice rather than independent moral agents overriding it. Where prohibited, they face licensure risk, criminal liability, or professional censure for honoring a competent patient's request, and many self-censor even where legally ambiguous protections exist.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, physicians_willing_to_assist, agenda_setter,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, physicians_willing_to_assist, payer).

% Regulate physician conduct and retain broad discretionary authority over what counts as permissible end-of-life care. Prohibition regimes preserve the boards' gatekeeping function and shield them from having to adjudicate contested individual competency and coercion-risk determinations at scale.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, medical_licensing_boards, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, medical_licensing_boards, agenda_setter).

% Advocate for and benefit from legal prohibitions that align public policy with doctrinal commitments about the sanctity of life, extending their normative authority into secular medical policy without bearing the costs imposed on dying patients.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, religious_institutional_authorities, beneficiary,
    institutional, civilizational, arbitrage, national).

% Hospitals and health systems avoid liability exposure, ethics-committee burden, and reputational risk by operating in jurisdictions or under policies that prohibit or narrowly restrict aid in dying, regardless of individual patient wishes.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, risk_averse_health_systems, beneficiary,
    institutional, generational, arbitrage, national).

% Disabled, elderly, or economically precarious individuals who might face subtle family, financial, or institutional pressure toward hastened death if access expanded without safeguards. This reading treats their risk as a policy-design problem for eligibility criteria, not as grounds to deny the underlying sovereign authority claim — a stance the vulnerability-protection reading contests directly.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, vulnerable_populations_at_coercion_risk, excluded,
    powerless, immediate, trapped, national).

% Adjudicate and theorize the competing claims — sovereignty of the competent individual against sanctity-of-life and vulnerability-protection concerns — through litigation, legislation, and scholarship, without personally bearing the extraction on either side.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, bioethics_scholars_and_courts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__autonomy_reading, medical_licensing_boards).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the arrangement nominally coordinates around protecting life and preventing wrongful death by centralizing the decision to end life within medical and legal gatekeeping structures rather than leaving it to the individual alone.
% TRANSFER_FUNCTION: The prohibition/gatekeeping regime moves control over the timing and manner of death away from the competent individual whose life it is and toward licensing boards, legislatures, and institutional risk-management functions, while the costs of that transfer — prolonged suffering, foreclosed autonomy, family strain — land on the patient and their household.
% ABSENT_VOICES: Patients who died before ever gaining legal access, and patients whose capacity declined below the competency threshold during years of legislative delay, cannot testify to what the prohibition cost them — the strongest evidence of harm is structurally unavailable because the harmed are dead or incompetent by the time policy catches up.
% DISAPPEARANCE_RATIONALE: If the prohibition/gatekeeping structure vanished overnight and sovereign authority over one's own death were fully recognized without institutional checkpoints, physicians would face immediate professional and legal clarity to honor patient requests, health systems would need new protocols and liability frameworks, and a body of patients currently denied access would gain it — a substantial reorganization of end-of-life medical practice, not a null change.
% FOUNDING_PROBLEM: The founding problem this reading identifies is the unjustified subordination of a competent adult's control over the terms of their own dying to institutional, religious, and professional gatekeeping that treats the individual as incapable of authoritative judgment about their own life.
% FOUNDING_PROBLEM_CORROBORATION: Patients' rights litigants, disability-rights-adjacent autonomy advocates (a minority within that movement), and comparative policy analysis from permissive jurisdictions (Oregon, Netherlands, Canada) attest the founding problem remains live in prohibition jurisdictions; medical licensing boards and religious authorities, the beneficiaries of the status quo, are the primary voices asserting the problem is either non-existent or adequately resolved by exception-based carve-outs — corroboration from outside the beneficiary set exists but is contested by those same beneficiaries.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__autonomy_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_decision_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62, reflecting substantial but not absolute extraction — many jurisdictions permit some access under narrow eligibility criteria, so the prohibition is partial rather than total, yet where it binds, the cost to the trapped patient (a foreclosed choice about one's own death) is severe and often irreversible. Suppression is authored high (0.71-0.78) because the prohibition depends on active criminal and licensing enforcement against physicians and patients, not on voluntary participation, and this suppressive infrastructure is long-standing so the trajectory is authored gently declining as jurisdictions incrementally liberalize. Theater ratio rises modestly (0.25 to 0.40) reflecting growing use of narrow exception carve-outs and ethics-committee review processes that perform careful deliberation while substantive denial continues for most who seek access outside tightly bounded criteria. Accessibility collapse (0.58) and resistance (0.69) reflect a constraint that is neither a settled natural fact nor a fully open field — real legal and clinical alternatives exist in some jurisdictions, and active political, legal, and advocacy resistance to the prohibition is substantial and organized.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute divergent seat classifications: from the beneficiary seats (licensing boards, religious authorities) the arrangement likely computes closer to a defensible coordination structure protecting against error and coercion; from the payer seats (denied patients, their families) the same arrangement computes as substantially extractive, coercive, and enforced against their expressed will. This divergence is exactly the structural fact the classification is designed to surface, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Terminally ill patients denied access and patients with prolonged suffering are the clearest targets: powerless, trapped, immediate time horizon, bearing the extraction directly and often unable to relocate to permissive jurisdictions due to illness, poverty, or rapid decline. Medical licensing boards, religious institutional authorities, and risk-averse health systems are beneficiaries: institutional power, arbitrage-grade exit (they are never personally subject to the prohibition's cost), and long time horizons that let them treat the arrangement as a stable policy equilibrium rather than an urgent harm. Physicians willing to assist occupy an intermediate position — agenda_setter in the narrow sense that they administer the clinical act, but also payers of professional and legal risk, which is why they carry a secondary payer role.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this arrangement as pure natural-law sanctity coordination: the tangled_rope reading requires naming both the coordination function claimed (protecting life, preventing wrongful death) and the asymmetric extraction (prolonged suffering imposed on those who would have chosen otherwise, borne overwhelmingly by the powerless). Treating the prohibition as a pure Rope would erase the victim class; treating it as a pure Snare would erase the genuine, if contested, coordination rationale institutions offer (protecting against coercion and irreversible error) — the tangled_rope classification holds both without resolving the underlying moral dispute, which is properly routed to the omega variables and the sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_versus_relational_dependency,
    'Is death-timing authority genuinely a matter of individual sovereignty severable from third-party and institutional interests, or is dying-well-under-relationship-and-care a matter this reading understates by framing it as purely individual?',
    'Comparative outcome data from permissive jurisdictions on family and caregiver experience, plus philosophical analysis of whether death decisions are ever purely individual acts or are always embedded in relational obligation networks.',
    'If dying is shown to be structurally relational rather than purely individual, the autonomy reading''s framing of the family and clinician as merely facilitating (rather than co-stakeholders with independent standing) would need revision, potentially shifting some family-cost burden into a different structural category.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_versus_relational_dependency, conceptual, 'Whether sovereign authority over death is severable from relational and institutional embeddedness.').

omega_variable(
    coercion_risk_externalization_validity,
    'Is the slippery-slope coercion risk toward vulnerable populations genuinely a separable policy-design problem solvable through eligibility criteria (as this reading holds), or is it an inherent structural risk of any authority-granting regime that this reading systematically undercounts?',
    'Longitudinal safeguard-effectiveness data from permissive jurisdictions tracking whether disabled, elderly, or economically vulnerable populations experience disproportionate uptake or documented coercion incidents over time.',
    'If coercion risk proves non-separable from the grant of authority itself, the autonomy reading''s exclusion of vulnerable-population coercion risk from its victim-set accounting understates true extraction, and the vulnerability_protection_reading''s institutional-checkpoint structure would gain empirical support as the more accurate account of the same kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_risk_externalization_validity, empirical, 'Whether externalizing coercion risk to policy design is empirically justified or definitionally convenient for this reading.').

omega_variable(
    competency_threshold_manipulability,
    'How stable and manipulation-resistant is the ''competent individual'' threshold this entire reading depends on — who determines competency, under what incentive structure, and does that determination process itself introduce a gatekeeping extraction this reading has assumed away?',
    'Audit of competency-determination processes across permissive jurisdictions for evidence of systematic bias, under-resourcing for marginalized patients, or inconsistent standards that functionally reproduce a denial mechanism inside the autonomy framework itself.',
    'If competency determination is itself a site of significant extraction or bias, the autonomy reading''s clean sovereignty claim is compromised at its own threshold condition, and some of what looks like resolved autonomy is actually gatekeeping relocated rather than eliminated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competency_threshold_manipulability, empirical, 'Whether the competency gate that operationalizes autonomy is itself extraction-free.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__autonomy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(end__tr_t8, end_of_life_decision_authority__autonomy_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(end__tr_t16, end_of_life_decision_authority__autonomy_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(end__tr_t24, end_of_life_decision_authority__autonomy_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(end__tr_t32, end_of_life_decision_authority__autonomy_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(end__tr_t40, end_of_life_decision_authority__autonomy_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(end__be_t8, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(end__be_t16, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(end__be_t24, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(end__be_t32, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(end__be_t40, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(end__su_t8, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 8, 0.76).
narrative_ontology:measurement(end__su_t16, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 16, 0.74).
narrative_ontology:measurement(end__su_t24, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(end__su_t32, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(end__su_t40, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the end_of_life_decision_authority kernel, each authored as a separate story per the epsilon-invariance principle: autonomy_reading (this file, tangled_rope, epsilon=0.62, prohibition read as extraction from individual sovereignty), sanctity_reading (expected mountain-adjacent or rope framing, low epsilon, prohibition read as vindicating intrinsic life-value rather than extracting), and vulnerability_protection_reading (expected rope or tangled_rope framing emphasizing institutional-checkpoint coordination against both wrongful denial and coercion, with a distinct beneficiary/victim structure centered on coercion-risk populations). All three share the same underlying legal and clinical arrangement but diverge sharply in claimed type, epsilon, and victim-set because they diverge in normative premises about who is harmed by what.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
