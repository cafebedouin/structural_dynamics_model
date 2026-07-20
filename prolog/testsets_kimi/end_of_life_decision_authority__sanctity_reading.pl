% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__sanctity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: end_of_life_decision_authority__sanctity_reading
 *   human_readable: Sanctity Reading of End-of-Life Decision Authority
 *   domain: medical ethics/bioethics/end-of-life policy
 *
 * SUMMARY:
 *   The sanctity reading of end-of-life decision authority treats the
 *   prohibition on intentional life-ending as a normative absolute grounded
 *   in the intrinsic value of human life. It is formalized in medical ethics
 *   codes, criminal prohibitions on assisted dying, and professional
 *   licensing standards. Under this reading, physicians are healers-only,
 *   vulnerable populations are protected from coercion by blanket
 *   prohibition, and competent suffering patients must endure until natural
 *   death. The constraint coordinates medical professional identity and
 *   vulnerable-population protection, but asymmetrically extracts autonomy
 *   and relief from suffering patients who are denied legal access to
 *   assisted death. It is actively enforced by medical licensing boards,
 *   criminal law, and institutional ethics committees.
 *
 * KEY AGENTS:
 *   - medical_licensing_authority: Agenda-setter (institutional/national) â enforces healer-only norm through licensing and discipline
 *   - medical_profession: Beneficiary (organized/national) â receives role clarity and moral insulation from the prohibition
 *   - sanctity_advocacy_institutions: Beneficiary (organized/national) â sees normative doctrine enacted in secular policy
 *   - suffering_terminally_ill_patients: Primary target (powerless/trapped) â bears extraction through denied autonomy and prolonged suffering
 *   - vulnerable_elderly_disabled: Declared beneficiary (powerless/trapped) â receives protective coordination but loses potential future agency
 *   - death_with_dignity_movement: Excluded (organized/constrained) â structurally absent from ethics councils where this reading dominates
 *   - bioethics_analyst: Observer (analytical/global) â tracks cross-jurisdictional divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.65).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.73).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Sanctity Reading of End-of-Life Decision Authority").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical ethics/bioethics/end-of-life policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, '44cc5fe2-ef62-44aa-89af-a993cdf7aad4').
narrative_ontology:cs_kernel_codification('44cc5fe2-ef62-44aa-89af-a993cdf7aad4', formalized).
narrative_ontology:cs_authority_grounding('44cc5fe2-ef62-44aa-89af-a993cdf7aad4', lineage).
narrative_ontology:cs_interpretation_layer_present('44cc5fe2-ef62-44aa-89af-a993cdf7aad4').
narrative_ontology:cs_reading_relation('44cc5fe2-ef62-44aa-89af-a993cdf7aad4', end_of_life_decision_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('44cc5fe2-ef62-44aa-89af-a993cdf7aad4', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('44cc5fe2-ef62-44aa-89af-a993cdf7aad4', foundational, life_intrinsic_value_independent_of_will).
narrative_ontology:cs_axiom_status(life_intrinsic_value_independent_of_will, holdable).
narrative_ontology:cs_axiom_grounding('44cc5fe2-ef62-44aa-89af-a993cdf7aad4', life_intrinsic_value_independent_of_will, deontological).
narrative_ontology:cs_axiom('44cc5fe2-ef62-44aa-89af-a993cdf7aad4', foundational, physician_as_healer_not_killer).
narrative_ontology:cs_axiom_status(physician_as_healer_not_killer, holdable).
narrative_ontology:cs_axiom_grounding('44cc5fe2-ef62-44aa-89af-a993cdf7aad4', physician_as_healer_not_killer, conventional).
narrative_ontology:cs_reference_frame('44cc5fe2-ef62-44aa-89af-a993cdf7aad4', absolute_sanctity_of_life).
narrative_ontology:cs_drift_state('44cc5fe2-ef62-44aa-89af-a993cdf7aad4', contemporary_medical_technology_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('44cc5fe2-ef62-44aa-89af-a993cdf7aad4', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, medical_profession).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, sanctity_advocacy_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, vulnerable_elderly_disabled).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, suffering_terminally_ill_patients).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, intrinsic_value_doctrine).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, hippocratic_healer_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the healer-only professional boundary through licensing requirements, disciplinary proceedings, and accreditation standards. Actively prohibits physicians from participating in intentional life-ending, and punishes violations with loss of licensure.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, medical_licensing_authority, agenda_setter,
    institutional, generational, constrained, national).

% Receives role clarity and moral insulation from an absolute prohibition on killing. The healer-only identity is preserved, but practitioners are constrained from responding to pleas for release and may experience moral distress when forced to witness prolonged suffering they are barred from ending.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, medical_profession, beneficiary,
    organized, biographical, constrained, national).

% Religious and secular institutions whose normative doctrineâthat human life has intrinsic value independent of individual willâis enacted in law, medical ethics, and public policy. They derive legitimation and mobilizing capacity from the constraint's dominance.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, sanctity_advocacy_institutions, beneficiary,
    organized, generational, mobile, national).

% Competent patients with unbearable suffering who are denied access to legal assisted dying. They must endure prolonged pain, loss of dignity, and progressive bodily degradation, or resort to violent, uncertain, or clandestine methods. Their expressed will regarding the timing and manner of death is structurally negated.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, suffering_terminally_ill_patients, payer,
    powerless, immediate, trapped, national).

% Elderly, disabled, or economically precarious individuals who might face family, insurer, or institutional pressure toward premature death if assisted dying were available. The constraint protects them by removing the medical pathway entirely, though at the cost of their potential future autonomy should they later become suffering and competent.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, vulnerable_elderly_disabled, beneficiary,
    powerless, biographical, trapped, national).

% Advocates for patient autonomy in end-of-life decisions who are structurally excluded from medical ethics councils, licensing boards, and policy tables where the sanctity reading dominates. Their empirical evidence from permissive jurisdictions is routinely dismissed or reframed as morally inadmissible.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, death_with_dignity_movement, excluded,
    organized, generational, constrained, national).

% Tracks cross-jurisdictional variation in end-of-life regimes, measuring how different readings of the same kernel produce divergent patient outcomes, physician moral distress, and vulnerable-population safeguards without taking a normative stance on any reading.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, bioethics_analyst, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates medical practice around an absolute prohibition on intentional life-ending, preserving a unified professional identity as healers-only and attempting to protect vulnerable patients from potential coercion by family, insurers, or institutions in a medical system where assisted death might otherwise be routinized.
% TRANSFER_FUNCTION: Transfers autonomy over the timing and manner of death from competent suffering patients to the protective framework of medical ethics and state law; transfers the moral and practical burden of witnessing or enduring intractable suffering onto patients and their families, who must absorb what the medical profession is barred from alleviating through intentional life-ending.
% ABSENT_VOICES: Competent patients actively requesting assisted death are excluded from medical ethics councils. Death-with-dignity advocates and empirical public-health evidence from permissive jurisdictions are structurally marginalized. Disabled-rights critics who oppose both coercion and forced suffering are rarely heard in the binary debate.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, medical practice would reorganize to incorporate assisted-dying protocols, medical training would include intentional life-ending competencies, suffering patients would gain legal access to aid in dying, and the vulnerable-population protections now achieved by blanket prohibition would need to be rebuilt through consent-and-checkpoint mechanisms rather than absolute exclusion.
% FOUNDING_PROBLEM: The risk that medical power over life and death could be abused to eliminate vulnerable, disabled, elderly, or economically burdensome individuals; the need to maintain public trust in medicine by ensuring physicians are never agents of death.
% FOUNDING_PROBLEM_CORROBORATION: Sanctity advocacy institutions and medical ethics authorities attest the problem remains live, citing disability-rights concerns and contested slippery-slope evidence. Death-with-dignity advocates and empirical public-health researchers from outside the benefiting parties attest that abuse is rare where regulated checkpoints exist, and that the founding problem has been partially solved by procedural safeguards; no neutral corroboration accepts the problem as fully live in its original form.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__sanctity_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_decision_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects that the constraint systematically transfers authority over death timing from competent patients to institutional frameworks, imposing prolonged suffering as the price of vulnerable-population protection. Suppression (0.73) is high because the constraint persists through criminalization of assisted dying, medical licensing discipline, and social stigma against giving up â alternatives are not merely unavailable but actively punished. Theater ratio (0.42) captures the performative dimension: public discourse frames the constraint as absolute moral truth, while clinical practice quietly accommodates terminal sedation and withdrawal of treatment that functionally approximate assisted dying, creating a gap between professed principle and operational reality. Accessibility collapse (0.60) is moderate: in sanctity-dominant jurisdictions legal assisted dying is fully collapsed, while in permissive jurisdictions the alternative exists but is stigmatized. Resistance (0.50) reflects sustained death-with-dignity movements, periodic legislative challenges, and physician conscientious objection to the healer-only straitjacket.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (medical licensing authority) and the beneficiary seats (medical profession, sanctity advocates) experience the constraint as necessary coordination that protects medicine's moral center and society's most vulnerable. The target seat (suffering patients) experiences the identical structure as brute extraction of their final autonomy. The engine computes this divergence from the structural asymmetry in exit options (patients are trapped in suffering; institutions are constrained but identity-committed) and directionality. The vulnerable-elderly-disabled seat is the contested middle: declared beneficiary but structurally powerless and trapped, suggesting their directionality may sit closer to target than the constraint's self-description admits.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical licensing authority and sanctity advocacy institutions sit near the beneficiary end: they design, enforce, and derive legitimation from the constraint. Medical profession sits slightly less pure beneficiary because the role constraint also limits clinical discretion and creates moral distress; however, they are net beneficiaries of role clarity. Suffering terminally ill patients are full targets: the constraint is constructed specifically to deny their expressed will. Vulnerable elderly/disabled are authored as beneficiaries under the coordination story, but their trapped exit and powerless position means the engine may compute a higher effective extraction than the narrative claims â this ambiguity is captured in the victim_beneficiary_boundary omega.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing abuse of medical power to eliminate vulnerable people â is contested rather than live in its original form. Empirical evidence from permissive jurisdictions with procedural safeguards suggests the problem is manageable through distributed checkpoints rather than blanket prohibition. The constraint persists beyond its demonstrated necessity for the founding problem, but it is not yet a pure piton because the coordination function (professional identity maintenance, vulnerable-population protection) is still load-bearing for the benefiting parties. Were empirical evidence to show that vulnerability protection is achievable without blanket prohibition, the constraint would migrate toward snare or piton status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sanctity_reading_kernel_boundary,
    'Does the sanctity reading logically foreclose the autonomy reading within any single framework, or do they merely coexist as incompatible normative positions held by different parties?',
    'Jurisprudential analysis of whether a single legal-medical framework can simultaneously hold that human life has intrinsic value independent of will and that competent individuals possess sovereign authority over their own death; natural experiments from mixed or pluralist jurisdictions.',
    'If foreclosed, adoption of the sanctity reading structurally displaces autonomy; if coexisting, the constraint''s extraction is softened by the live presence of an alternative reading within the same institutional space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctity_reading_kernel_boundary, conceptual, 'Logical relationship between sanctity and autonomy readings of the end-of-life kernel.').

omega_variable(
    victim_beneficiary_boundary,
    'Are pressured-vulnerable populations genuine beneficiaries of this constraint, or does the protection narrative mask paternalistic extraction of agency from disabled and elderly persons?',
    'Longitudinal comparison of wellbeing, institutionalization rates, self-reported agency, and economic security of vulnerable populations under sanctity-dominant versus autonomy-permissive regimes, controlling for healthcare access and wealth.',
    'If vulnerable populations experience the constraint as protective, the coordination function is genuine and asymmetric extraction is limited to suffering patients; if they experience it as infantilizing or identity-erasing, the beneficiary set is misidentified and the constraint extracts more broadly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_beneficiary_boundary, empirical, 'Whether vulnerable-population protection is genuine coordination or paternalistic extraction.').

omega_variable(
    physician_role_necessity,
    'Is the healer-only physician role structurally necessary for public trust in medicine, or is it a contingent professional preference maintained by the constraint?',
    'Comparative trust surveys and physician moral-distress studies in jurisdictions with and without legal assisted dying; analysis of whether trust correlates with the healer-only boundary or with procedural transparency.',
    'If trust is independent of the healer-only boundary, the coordination story collapses toward pure extraction; if trust depends on it, the coordination function is load-bearing and the classification leans toward rope rather than snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(physician_role_necessity, empirical, 'Whether the healer-only role is necessary for medical trust or contingent on the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__sanctity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(end__tr_t8, end_of_life_decision_authority__sanctity_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(end__tr_t16, end_of_life_decision_authority__sanctity_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(end__tr_t24, end_of_life_decision_authority__sanctity_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(end__tr_t32, end_of_life_decision_authority__sanctity_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(end__tr_t40, end_of_life_decision_authority__sanctity_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(end__be_t8, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(end__be_t16, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(end__be_t24, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(end__be_t32, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(end__be_t40, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(end__su_t8, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(end__su_t16, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(end__su_t24, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(end__su_t32, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(end__su_t40, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 40, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the end_of_life_decision_authority kernel. The kernel decomposes into three structurally distinct constraints: sanctity_reading (absolute prohibition), autonomy_reading (individual sovereignty), and vulnerability_protection_reading (distributed checkpoints). Each has a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
