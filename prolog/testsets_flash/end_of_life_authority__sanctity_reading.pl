% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Sanctity of Life Prohibition on Assisted Dying
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint represents the 'sanctity of life' reading of end-of-life
 *   authority, which holds that human life has intrinsic value and prohibits
 *   intentional life-ending regardless of individual preference. It is a
 *   foundational principle in many religious and ethical traditions, shaping
 *   legal and medical policy. This reading places vulnerable populations
 *   (elderly, disabled, economically disadvantaged) into the victim set due
 *   to the perceived risk of coercion if assisted dying were permitted, even
 *   as it denies agency to terminally ill patients. The constraint is
 *   actively enforced through legal prohibitions and professional medical
 *   codes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.65).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.78).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Sanctity of Life Prohibition on Assisted Dying").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, '4da4d40d-4e85-42ca-9b29-4aad1d866b01').
narrative_ontology:cs_kernel_codification('4da4d40d-4e85-42ca-9b29-4aad1d866b01', formalized).
narrative_ontology:cs_authority_grounding('4da4d40d-4e85-42ca-9b29-4aad1d866b01', lineage).
narrative_ontology:cs_interpretation_layer_present('4da4d40d-4e85-42ca-9b29-4aad1d866b01').
narrative_ontology:cs_reading_relation('4da4d40d-4e85-42ca-9b29-4aad1d866b01', end_of_life_authority__autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('4da4d40d-4e85-42ca-9b29-4aad1d866b01', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('4da4d40d-4e85-42ca-9b29-4aad1d866b01', foundational, intrinsic_value_of_human_life).
narrative_ontology:cs_axiom_status(intrinsic_value_of_human_life, holdable).
narrative_ontology:cs_axiom_grounding('4da4d40d-4e85-42ca-9b29-4aad1d866b01', intrinsic_value_of_human_life, deontological).
narrative_ontology:cs_axiom('4da4d40d-4e85-42ca-9b29-4aad1d866b01', foundational, protection_of_the_vulnerable_from_coercion).
narrative_ontology:cs_axiom_status(protection_of_the_vulnerable_from_coercion, holdable).
narrative_ontology:cs_axiom_grounding('4da4d40d-4e85-42ca-9b29-4aad1d866b01', protection_of_the_vulnerable_from_coercion, deontological).
narrative_ontology:cs_reference_frame('4da4d40d-4e85-42ca-9b29-4aad1d866b01', traditional_medical_ethics_life_preservation).
narrative_ontology:cs_drift_state('4da4d40d-4e85-42ca-9b29-4aad1d866b01', contemporary_patient_rights_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4da4d40d-4e85-42ca-9b29-4aad1d866b01', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, conservative_bioethicists).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, palliative_care_providers).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, terminally_ill_patients).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, severely_disabled_individuals).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, economically_disadvantaged_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, physicians).
narrative_ontology:constraint_vindicates(end_of_life_authority__sanctity_reading, intrinsic_value_of_life_doctrine).
narrative_ontology:constraint_vindicates(end_of_life_authority__sanctity_reading, do_no_harm_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and enforce the sanctity of life principle, viewing all intentional life-ending as morally impermissible. They provide moral and theological grounding for legal prohibitions and influence public opinion and policy makers. Their identity is deeply fused with this doctrine.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, religious_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the persistence of the sanctity of life framework, which aligns with their professional and ethical commitments. They publish research, advise policy, and participate in legal challenges to assisted dying, reinforcing the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, conservative_bioethicists, beneficiary,
    organized, biographical, constrained, national).

% Benefit from the focus on life preservation and comfort care, as it directs resources and attention to their services. While not directly enforcing the prohibition, their practice aligns with it, and they often present palliative care as the ethical alternative to assisted dying.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, palliative_care_providers, beneficiary,
    moderate, biographical, mobile, local).

% Bear the cost of being denied the option of assisted dying, even when facing unbearable suffering and a clear desire to end their lives. Their choices are limited to prolonged suffering, natural death, or illicit means, with significant legal and social barriers to exit.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, terminally_ill_patients, payer,
    powerless, immediate, trapped, local).

% Are often included in the 'vulnerable' population whose lives are protected by the sanctity of life principle, even if they do not wish to be. They may experience a loss of agency and fear that their lives are being prolonged against their will, or that their desire for assisted dying is interpreted as a sign of devaluing their lives.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, severely_disabled_individuals, payer,
    powerless, biographical, identity_locked, national).

% Face heightened vulnerability to coercion if assisted dying were widely available, as economic pressures could influence end-of-life decisions. The sanctity of life constraint, while protecting them from this specific coercion, also denies them a choice that might be desired in the face of prolonged, costly illness.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, economically_disadvantaged_patients, payer,
    powerless, immediate, trapped, local).

% Are bound by ethical codes and legal frameworks that prohibit active participation in ending a patient's life. This limits their professional role to preserving life and alleviating suffering, even when patients request assistance in dying. They bear the moral and professional cost of denying patient requests, but also benefit from a clear, protective boundary.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, physicians, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, physicians, payer).

% Advocate for individual self-determination in end-of-life decisions. They are excluded from the core decision-making framework of the sanctity of life reading, which prioritizes the intrinsic value of life over individual preference. Their arguments are often framed as a threat to the vulnerable.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, proponents_of_autonomy, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a categorical moral and legal boundary against intentional life-ending, providing a clear framework for medical practice and public policy that prioritizes life preservation and protects vulnerable populations from potential coercion.
% TRANSFER_FUNCTION: Transfers the ultimate decision-making authority over the timing and manner of death from the individual to a collective moral and legal framework, enforced by medical and religious institutions. It also transfers the burden of prolonged suffering onto individuals who desire assisted dying.
% ABSENT_VOICES: Proponents of individual autonomy and patient choice are actively excluded from the core framing of this constraint, as their arguments directly challenge the foundational premise of life's intrinsic value overriding personal preference. Their voices are present in public discourse but not in the constraint's internal logic.
% DISAPPEARANCE_RATIONALE: If the sanctity of life prohibition vanished overnight, medical ethics, legal frameworks, and end-of-life care practices would undergo a profound reorganization. Assisted dying would likely become a legal and medical option, shifting the landscape of patient rights, physician roles, and societal views on death and suffering.
% FOUNDING_PROBLEM: The problem of protecting human life from arbitrary or coerced termination, particularly for vulnerable individuals who might be pressured to end their lives.
% FOUNDING_PROBLEM_CORROBORATION: Religious leaders and conservative bioethicists attest that the problem of protecting vulnerable lives remains live, citing ongoing concerns about potential abuses if assisted dying is legalized. Patient advocacy groups for the disabled also corroborate the risk of coercion, even if they disagree on the solution.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__sanctity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because it imposes a significant cost on individuals who desire to end their suffering through assisted dying, denying them a choice they value. Suppression is also high (0.78) due to strong legal and institutional barriers, making exit options like assisted dying virtually impossible within the formal system. Theater ratio is low (0.1) as the constraint's function is genuinely maintained, with little performative activity masking atrophy. The metrics reflect a system that is effective in its stated goal of prohibiting life-ending, but at a high cost to those whose preferences are suppressed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious institutions and conservative bioethicists, this constraint is a necessary protection for human dignity and the vulnerable (a Rope or even a Mountain). From the perspective of terminally ill patients, it is a Snare, trapping them in suffering against their will. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutions and conservative bioethicists are beneficiaries and agenda-setters, as the constraint aligns with their core values and strengthens their institutional authority. Palliative care providers also benefit from the focus on life preservation. Terminally ill patients, severely disabled individuals, and economically disadvantaged patients are victims/payers, as they bear the direct costs of denied agency and prolonged suffering. Physicians are both agenda-setters (through their ethical codes) and payers (bearing the moral burden of denying patient requests).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to protect vulnerable lives is still considered 'live' by its proponents, preventing a full mandatrophy resolution. However, the increasing resistance from patient autonomy advocates suggests a growing contestation of whether the constraint's current form is the optimal or only way to achieve that protection. The high suppression indicates that its persistence relies on active enforcement against alternatives, rather than universal consent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vulnerability_vs_autonomy_priority,
    'Does prioritizing the protection of vulnerable populations from potential coercion (sanctity of life) necessarily foreclose individual autonomy in end-of-life decisions, or can both be reconciled?',
    'Development of robust safeguards and legal frameworks that protect the vulnerable while allowing for autonomous choice in specific, well-defined circumstances.',
    'If reconcilable, the constraint''s suppression of autonomy could be reduced without compromising protection, potentially shifting its classification towards a more balanced Tangled Rope or even a Rope. If irreconcilable, the current high suppression is inherent to this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerability_vs_autonomy_priority, conceptual, 'The fundamental tension between collective protection and individual self-determination in end-of-life policy.').

omega_variable(
    coercion_risk_empirical_basis,
    'What is the empirical evidence for the actual risk of coercion for vulnerable populations if assisted dying is legalized with safeguards, compared to the current suffering endured by those denied the option?',
    'Longitudinal studies and comparative analyses from jurisdictions where assisted dying is legal, evaluating the effectiveness of safeguards and the incidence of coercion.',
    'Strong evidence of low coercion risk would weaken the ''protection of vulnerable'' justification for the categorical prohibition, potentially reducing the perceived legitimacy of the constraint''s high suppression. Conversely, high coercion risk would reinforce it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_risk_empirical_basis, empirical, 'The factual basis for the ''slippery slope'' argument and its impact on vulnerable groups.').

omega_variable(
    identity_lock_physicians,
    'To what extent is the physician''s ''identity_locked'' exit option a result of deeply internalized professional ethics (deontological) versus structural legal and institutional pressures (conventional)?',
    'Surveys and qualitative studies of physicians in jurisdictions with varying legal frameworks for assisted dying, exploring their moral distress and professional identity shifts.',
    'If primarily internalized, changing legal frameworks might not immediately alter physician behavior or perceived constraints. If primarily structural, legal changes would more directly reduce their ''trapped'' status, potentially shifting their directionality and the constraint''s overall suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_physicians, empirical, 'Structural vs. internalized components of physician identity lock in end-of-life care.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t1970, end_of_life_authority__sanctity_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(end__tr_t1985, end_of_life_authority__sanctity_reading, theater_ratio, 1985, 0.07).
narrative_ontology:measurement(end__tr_t2000, end_of_life_authority__sanctity_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(end__tr_t2010, end_of_life_authority__sanctity_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(end__tr_t2024, end_of_life_authority__sanctity_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(end__be_t1970, end_of_life_authority__sanctity_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(end__be_t1985, end_of_life_authority__sanctity_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(end__be_t2000, end_of_life_authority__sanctity_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(end__be_t2010, end_of_life_authority__sanctity_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(end__be_t2024, end_of_life_authority__sanctity_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1970, end_of_life_authority__sanctity_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(end__su_t1985, end_of_life_authority__sanctity_reading, suppression_requirement, 1985, 0.72).
narrative_ontology:measurement(end__su_t2000, end_of_life_authority__sanctity_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(end__su_t2010, end_of_life_authority__sanctity_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(end__su_t2024, end_of_life_authority__sanctity_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
