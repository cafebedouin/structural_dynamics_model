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
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Intrinsic Value of Human Life (Sanctity Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint represents the 'sanctity of life' reading of end-of-life
 *   authority, which posits an intrinsic value to human life that prohibits
 *   intentional life-ending, regardless of individual preference. It is a
 *   tangled rope because it genuinely coordinates a societal value
 *   (protection of vulnerable life) but does so through asymmetric extraction
 *   from individuals seeking aid in dying, requiring active enforcement to
 *   maintain its categorical prohibition. The structural delta for this
 *   reading is that vulnerable populations (elderly, disabled, economically
 *   disadvantaged) are included in the victim set due to the risk of
 *   coercion, and the physician's role is strictly limited to life
 *   preservation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.65).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.75).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Intrinsic Value of Human Life (Sanctity Reading)").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, '20946bb7-0224-48fc-8770-a2adf4e060a4').
narrative_ontology:cs_kernel_codification('20946bb7-0224-48fc-8770-a2adf4e060a4', formalized).
narrative_ontology:cs_authority_grounding('20946bb7-0224-48fc-8770-a2adf4e060a4', lineage).
narrative_ontology:cs_interpretation_layer_present('20946bb7-0224-48fc-8770-a2adf4e060a4').
narrative_ontology:cs_reading_relation('20946bb7-0224-48fc-8770-a2adf4e060a4', end_of_life_authority__autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('20946bb7-0224-48fc-8770-a2adf4e060a4', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('20946bb7-0224-48fc-8770-a2adf4e060a4', foundational, human_life_has_intrinsic_value).
narrative_ontology:cs_axiom_status(human_life_has_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('20946bb7-0224-48fc-8770-a2adf4e060a4', human_life_has_intrinsic_value, deontological).
narrative_ontology:cs_axiom('20946bb7-0224-48fc-8770-a2adf4e060a4', foundational, intentional_killing_is_categorically_wrong).
narrative_ontology:cs_axiom_status(intentional_killing_is_categorically_wrong, holdable).
narrative_ontology:cs_axiom_grounding('20946bb7-0224-48fc-8770-a2adf4e060a4', intentional_killing_is_categorically_wrong, deontological).
narrative_ontology:cs_reference_frame('20946bb7-0224-48fc-8770-a2adf4e060a4', traditional_medical_ethics_life_preservation).
narrative_ontology:cs_drift_state('20946bb7-0224-48fc-8770-a2adf4e060a4', contemporary_autonomy_movement, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('20946bb7-0224-48fc-8770-a2adf4e060a4', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, pro_life_advocacy_groups).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, healthcare_systems_focused_on_preservation).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, terminally_ill_patients_seeking_aid_in_dying).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, severely_disabled_individuals).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, elderly_patients_at_coercion_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for the sanctity of life as a foundational moral principle, influencing policy and medical practice to prohibit intentional life-ending. Their identity is deeply intertwined with this doctrine.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, religious_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the legal and medical frameworks that uphold the sanctity of life, aligning with their mission to prevent assisted dying. They actively lobby for the maintenance and strengthening of these prohibitions.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, pro_life_advocacy_groups, beneficiary,
    organized, biographical, constrained, national).

% Their institutional mandate and funding structures are often aligned with life preservation, making them beneficiaries of policies that prohibit assisted dying. They provide palliative care but not aid in dying.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, healthcare_systems_focused_on_preservation, beneficiary,
    institutional, generational, constrained, national).

% Are denied the option of physician-assisted dying, even when facing unbearable suffering and a clear prognosis. They bear the cost of prolonged suffering and loss of autonomy at the end of life.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, terminally_ill_patients_seeking_aid_in_dying, payer,
    powerless, immediate, trapped, local).

% Are implicitly pressured by a system that prioritizes life preservation above all else, potentially leading to a perception that their lives are 'burdensome' if they express a desire for aid in dying. They face the risk of being seen as candidates for 'mercy killing' if the sanctity principle is weakened.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, severely_disabled_individuals, payer,
    powerless, biographical, identity_locked, local).

% Are vulnerable to subtle or overt pressure to continue living, even when they might prefer to end their lives, due to family expectations, economic dependency, or fear of being a burden. The sanctity principle, while intended to protect, can inadvertently remove their agency.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, elderly_patients_at_coercion_risk, payer,
    powerless, immediate, trapped, local).

% Are bound by ethical codes and legal frameworks that prohibit them from intentionally ending a patient's life. Their role is limited to alleviating suffering and preserving life, even when patients request aid in dying. They navigate the tension between patient autonomy and the sanctity principle.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, physicians_providing_palliative_care, agenda_setter,
    moderate, biographical, constrained, local).

% Advocate for individual choice and control over end-of-life decisions, including the right to physician-assisted dying. Their arguments are often marginalized or actively opposed by those upholding the sanctity of life, making them excluded from the dominant policy-setting discourse.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, autonomy_advocacy_groups, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal moral and legal framework that prioritizes the preservation of human life, providing a clear ethical boundary for medical practice and societal norms regarding death.
% TRANSFER_FUNCTION: Transfers the ultimate decision-making authority over the timing and manner of death from the individual to a collective moral and legal framework, enforced by medical and legal institutions. It also transfers the burden of prolonged suffering to individuals who wish to end their lives but are prohibited from doing so.
% ABSENT_VOICES: Patients who prioritize autonomy and seek aid in dying, as well as their advocates, are often excluded from the policy-making process where sanctity-of-life arguments dominate. Their perspectives on unbearable suffering and personal choice are systematically de-prioritized.
% DISAPPEARANCE_RATIONALE: If the sanctity of life principle as a categorical prohibition vanished overnight, end-of-life care, medical ethics, and legal frameworks would undergo a profound reorganization. Physician-assisted dying would likely become more widely available, leading to shifts in patient care, family dynamics, and societal attitudes towards death. The role of physicians would expand, and the legal system would need to establish new safeguards.
% FOUNDING_PROBLEM: To prevent arbitrary or coerced termination of life, protect vulnerable individuals from exploitation, and uphold a fundamental respect for human existence against utilitarian calculations.
% FOUNDING_PROBLEM_CORROBORATION: Religious leaders, many medical ethicists, and pro-life organizations attest that the problem of protecting vulnerable life remains live. While autonomy advocates contest the scope of 'vulnerable' and the necessity of a categorical prohibition, the core concern about protecting life from coercion is widely acknowledged, even if the proposed solution is debated.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high because it imposes a significant cost (prolonged suffering, loss of autonomy) on individuals who wish to end their lives. Suppression is also high, as legal and medical systems actively enforce the prohibition, limiting exit options for those seeking aid in dying. Theater ratio is low because the commitment to life preservation is generally genuine, though some enforcement may be performative in the face of growing public support for autonomy. Accessibility collapse is moderate, as palliative care and natural death remain options, but intentional life-ending is foreclosed. Resistance is high due to ongoing advocacy for physician-assisted dying.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, this constraint is a necessary moral safeguard, a rope protecting the vulnerable. From the perspective of victims, it is a snare that denies agency and prolongs suffering. The engine's classification as a tangled rope reflects this dual nature: a genuine coordination function (protecting vulnerable life) intertwined with asymmetric extraction (denying individual end-of-life choice) that requires active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutions and pro-life groups are clear beneficiaries, as the constraint aligns with their core values and missions. Healthcare systems focused on preservation also benefit from a clear, life-affirming mandate. Terminally ill patients, severely disabled individuals, and elderly patients at coercion risk are victims, as their autonomy is curtailed, and they bear the costs of prolonged suffering or potential pressure. Physicians are agenda-setters, bound by the constraint's ethical and legal framework. Autonomy advocacy groups are excluded, as their perspective is actively resisted by the constraint's proponents.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to protect vulnerable life remains live, preventing it from being a piton. However, the contestation around 'vulnerability' and 'protection' versus 'autonomy' means its function is increasingly debated. The classification as a tangled rope prevents mislabeling it as pure extraction (snare) by acknowledging its coordination function, while also preventing it from being seen as pure coordination (rope) by highlighting the asymmetric extraction and active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_risk_vs_autonomy,
    'To what extent does the sanctity reading genuinely protect vulnerable populations from coercion, versus merely denying autonomy to competent individuals?',
    'Empirical studies on the prevalence of coercion in jurisdictions with legal aid-in-dying, compared to those without. Analysis of safeguards'' effectiveness in preventing abuse.',
    'If coercion risk is low and safeguards are effective, the justification for a categorical prohibition weakens, potentially shifting the constraint towards a more autonomy-respecting framework. If coercion is high, the sanctity reading''s protective function is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_risk_vs_autonomy, empirical, 'Assessing the balance between protection from coercion and denial of autonomy.').

omega_variable(
    definition_of_vulnerability,
    'How is ''vulnerability'' defined in the context of end-of-life decisions, and does this definition inadvertently encompass individuals who are competent and capable of autonomous choice?',
    'Conceptual analysis of legal and medical definitions of vulnerability, combined with qualitative research on how these definitions are applied in practice and perceived by patients.',
    'A broad definition of vulnerability that includes competent individuals would amplify the perceived extraction from the autonomy-seeking population. A narrower definition would reduce this perceived extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_vulnerability, conceptual, 'Clarifying the scope of ''vulnerability'' in end-of-life contexts.').

omega_variable(
    physician_role_conflict,
    'Does the categorical prohibition on aid-in-dying create an unresolvable moral conflict for physicians who witness unbearable suffering and believe in patient autonomy?',
    'Qualitative studies and ethical analyses of physicians'' experiences in jurisdictions with and without aid-in-dying. Examination of professional burnout and moral distress related to end-of-life care.',
    'If the conflict is severe, it suggests the constraint imposes a significant ethical burden on a key agenda-setter, potentially leading to covert practices or calls for reform. If manageable, the constraint''s stability is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physician_role_conflict, empirical, 'Assessing the moral burden on physicians under the sanctity reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t1970, end_of_life_authority__sanctity_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(end__tr_t1985, end_of_life_authority__sanctity_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(end__tr_t2000, end_of_life_authority__sanctity_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement(end__tr_t2010, end_of_life_authority__sanctity_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(end__tr_t2024, end_of_life_authority__sanctity_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(end__be_t1970, end_of_life_authority__sanctity_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(end__be_t1985, end_of_life_authority__sanctity_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(end__be_t2000, end_of_life_authority__sanctity_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(end__be_t2010, end_of_life_authority__sanctity_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(end__be_t2024, end_of_life_authority__sanctity_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1970, end_of_life_authority__sanctity_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(end__su_t1985, end_of_life_authority__sanctity_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(end__su_t2000, end_of_life_authority__sanctity_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(end__su_t2010, end_of_life_authority__sanctity_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(end__su_t2024, end_of_life_authority__sanctity_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__slippery_slope_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
