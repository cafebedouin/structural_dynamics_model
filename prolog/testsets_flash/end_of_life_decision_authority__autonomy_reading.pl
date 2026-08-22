% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: Competent Individuals' Sovereign Authority Over Own Death (Autonomy Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint asserts that competent individuals hold ultimate
 *   authority over decisions regarding their own death, including the right
 *   to seek assistance in dying. It frames the denial of such agency as a
 *   form of extraction, prolonging suffering against an individual's will.
 *   This is the 'autonomy_reading' of the 'end_of_life_decision_authority'
 *   kernel, which is contested by 'sanctity_reading' and
 *   'vulnerability_protection_reading'. The structural delta for this reading
 *   is that individuals whose suffering is prolonged against their will enter
 *   the victim set, and healthcare professionals become facilitators rather
 *   than solely preservers of life.
 *
 * KEY AGENTS:
 *   - competent_individuals: Primary beneficiary/agenda_setter (powerful/mobile)
 *   - suffering_prolonged_against_will: Primary victim (powerless/trapped)
 *   - healthcare_professionals_facilitating: Beneficiary/agenda_setter (institutional/constrained)
 *   - healthcare_professionals_objecting: Payer (institutional/constrained)
 *   - legal_frameworks: Agenda-setter (institutional/civilizational)
 *   - advocacy_groups_for_autonomy: Beneficiary (organized/mobile)
 *   - advocacy_groups_for_sanctity: Payer (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.25).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.35).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "Competent Individuals' Sovereign Authority Over Own Death (Autonomy Reading)").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, 'a004eb5c-73d1-40fb-a347-65195faf0919').
narrative_ontology:cs_kernel_codification('a004eb5c-73d1-40fb-a347-65195faf0919', formalized).
narrative_ontology:cs_authority_grounding('a004eb5c-73d1-40fb-a347-65195faf0919', expertise).
narrative_ontology:cs_interpretation_layer_present('a004eb5c-73d1-40fb-a347-65195faf0919').
narrative_ontology:cs_reading_relation('a004eb5c-73d1-40fb-a347-65195faf0919', end_of_life_decision_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('a004eb5c-73d1-40fb-a347-65195faf0919', end_of_life_decision_authority__vulnerability_protection_reading, coexists_with).
narrative_ontology:cs_axiom('a004eb5c-73d1-40fb-a347-65195faf0919', foundational, individual_autonomy_is_paramount).
narrative_ontology:cs_axiom_status(individual_autonomy_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('a004eb5c-73d1-40fb-a347-65195faf0919', individual_autonomy_is_paramount, deontological).
narrative_ontology:cs_axiom('a004eb5c-73d1-40fb-a347-65195faf0919', secondary, relief_of_suffering_is_a_moral_imperative).
narrative_ontology:cs_axiom_status(relief_of_suffering_is_a_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('a004eb5c-73d1-40fb-a347-65195faf0919', relief_of_suffering_is_a_moral_imperative, deontological).
narrative_ontology:cs_reference_frame('a004eb5c-73d1-40fb-a347-65195faf0919', individual_self_determination_framework).
narrative_ontology:cs_drift_state('a004eb5c-73d1-40fb-a347-65195faf0919', contemporary_bioethics_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a004eb5c-73d1-40fb-a347-65195faf0919', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, competent_individuals).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_facilitating).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_against_will).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, advocacy_groups_for_autonomy).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_objecting).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, advocacy_groups_for_sanctity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who, being of sound mind and facing unbearable suffering, seek to exercise control over the timing and manner of their death. They benefit from the recognition of their autonomy.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, competent_individuals, beneficiary,
    powerful, biographical, mobile, global).

% Individuals who are competent but are denied access to end-of-life options, leading to prolonged suffering against their expressed wishes. They bear the direct cost of the constraint's absence or denial.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_against_will, payer,
    powerless, immediate, trapped, local).

% Medical practitioners who, in accordance with legal and ethical guidelines, assist competent individuals in exercising their end-of-life choices. They benefit from aligning their practice with patient autonomy.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_facilitating, agenda_setter,
    institutional, biographical, constrained, national).

% Medical practitioners who, due to moral or religious convictions, object to participating in end-of-life assistance. They bear the cost of professional tension, potential legal challenges, or moral distress.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_objecting, payer,
    institutional, biographical, constrained, national).

% The body of laws and regulations that define the scope and limits of end-of-life decision-making. They set the rules for all other stakeholders.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, legal_frameworks, agenda_setter,
    institutional, generational, analytical, national).

% Organizations that champion individual rights and self-determination in end-of-life decisions. They benefit from the expansion of autonomy-based policies.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, advocacy_groups_for_autonomy, beneficiary,
    organized, generational, mobile, global).

% Organizations that advocate for the intrinsic value of human life and oppose policies that permit assisted dying. They bear the cost of policies that contradict their core values.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, advocacy_groups_for_sanctity, payer,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the individual's autonomous will regarding their death with the medical and legal systems, ensuring that end-of-life decisions are respected and facilitated within a structured framework.
% TRANSFER_FUNCTION: Transfers the ultimate authority over end-of-life decisions from external moral or institutional bodies to the competent individual, and transfers the responsibility for facilitating these decisions to the medical system.
% ABSENT_VOICES: The voices of future vulnerable populations, whose potential for coercion is a central concern of the 'vulnerability_protection_reading', are absent from the direct articulation of this autonomy-focused constraint. Their concerns are externalized as a 'slippery slope' risk rather than integrated into the core design.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the legal and ethical landscape around end-of-life care would be fundamentally altered. Individuals would lose recognized agency, medical professionals would revert to a purely life-preserving role, and the intense suffering of those wishing to die would be prolonged without recourse. The entire system would need to reorganize around a different foundational principle.
% FOUNDING_PROBLEM: The problem of individuals experiencing prolonged, unbearable suffering with no legal or medical means to end their lives, leading to a loss of dignity and autonomy in their final moments.
% FOUNDING_PROBLEM_CORROBORATION: Patient advocacy groups, medical ethicists, and legal scholars (outside of groups directly benefiting from the constraint's existence) consistently attest to the ongoing nature of this problem, citing cases of individuals seeking end-of-life options and the ethical dilemmas faced by healthcare providers. Public opinion polls also frequently show support for individual autonomy in these decisions.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__autonomy_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__autonomy_reading_tests).
:- end_tests(end_of_life_decision_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.25) is moderate, reflecting the burden placed on individuals denied agency over their death, but not a direct financial transfer. Suppression (0.35) is also moderate, as legal and ethical barriers actively prevent individuals from exercising this authority. Theater ratio is low (0.1) as the debate is largely substantive. The claimed type is 'rope' because, from the perspective of this reading, it coordinates individual will with medical practice to alleviate suffering, with beneficiaries being those who gain agency and facilitators. However, the existence of victims and active enforcement suggests a potential for reclassification to 'tangled_rope' or 'snare' from other perspectives.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of competent individuals seeking to end their suffering, the constraint is a 'rope' that enables agency. From the perspective of those who uphold the sanctity of life, it might be seen as a 'snare' that devalues human existence. For healthcare professionals, it shifts their role from solely preserving life to also facilitating its end, creating a tension that can be extractive for those who object on moral grounds.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent individuals are beneficiaries (d=0.0) as they gain agency. Healthcare professionals who facilitate are also beneficiaries (d=0.0) as they align with patient autonomy. Individuals whose suffering is prolonged against their will are victims (d=1.0). Healthcare professionals who object to facilitating are payers (d=1.0) as they bear the moral cost of participating or the professional cost of refusing. Legal frameworks are agenda-setters (d=0.5) as they define the boundaries. Advocacy groups for autonomy are beneficiaries (d=0.0) as their goals are met, while advocacy groups for sanctity are payers (d=1.0) as their values are challenged.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as the underlying problem of suffering and the desire for autonomy in death are perennial. The contest is over the *framing* of the problem and its solution, not the problem's disappearance. The classification as 'rope' from the autonomy reading prevents mislabeling genuine coordination of individual will and medical practice as pure extraction, while acknowledging the costs borne by those who oppose it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_autonomy_reading,
    'Is this constraint a genuine expression of individual autonomy, or is it a specific reading of a broader kernel that could be interpreted differently?',
    'Analysis of legal and ethical frameworks that explicitly prioritize individual self-determination in end-of-life decisions over other values.',
    'If it is merely one reading, its classification is contingent on the acceptance of the autonomy principle as primary. If it were superseded by a sanctity_reading, for example, the victim set would shift, and the constraint would appear extractive from the perspective of those seeking to end their lives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_autonomy_reading, conceptual, 'This constraint is the ''autonomy_reading'' of the ''end_of_life_decision_authority'' kernel.').

omega_variable(
    slippery_slope_risk,
    'Does the implementation of this autonomy-based constraint lead to a ''slippery slope'' where vulnerable individuals are coerced into ending their lives, as argued by the vulnerability_protection_reading?',
    'Longitudinal empirical studies in jurisdictions where such policies are implemented, tracking rates of voluntary vs. potentially coerced decisions, and the demographic profiles of those utilizing end-of-life options.',
    'If a ''slippery slope'' is empirically demonstrated, the effective extractiveness and suppression for vulnerable individuals would increase, potentially reclassifying the constraint as a Tangled Rope or Snare for that specific group. This would shift the constraint closer to the vulnerability_protection_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_risk, empirical, 'Assessing the ''slippery slope'' argument against the autonomy reading.').

omega_variable(
    sanctity_of_life_conflict,
    'How does this autonomy-based constraint reconcile with the ''sanctity_reading'' which posits an intrinsic value of human life independent of individual will?',
    'Philosophical and theological debate, and legal frameworks that attempt to balance or prioritize these competing values. This is a fundamental value conflict.',
    'If the sanctity_reading were to gain dominance, this autonomy_reading would be foreclosed, and the act of ending one''s life would be reclassified as a violation, shifting the victim set to those who facilitate it, and the constraint itself would likely become a Snare from the perspective of those seeking autonomy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanctity_of_life_conflict, preference, 'Conflict with the sanctity of life principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(end__be_t5, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(end__be_t10, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 10, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(end__su_t5, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(end__su_t10, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__vulnerability_protection_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
