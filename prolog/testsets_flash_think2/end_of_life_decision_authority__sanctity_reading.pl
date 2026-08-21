% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: end_of_life_decision_authority__sanctity_reading
 *   human_readable: Intrinsic Value of Human Life (Sanctity Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sanctity_reading' of the
 *   'end_of_life_decision_authority' kernel. It asserts that human life
 *   possesses intrinsic value independent of individual will, and that
 *   intentional life-ending violates this value. This reading underpins legal
 *   and ethical prohibitions against euthanasia and physician-assisted
 *   suicide, defining the medical profession's role as life-preserver and
 *   protector of the vulnerable. The constraint is actively enforced through
 *   medical policy and law, leading to high suppression of alternatives for
 *   suffering individuals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.78).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.85).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Intrinsic Value of Human Life (Sanctity Reading)").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, '341325d1-2c99-4412-8b7d-24056dbe7dc5').
narrative_ontology:cs_kernel_codification('341325d1-2c99-4412-8b7d-24056dbe7dc5', formalized).
narrative_ontology:cs_authority_grounding('341325d1-2c99-4412-8b7d-24056dbe7dc5', lineage).
narrative_ontology:cs_interpretation_layer_present('341325d1-2c99-4412-8b7d-24056dbe7dc5').
narrative_ontology:cs_reading_relation('341325d1-2c99-4412-8b7d-24056dbe7dc5', end_of_life_decision_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('341325d1-2c99-4412-8b7d-24056dbe7dc5', end_of_life_decision_authority__vulnerability_protection_reading, coexists_with).
narrative_ontology:cs_axiom('341325d1-2c99-4412-8b7d-24056dbe7dc5', foundational, human_life_is_sacred).
narrative_ontology:cs_axiom_status(human_life_is_sacred, holdable).
narrative_ontology:cs_axiom_grounding('341325d1-2c99-4412-8b7d-24056dbe7dc5', human_life_is_sacred, deontological).
narrative_ontology:cs_axiom('341325d1-2c99-4412-8b7d-24056dbe7dc5', secondary, physician_role_is_to_preserve_life).
narrative_ontology:cs_axiom_status(physician_role_is_to_preserve_life, holdable).
narrative_ontology:cs_axiom_grounding('341325d1-2c99-4412-8b7d-24056dbe7dc5', physician_role_is_to_preserve_life, conventional).
narrative_ontology:cs_reference_frame('341325d1-2c99-4412-8b7d-24056dbe7dc5', life_as_sacred_gift).
narrative_ontology:cs_drift_state('341325d1-2c99-4412-8b7d-24056dbe7dc5', contemporary_secular_society, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('341325d1-2c99-4412-8b7d-24056dbe7dc5', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, medical_profession).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, society_at_large).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, vulnerable_persons_advocates).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, suffering_individuals).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, families_of_suffering).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, advocates_for_death_with_dignity).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, sanctity_of_life_doctrine).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, medical_non_maleficence_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Upholds the principle that life must be preserved and defines its role as healer, not life-ender. Enforces policies that prohibit physician-assisted suicide or euthanasia, maintaining professional boundaries and ethical standards.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, medical_profession, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the legal and ethical framework that aligns with their theological doctrines regarding the sacredness of human life. Actively advocates for the maintenance of these prohibitions.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, religious_institutions, beneficiary,
    organized, generational, constrained, global).

% Benefits from a perceived moral order that values all human life, potentially reducing pressure on vulnerable individuals and reinforcing collective responsibility for care. Bears the societal cost of prolonged suffering for some.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, society_at_large, beneficiary,
    organized, generational, constrained, national).

% Benefits from the constraint's protective stance, arguing it prevents coercion and exploitation of those who might be pressured into ending their lives due to illness, disability, or economic hardship. Their constituency is explicitly protected by this reading.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, vulnerable_persons_advocates, beneficiary,
    organized, biographical, constrained, national).

% Bears the cost of prolonged suffering when denied the option of physician-assisted suicide or euthanasia, even when facing intractable pain or irreversible decline. Their individual will is overridden by the collective value.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, suffering_individuals, payer,
    powerless, immediate, trapped, local).

% Bears the emotional, physical, and financial burden of caring for loved ones whose suffering is prolonged by the constraint, without the option of an assisted death. Their choices are limited by the legal and medical framework.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, families_of_suffering, payer,
    moderate, biographical, constrained, local).

% Actively resists the constraint, advocating for individual autonomy and the right to choose in end-of-life decisions. Their efforts are suppressed by the existing legal and ethical prohibitions.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, advocates_for_death_with_dignity, payer,
    organized, biographical, constrained, national).

% Represents the perspective that individual self-determination should be paramount in end-of-life decisions. Their core premise is fundamentally at odds with the sanctity reading and is largely excluded from the framework this reading establishes.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, autonomy_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__sanctity_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a societal moral consensus around the intrinsic value of human life, independent of individual will, and defines the role of the medical profession as exclusively dedicated to preserving life and alleviating suffering, not intentionally ending it.
% TRANSFER_FUNCTION: Transfers the ultimate authority over life-ending decisions from the individual to a collective moral and legal framework, imposing the burden of continued existence and suffering on individuals and their families, while upholding a perceived societal good.
% ABSENT_VOICES: Competent individuals seeking self-determination in end-of-life decisions, and those who believe that quality of life and relief from suffering can, in some circumstances, ethically outweigh the mere biological prolongation of life. Their perspectives are actively marginalized by this constraint.
% DISAPPEARANCE_RATIONALE: If the constraint that human life possesses intrinsic value independent of individual will vanished overnight, the legal, ethical, and medical frameworks surrounding end-of-life care would undergo profound and rapid reorganization. Physician roles would be redefined, euthanasia and assisted suicide would likely become widely legalized, and societal norms around death and dying would fundamentally shift, leading to a new landscape of choices and responsibilities.
% FOUNDING_PROBLEM: To prevent arbitrary or coerced life-ending, protect vulnerable individuals from pressure to end their lives, and uphold a fundamental moral order that values human existence as inherently good and not subject to individual disposal.
% FOUNDING_PROBLEM_CORROBORATION: Religious institutions and some medical ethicists attest that the problem of protecting life's intrinsic value and preventing coercion remains live. Advocates for autonomy and death with dignity, however, argue that while preventing coercion is important, the constraint has overshot its original purpose and now primarily serves to deny self-determination, causing unnecessary suffering. Independent bioethicists and patient advocacy groups often highlight this tension.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__sanctity_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because the constraint overrides individual autonomy, imposing continued existence and suffering on those who wish to end their lives. This burden is amplified by advances in medical technology that can prolong life indefinitely. Suppression is very high (0.85) as legal and medical systems actively prohibit and prevent intentional life-ending, leaving few accessible alternatives. Theater ratio is low (0.10) because the constraint is genuinely enforced; its maintenance is not primarily performative. Accessibility collapse is high (0.85) as legal and medical alternatives are largely foreclosed. Resistance is substantial (0.70) due to ongoing advocacy for 'death with dignity' and individual autonomy.
 *
 * PERSPECTIVAL GAP:
 *   The constraint is experienced very differently by its beneficiaries and victims. For beneficiaries, it represents a fundamental moral good and a protective measure. For victims, it is a source of profound suffering and a denial of self-determination. The engine's per-seat classification will reflect this divergence, with the claimed 'tangled_rope' type capturing both the coordination function (for society/medical profession) and the asymmetric extraction (from individuals).
 *
 * DIRECTIONALITY LOGIC:
 *   The medical profession, religious institutions, and advocates for vulnerable persons are beneficiaries, as the constraint aligns with their ethical frameworks and protective mandates. Society at large is also a beneficiary, upholding a perceived moral order. Suffering individuals, their families, and advocates for death with dignity are victims, as their autonomy and relief from suffering are suppressed by the constraint. The directionality for victims is high, reflecting the significant extraction they experience.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the constraint as a pure Snare, acknowledging its genuine coordination function in upholding a societal value and defining the medical role. However, it also highlights the significant and increasing extraction from suffering individuals, preventing it from being mislabeled as a pure Rope or Mountain. The 'contested' status of the founding problem further underscores the ongoing tension between its original protective mandate and its current extractive effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sanctity_vs_autonomy_conflict,
    'How does the ''sanctity_reading'' fundamentally conflict with the ''autonomy_reading'' of end-of-life decision authority, and what are the irreducible trade-offs?',
    'Conceptual analysis of foundational ethical principles and their application in legal and medical policy. Resolution would involve a societal choice between prioritizing collective intrinsic value or individual self-determination.',
    'If autonomy is prioritized, the constraint''s extractiveness from individuals would be re-evaluated as unjust, leading to pressure for its removal or modification. If sanctity is reaffirmed, the constraint''s coordination function would be emphasized, but the ethical burden on suffering individuals would remain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanctity_vs_autonomy_conflict, conceptual, 'The core conflict between intrinsic value of life and individual self-determination.').

omega_variable(
    vulnerability_protection_efficacy,
    'Does the prohibition on intentional life-ending, as advocated by the ''sanctity_reading'', genuinely protect vulnerable individuals from coercion, or does it primarily prolong suffering for those who are not coerced?',
    'Empirical studies comparing outcomes in jurisdictions with and without legalized euthanasia/assisted suicide, focusing on rates of coercion, quality of life, and access to palliative care for vulnerable populations.',
    'If empirical evidence shows that prohibitions do not effectively prevent coercion or that safeguards in other models are sufficient, the justification for the constraint''s high suppression would weaken, potentially leading to reclassification or policy changes. If coercion is demonstrably prevented, the protective aspect of the constraint would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerability_protection_efficacy, empirical, 'Empirical effectiveness of the sanctity reading in protecting the vulnerable.').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (''sanctity_reading'') of the ''end_of_life_decision_authority'' kernel. What specific structural elements would change if the ''autonomy_reading'' or ''vulnerability_protection_reading'' were adopted as the dominant framework?',
    'Comparative legal and ethical analysis of different jurisdictions where these readings are dominant. For the autonomy reading, the victim set would shift to those denied choice; for the vulnerability protection reading, the enforcement mechanisms would focus on institutional safeguards rather than outright prohibition.',
    'A shift to the autonomy reading would likely reduce extractiveness from individuals but potentially increase perceived vulnerability. A shift to the vulnerability protection reading would re-distribute enforcement and oversight, potentially altering the balance of coordination and extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural changes under alternative readings of the end-of-life decision authority kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t1950, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(end__tr_t1965, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1965, 0.06).
narrative_ontology:measurement(end__tr_t1980, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1980, 0.07).
narrative_ontology:measurement(end__tr_t1995, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(end__tr_t2010, end_of_life_decision_authority__sanctity_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(end__tr_t2024, end_of_life_decision_authority__sanctity_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(end__be_t1950, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(end__be_t1965, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1965, 0.65).
narrative_ontology:measurement(end__be_t1980, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(end__be_t1995, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1995, 0.74).
narrative_ontology:measurement(end__be_t2010, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 2010, 0.76).
narrative_ontology:measurement(end__be_t2024, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1950, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(end__su_t1965, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1965, 0.78).
narrative_ontology:measurement(end__su_t1980, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(end__su_t1995, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1995, 0.82).
narrative_ontology:measurement(end__su_t2010, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 2010, 0.84).
narrative_ontology:measurement(end__su_t2024, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
