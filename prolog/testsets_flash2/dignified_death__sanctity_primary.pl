% ============================================================================
% CONSTRAINT STORY: dignified_death__sanctity_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__sanctity_primary, []).

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
 *   constraint_id: dignified_death__sanctity_primary
 *   human_readable: Dignity as Sanctity of Life (Sanctity-Primary Reading)
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint, 'Dignity as Sanctity of Life (Sanctity-Primary
 *   Reading)', is one interpretation of the broader 'dignified_death' kernel.
 *   It asserts that dignity resides in life's intrinsic value, making
 *   intentional life-termination a violation of transcendent moral law,
 *   regardless of consent. This reading prioritizes the protection of life
 *   over individual autonomy, often leading to the prolongation of suffering
 *   for terminally ill or vulnerable individuals. The constraint is claimed
 *   as a snare because, while ostensibly protective, it coercively limits
 *   choices and extracts agency from those it purports to protect.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__sanctity_primary, 0.6).
domain_priors:suppression_score(dignified_death__sanctity_primary, 0.75).
domain_priors:theater_ratio(dignified_death__sanctity_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, extractiveness, 0.6).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Dignity as Sanctity of Life (Sanctity-Primary Reading)").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__sanctity_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, '0b71bca7-2789-4a95-abcf-281b550bc854').
narrative_ontology:cs_kernel_codification('0b71bca7-2789-4a95-abcf-281b550bc854', formalized).
narrative_ontology:cs_authority_grounding('0b71bca7-2789-4a95-abcf-281b550bc854', lineage).
narrative_ontology:cs_interpretation_layer_present('0b71bca7-2789-4a95-abcf-281b550bc854').
narrative_ontology:cs_reading_relation('0b71bca7-2789-4a95-abcf-281b550bc854', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('0b71bca7-2789-4a95-abcf-281b550bc854', dignified_death__relational_autonomy, coexists_with).
narrative_ontology:cs_axiom('0b71bca7-2789-4a95-abcf-281b550bc854', foundational, life_has_intrinsic_value).
narrative_ontology:cs_axiom_status(life_has_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('0b71bca7-2789-4a95-abcf-281b550bc854', life_has_intrinsic_value, deontological).
narrative_ontology:cs_axiom('0b71bca7-2789-4a95-abcf-281b550bc854', foundational, intentional_killing_is_morally_wrong).
narrative_ontology:cs_axiom_status(intentional_killing_is_morally_wrong, holdable).
narrative_ontology:cs_axiom_grounding('0b71bca7-2789-4a95-abcf-281b550bc854', intentional_killing_is_morally_wrong, theological).
narrative_ontology:cs_reference_frame('0b71bca7-2789-4a95-abcf-281b550bc854', traditional_sanctity_of_life_doctrine).
narrative_ontology:cs_drift_state('0b71bca7-2789-4a95-abcf-281b550bc854', contemporary_bioethics_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('0b71bca7-2789-4a95-abcf-281b550bc854', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, moral_order_advocates).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, religious_institutions).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, terminally_ill_patients).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, vulnerable_elderly).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, disabled_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, medical_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and enforce legal and social norms against intentional life-termination, viewing it as a violation of transcendent moral law. They benefit from the preservation of a moral order aligned with their values.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, moral_order_advocates, agenda_setter,
    institutional, generational, identity_locked, national).

% Their doctrines often align with the sanctity-of-life principle, reinforcing their moral authority and community cohesion. They benefit from the societal adherence to this principle.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, religious_institutions, beneficiary,
    organized, civilizational, identity_locked, global).

% Are denied the option of physician-assisted dying or euthanasia, even when facing intractable suffering and loss of autonomy. They are forced to prolong their lives against their will, often in pain.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, terminally_ill_patients, payer,
    powerless, immediate, trapped, local).

% May face pressure to continue living even when their quality of life is severely diminished, due to the societal and familial norms reinforced by this constraint. Their choices are limited by the lack of legal alternatives.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, vulnerable_elderly, payer,
    powerless, biographical, constrained, local).

% Are often seen as a group needing protection from coercion, which can inadvertently deny them agency over end-of-life decisions. The constraint, intended to protect, can become a barrier to their self-determination.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, disabled_individuals, payer,
    moderate, biographical, constrained, national).

% Are legally and ethically bound to prolong life, even when it conflicts with patient wishes or causes suffering. They face moral distress and legal risks if they assist in life-termination, regardless of patient consent.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, medical_professionals, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, medical_professionals, agenda_setter).

% Argue for individual self-determination in end-of-life decisions, but their proposals for legalizing physician-assisted dying are actively suppressed or rejected by the prevailing sanctity-of-life framework.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, autonomy_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a societal commitment to the intrinsic value of human life, aiming to prevent coercion and protect vulnerable individuals from pressure to end their lives prematurely.
% TRANSFER_FUNCTION: Transfers the ultimate decision-making authority over life-termination from the individual to a transcendent moral law, enforced by legal and social structures. It prolongs life for those who might otherwise choose to end it.
% ABSENT_VOICES: Advocates for individual autonomy and relational autonomy in end-of-life decisions are actively marginalized or excluded from the dominant discourse, as their perspectives challenge the foundational premise of life's sanctity as primary.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, legal frameworks would rapidly shift to permit various forms of assisted dying, medical practice would adapt, and individuals would gain new choices, fundamentally altering end-of-life care and societal norms around death.
% FOUNDING_PROBLEM: The problem of protecting vulnerable individuals from coercion or pressure to end their lives, and upholding a societal reverence for life against utilitarian or instrumental views.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for the sanctity-of-life principle, including religious leaders and some disability rights groups, attest that the problem of protecting the vulnerable from coercion remains live. However, patient autonomy advocates argue that the constraint now primarily serves to prolong suffering rather than genuinely protect.
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dignified_death__sanctity_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__sanctity_primary, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__sanctity_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__sanctity_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__sanctity_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) is high because it imposes a significant cost on individuals who wish to end their suffering, forcing them to endure unwanted prolongation of life. Suppression (0.75) is also high, as legal and medical systems actively enforce this principle, suppressing alternatives like physician-assisted dying. The theater ratio is low (0.1) because the constraint is genuinely enforced, not merely performed; its function is to prevent life-termination, which it largely achieves. Resistance is high (0.7) due to ongoing advocacy for autonomy-based end-of-life options.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of moral order advocates, this constraint is a necessary protection, a 'rope' safeguarding the vulnerable and upholding a sacred value. From the perspective of terminally ill patients, it is a 'snare' that prolongs suffering and denies agency. The engine's classification as a snare reflects the structural reality of coercive extraction from the victims, despite the claimed protective intent.
 *
 * DIRECTIONALITY LOGIC:
 *   Moral order advocates and religious institutions are beneficiaries (d near 0.0) as the constraint aligns with their deeply held values and reinforces their societal influence. Terminally ill patients, vulnerable elderly, and disabled individuals are victims (d near 1.0) as their agency is extracted, and they are forced to endure conditions against their will. Medical professionals are also payers (d near 0.7) as they are compelled to act against patient wishes in some cases, facing moral distress and legal constraints.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_protection_boundary,
    'At what point does the ''protection of vulnerable individuals'' (the stated coordination function) transition into ''coercive prolongation of suffering'' (the observed extraction)?',
    'Empirical studies on patient-reported quality of life, psychological distress, and perceived autonomy in jurisdictions with and without legal end-of-life options. Analysis of the actual incidence of coercion in systems that permit assisted dying.',
    'If coercion is demonstrably rare in autonomy-respecting systems, it weakens the protective justification for the sanctity-primary constraint, reclassifying it more firmly as a snare. If coercion is significant, it strengthens the protective justification, potentially shifting the classification towards a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_protection_boundary, empirical, 'Distinguishing genuine protection from coercive control in end-of-life decisions.').

omega_variable(
    transcendent_moral_law_status,
    'Is the ''transcendent moral law'' invoked by this reading a universally recognized and self-evident truth, or a culturally/religiously specific normative claim?',
    'Cross-cultural philosophical analysis and comparative legal studies of end-of-life ethics in diverse societies. Examination of the historical evolution of ''sanctity of life'' concepts.',
    'If universally recognized, it strengthens the ''mountain-like'' aspect of the constraint''s justification. If culturally specific, it highlights the constructed nature of the constraint, making its coercive enforcement more problematic and reinforcing its snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transcendent_moral_law_status, conceptual, 'The epistemic grounding and universality of the ''transcendent moral law'' claim.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, medical protocols) or internalized (cognitive patterns, moral guilt) for individuals seeking end-of-life options?',
    'Post-exit suppression trajectory: if individuals who move to jurisdictions with legal end-of-life options still report significant internal barriers or guilt, reclassify as partially internalized. If barriers are purely external, suppression is structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making true exit more difficult. This would reinforce the ''trapped'' exit option for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in end-of-life choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__sanctity_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dign_tr_t10, dignified_death__sanctity_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(dign_tr_t20, dignified_death__sanctity_primary, theater_ratio, 20, 0.1).
narrative_ontology:measurement(dign_tr_t30, dignified_death__sanctity_primary, theater_ratio, 30, 0.1).
narrative_ontology:measurement(dign_tr_t40, dignified_death__sanctity_primary, theater_ratio, 40, 0.1).
narrative_ontology:measurement(dign_tr_t50, dignified_death__sanctity_primary, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__sanctity_primary, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(dign_be_t10, dignified_death__sanctity_primary, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(dign_be_t20, dignified_death__sanctity_primary, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(dign_be_t30, dignified_death__sanctity_primary, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(dign_be_t40, dignified_death__sanctity_primary, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(dign_be_t50, dignified_death__sanctity_primary, base_extractiveness, 50, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__sanctity_primary, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(dign_su_t10, dignified_death__sanctity_primary, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(dign_su_t20, dignified_death__sanctity_primary, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(dign_su_t30, dignified_death__sanctity_primary, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(dign_su_t40, dignified_death__sanctity_primary, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(dign_su_t50, dignified_death__sanctity_primary, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__sanctity_primary, identity_coordination).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% This is the 'sanctity_primary' reading of the 'dignified_death' kernel. It is structurally distinct from the 'autonomy_primary' and 'relational_autonomy' readings, which emphasize different foundational values and produce different victim/beneficiary sets. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
