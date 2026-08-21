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
 *   human_readable: Sanctity of Life Principle (End-of-Life Context)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint represents the 'sanctity of life' reading of end-of-life
 *   decision authority, asserting that human life possesses intrinsic value
 *   independent of individual will and that intentional life-ending violates
 *   this value. It is a Snare from the perspective of patients experiencing
 *   intractable suffering, as it actively suppresses their autonomy and
 *   denies them exit options, while benefiting institutions and individuals
 *   whose worldview aligns with this principle. The constraint requires
 *   active enforcement through legal prohibitions and medical ethical
 *   guidelines. The claimed type is 'snare' because its primary function,
 *   from the perspective of those it governs, is extraction of autonomy and
 *   prolongation of suffering, rather than genuine coordination.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.65).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.78).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, snare).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Sanctity of Life Principle (End-of-Life Context)").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, '2e513838-c83d-4654-858e-9fe12700b777').
narrative_ontology:cs_kernel_codification('2e513838-c83d-4654-858e-9fe12700b777', formalized).
narrative_ontology:cs_authority_grounding('2e513838-c83d-4654-858e-9fe12700b777', lineage).
narrative_ontology:cs_interpretation_layer_present('2e513838-c83d-4654-858e-9fe12700b777').
narrative_ontology:cs_reading_relation('2e513838-c83d-4654-858e-9fe12700b777', end_of_life_decision_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('2e513838-c83d-4654-858e-9fe12700b777', end_of_life_decision_authority__vulnerability_protection_reading, coexists_with).
narrative_ontology:cs_axiom('2e513838-c83d-4654-858e-9fe12700b777', foundational, human_life_intrinsic_value_absolute).
narrative_ontology:cs_axiom_status(human_life_intrinsic_value_absolute, holdable).
narrative_ontology:cs_axiom_grounding('2e513838-c83d-4654-858e-9fe12700b777', human_life_intrinsic_value_absolute, deontological).
narrative_ontology:cs_axiom('2e513838-c83d-4654-858e-9fe12700b777', secondary, physician_role_life_preserver_only).
narrative_ontology:cs_axiom_status(physician_role_life_preserver_only, holdable).
narrative_ontology:cs_axiom_grounding('2e513838-c83d-4654-858e-9fe12700b777', physician_role_life_preserver_only, conventional).
narrative_ontology:cs_reference_frame('2e513838-c83d-4654-858e-9fe12700b777', traditional_medical_ethics_sanctity_framework).
narrative_ontology:cs_drift_state('2e513838-c83d-4654-858e-9fe12700b777', contemporary_patient_autonomy_movement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2e513838-c83d-4654-858e-9fe12700b777', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, conservative_bioethicists).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, terminally_ill_patients).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, patients_with_intractable_suffering).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, intrinsic_value_of_life_doctrine).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, physician_as_healer_only_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and enforce policies that prohibit intentional life-ending, grounding their position in theological and moral doctrines. They benefit from the perpetuation of a worldview where life is sacred and not subject to individual will.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, religious_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Provide intellectual and ethical justifications for the sanctity of life principle, influencing policy and medical practice. Their professional identity and careers are often aligned with defending this principle.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, conservative_bioethicists, beneficiary,
    organized, biographical, constrained, national).

% Are denied the option of physician-assisted dying or euthanasia, even when facing intractable suffering and a clear desire to end their lives with dignity. Their suffering is prolonged by the constraint.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, terminally_ill_patients, payer,
    powerless, immediate, trapped, local).

% Experience prolonged physical or psychological suffering that cannot be alleviated, yet are compelled to continue living due to legal and ethical prohibitions on intentional life-ending. Their autonomy is overridden.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, patients_with_intractable_suffering, payer,
    powerless, immediate, trapped, local).

% Are seen by this reading as potential victims of coercion if euthanasia is available, even if they express a desire for it. The constraint protects them from external pressure but at the cost of denying their agency in end-of-life decisions.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients, payer,
    powerless, immediate, identity_locked, local).

% Are constrained to a 'healer-only' role, prohibited from participating in intentional life-ending, even when faced with patient requests for assistance in dying. This maintains their professional identity as preservers of life.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, physicians, agenda_setter,
    institutional, biographical, constrained, national).

% Argue for individual autonomy and the right to self-determination in end-of-life decisions, but their arguments are often marginalized or dismissed by proponents of the sanctity of life principle in policy debates.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, secular_human_rights_advocates, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a societal commitment to the preservation of human life, ensuring that medical practice and legal frameworks prioritize life-sustaining measures and prohibit actions that intentionally end life.
% TRANSFER_FUNCTION: Transfers the authority over end-of-life decisions from the individual to a collective moral and legal framework that prioritizes the intrinsic value of life, externalizing individual suffering as a consequence of this higher value.
% ABSENT_VOICES: Terminally ill patients and those with intractable suffering who desire to end their lives with dignity are often not genuinely heard in policy debates dominated by sanctity of life arguments. Secular human rights advocates are also excluded from the core framing.
% DISAPPEARANCE_RATIONALE: If the sanctity of life principle as a binding constraint vanished overnight, end-of-life care would rapidly reorganize around patient autonomy, leading to widespread legalization of physician-assisted dying and euthanasia in many jurisdictions. Medical ethics and legal frameworks would undergo a profound shift.
% FOUNDING_PROBLEM: The founding problem was to establish a universal moral and legal framework that protects human life from arbitrary termination and upholds its inherent dignity, preventing a 'slippery slope' towards devaluation of life.
% FOUNDING_PROBLEM_CORROBORATION: Religious institutions and conservative bioethicists attest that the problem of protecting vulnerable life remains live, citing concerns about potential abuses if intentional life-ending is permitted. However, secular human rights advocates and many medical professionals contest this, arguing that the problem has shifted from protecting life to respecting autonomy and alleviating suffering, and that the 'slippery slope' argument lacks empirical corroboration from jurisdictions where physician-assisted dying is legal.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__sanctity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because it compels individuals to endure suffering against their will, transferring control over their death to an external moral framework. Suppression is very high (0.78) due to legal prohibitions, medical professional codes, and strong societal norms that actively prevent and stigmatize intentional life-ending. Theater ratio is low (0.1) as the constraint is actively and genuinely enforced, not merely performative. Accessibility collapse is high (0.7) because legal and medical alternatives for self-determined death are severely restricted or non-existent. Resistance is moderate (0.4) but growing, as patient advocacy groups and some medical professionals increasingly challenge the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious institutions and conservative bioethicists, this constraint is a Mountain or Rope, representing a fundamental moral truth or a necessary coordination mechanism for protecting vulnerable life. From the perspective of terminally ill patients and those with intractable suffering, it is a Snare, actively extracting their autonomy and prolonging their suffering. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutions and conservative bioethicists are beneficiaries/agenda-setters (low d) as they uphold and benefit from the moral and institutional order this principle creates. Terminally ill patients, patients with intractable suffering, and pressured vulnerable patients are victims/payers (high d) as their autonomy is directly suppressed and their suffering prolonged. Physicians are also constrained (moderate d) as their professional role is strictly defined by this principle, limiting their options for patient care.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Snare prevents mislabeling this constraint as a 'natural law' (Mountain) or a 'coordination mechanism' (Rope) from the perspective of those who bear its costs. While it claims to protect vulnerable life, its actual operation, from the perspective of the victims, is to extract autonomy and prolong suffering without their consent. The 'slippery slope' argument, often used to justify the constraint, serves as a cover story for the extraction, rather than a genuine coordination problem for the victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slippery_slope_empirical_status,
    'Is the ''slippery slope'' argument (that legalizing intentional life-ending inevitably leads to abuses and devaluation of life) empirically corroborated in jurisdictions where it is permitted?',
    'Longitudinal empirical studies comparing outcomes in jurisdictions with and without legalized physician-assisted dying/euthanasia, focusing on rates of abuse, coercion, and changes in societal attitudes towards vulnerable populations.',
    'If empirically disproven, the justification for the constraint weakens significantly, potentially reclassifying it closer to a Piton or a less legitimate Snare. If corroborated, it would strengthen the ''vulnerability_protection_reading'' and potentially shift this reading closer to a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_empirical_status, empirical, 'Empirical status of the ''slippery slope'' argument.').

omega_variable(
    suffering_vs_intrinsic_value_priority,
    'Should the alleviation of intractable individual suffering take precedence over the abstract intrinsic value of life, or vice versa, in end-of-life decisions?',
    'This is a fundamental normative choice, resolvable only through societal value shifts or philosophical consensus, not empirical data.',
    'If individual suffering is prioritized, the constraint''s legitimacy erodes, leading to reclassification towards a Snare or Piton. If intrinsic value is universally prioritized, the constraint''s Mountain-like qualities are reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suffering_vs_intrinsic_value_priority, preference, 'Prioritization of individual suffering versus intrinsic value of life.').

omega_variable(
    sanctity_vs_autonomy_framing,
    'Is the ''sanctity of life'' principle a genuine moral truth (Mountain) or a constructed constraint that serves to maintain institutional power and a specific worldview (Snare)?',
    'Analysis of the historical evolution of the principle, its beneficiaries, and the mechanisms of its enforcement, particularly in contexts where it conflicts with individual autonomy. If its persistence is tied to identifiable beneficiaries and active suppression of alternatives, it leans towards a constructed constraint.',
    'If resolved as a constructed constraint, its classification as a Snare is reinforced. If resolved as a universally recognized moral truth, it would shift towards a Mountain or Rope, but this is unlikely given the existence of strong counter-arguments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctity_vs_autonomy_framing, conceptual, 'Conceptual framing of the sanctity of life principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t1950, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(end__tr_t1970, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(end__tr_t1990, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(end__tr_t2010, end_of_life_decision_authority__sanctity_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(end__tr_t2024, end_of_life_decision_authority__sanctity_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(end__be_t1950, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(end__be_t1970, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1970, 0.62).
narrative_ontology:measurement(end__be_t1990, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(end__be_t2010, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(end__be_t2024, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1950, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(end__su_t1970, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(end__su_t1990, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(end__su_t2010, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 2010, 0.82).
narrative_ontology:measurement(end__su_t2024, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'end_of_life_decision_authority' kernel. Its structural delta is that pressured-vulnerable patients enter the victim set when euthanasia is available, the physician role is healer-only, and individual suffering is externalized. It is linked to the 'autonomy_reading' and 'vulnerability_protection_reading' siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
