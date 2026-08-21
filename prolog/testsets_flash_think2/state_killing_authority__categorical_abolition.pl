% ============================================================================
% CONSTRAINT STORY: state_killing_authority__categorical_abolition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__categorical_abolition, []).

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
 *   constraint_id: state_killing_authority__categorical_abolition
 *   human_readable: State Killing Authority (Categorical Abolition Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint story analyzes the 'state_killing_authority' from the
 *   perspective of the 'categorical_abolition' reading. This reading asserts
 *   that state killing is inherently impermissible, regardless of crime or
 *   consequence, because life is inalienable. Therefore, the state's exercise
 *   of capital punishment is viewed as a highly extractive and suppressive
 *   mechanism, a 'snare' that violates fundamental rights. The metrics
 *   reflect the operation of the state's authority as assessed by this
 *   abolitionist framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, 0.9).
domain_priors:suppression_score(state_killing_authority__categorical_abolition, 0.85).
domain_priors:theater_ratio(state_killing_authority__categorical_abolition, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, extractiveness, 0.9).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__categorical_abolition, snare).
narrative_ontology:human_readable(state_killing_authority__categorical_abolition, "State Killing Authority (Categorical Abolition Reading)").
narrative_ontology:topic_domain(state_killing_authority__categorical_abolition, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__categorical_abolition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, '91e22a4b-c6f7-447f-b1ce-79c97e386ac7').
narrative_ontology:cs_kernel_codification('91e22a4b-c6f7-447f-b1ce-79c97e386ac7', formalized).
narrative_ontology:cs_authority_grounding('91e22a4b-c6f7-447f-b1ce-79c97e386ac7', lineage).
narrative_ontology:cs_interpretation_layer_present('91e22a4b-c6f7-447f-b1ce-79c97e386ac7').
narrative_ontology:cs_reading_relation('91e22a4b-c6f7-447f-b1ce-79c97e386ac7', state_killing_authority__retributive_desert, forecloses).
narrative_ontology:cs_reading_relation('91e22a4b-c6f7-447f-b1ce-79c97e386ac7', state_killing_authority__deterrence_instrument, forecloses).
narrative_ontology:cs_axiom('91e22a4b-c6f7-447f-b1ce-79c97e386ac7', foundational, life_is_inalienable).
narrative_ontology:cs_axiom_status(life_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('91e22a4b-c6f7-447f-b1ce-79c97e386ac7', life_is_inalienable, deontological).
narrative_ontology:cs_axiom('91e22a4b-c6f7-447f-b1ce-79c97e386ac7', foundational, state_killing_is_categorically_wrong).
narrative_ontology:cs_axiom_status(state_killing_is_categorically_wrong, holdable).
narrative_ontology:cs_axiom_grounding('91e22a4b-c6f7-447f-b1ce-79c97e386ac7', state_killing_is_categorically_wrong, deontological).
narrative_ontology:cs_reference_frame('91e22a4b-c6f7-447f-b1ce-79c97e386ac7', universal_human_rights_framework).
narrative_ontology:cs_drift_state('91e22a4b-c6f7-447f-b1ce-79c97e386ac7', contemporary_legal_discourse, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('91e22a4b-c6f7-447f-b1ce-79c97e386ac7', '').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, state_punitive_authority).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, retributive_justice_advocates).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, victims_families_pro_execution).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, abolitionist_advocates).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, victims_families_anti_execution).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, inalienable_rights_doctrine).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, human_dignity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals sentenced to death, whose lives are directly extracted by the state's authority. They have no legal or practical exit from the constraint once all appeals are exhausted.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_persons, payer,
    powerless, immediate, trapped, national).

% The governmental bodies (legislatures, courts, executive branches) that authorize, sentence, and carry out executions. They benefit from the perceived finality and retributive power of capital punishment, and from the suppression of challenges to this authority.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, state_punitive_authority, agenda_setter,
    institutional, generational, constrained, national).

% Individuals and organizations actively campaigning against capital punishment. They bear the costs of resistance, legal challenges, and public education, often facing political and social opposition.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, abolitionist_advocates, payer,
    organized, generational, constrained, global).

% Groups and individuals who believe in 'just deserts' and that capital punishment is a morally appropriate response to certain crimes. They benefit from the state's exercise of this authority as it aligns with their moral framework.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, retributive_justice_advocates, beneficiary,
    organized, generational, mobile, national).

% Families of victims who seek retribution and closure through the execution of offenders. They benefit from the state's punitive action, which they perceive as justice for their loss.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, victims_families_pro_execution, beneficiary,
    moderate, biographical, constrained, local).

% Families of victims who oppose capital punishment, often advocating for life imprisonment or restorative justice. They bear the emotional and social costs of having their views marginalized by the dominant punitive narrative.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, victims_families_anti_execution, payer,
    moderate, biographical, constrained, local).

% Judicial bodies tasked with interpreting constitutional limits on state power, including capital punishment. They are a site of contestation, weighing legal precedent, evolving standards of decency, and public opinion.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The state's authority to kill coordinates the finality of punishment, the expression of societal retribution, and the perceived maintenance of social order against severe crime.
% TRANSFER_FUNCTION: Transfers the life of the condemned person to the state's punitive authority, and transfers a sense of ultimate justice or retribution to some segments of society, particularly victims' families who support it.
% ABSENT_VOICES: The voice of the condemned person, once executed, is permanently silenced. The voices of abolitionist victims' families and human rights advocates are often marginalized in public discourse, particularly when the state's authority is being asserted.
% DISAPPEARANCE_RATIONALE: If the state's authority to kill vanished overnight, the entire criminal justice system would need to fundamentally restructure its sentencing guidelines, appeals processes, and philosophical justifications for punishment. Public discourse on justice and retribution would shift dramatically, and the state's perceived power would be redefined.
% FOUNDING_PROBLEM: Historically, capital punishment was instituted to provide ultimate retribution for heinous crimes, to deter others from similar acts, and to remove dangerous individuals from society permanently.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of capital punishment (retributive justice advocates, some victims' families) attest that the problems of ultimate justice and deterrence are still live. Abolitionist advocates, human rights organizations, and many legal scholars attest that the founding problems are either dead (e.g., no proven deterrence) or that the 'solution' is morally impermissible regardless of the problem. This is corroborated by international human rights law and empirical studies on deterrence.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_killing_authority__categorical_abolition, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__categorical_abolition, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__categorical_abolition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__categorical_abolition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.9) because the constraint (state killing) extracts life itself, the ultimate and irreversible cost. Suppression is also very high (0.85) as the state actively enforces its authority to kill, suppressing legal and moral challenges, and physically eliminating the condemned. Theater ratio is low (0.1) because the act of execution is direct and functional from the state's perspective, not primarily performative. Accessibility collapse is high (0.75) because for the condemned, there are no alternatives to the state's power, and for abolitionists, the state's power makes the 'inalienable life' claim difficult to uphold in practice. Resistance is high (0.7) due to persistent and organized abolitionist movements globally.
 *
 * PERSPECTIVAL GAP:
 *   The state's punitive authority and its proponents (retributive justice advocates, pro-execution victims' families) perceive capital punishment as a legitimate exercise of justice, potentially a 'rope' or even a 'mountain' of moral necessity. However, from the perspective of condemned persons and abolitionist advocates, the same authority is a 'snare' that unjustly extracts life and suppresses fundamental rights. The engine computes this divergence from the structural data, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons are the ultimate targets (full extraction, trapped exit). Abolitionist advocates and anti-execution victims' families are also targets, bearing the costs of resistance and marginalization. The state punitive authority, retributive justice advocates, and pro-execution victims' families are beneficiaries, gaining from the exercise of this authority or its perceived justice. Constitutional courts act as observers, mediating the contestation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_status_of_life,
    'Is the ''inalienable right to life'' a universally recognized moral truth (Mountain) or a culturally/legally constructed norm (Rope)?',
    'Cross-cultural philosophical analysis and the evolution of international human rights law. If the principle gains universal, non-contingent acceptance, it leans towards Mountain; if its application remains contingent on legal systems, it leans towards Rope.',
    'If a Mountain, the state''s authority to kill is a direct violation of natural law. If a Rope, the state''s authority is a violation of a widely coordinated moral norm, but one that could theoretically be un-coordinated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_status_of_life, conceptual, 'Ambiguity regarding the fundamental grounding of the ''right to life''.').

omega_variable(
    state_legitimacy_to_kill,
    'Does the state possess inherent moral legitimacy to take a life, or is such authority always a constructed power that can be withdrawn?',
    'Analysis of political philosophy regarding the social contract and the limits of state power. If state legitimacy is derived from popular consent, then consent could theoretically be withdrawn; if it''s seen as an inherent aspect of sovereignty, it''s more fixed.',
    'If inherent, the abolitionist position faces a more fundamental challenge. If constructed, the ''snare'' classification is reinforced as the state''s power is contingent and potentially revocable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_legitimacy_to_kill, conceptual, 'The source and limits of state authority over life.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__categorical_abolition, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__categorical_abolition, theater_ratio, 10, 0.1).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__categorical_abolition, theater_ratio, 20, 0.1).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__categorical_abolition, theater_ratio, 30, 0.1).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__categorical_abolition, theater_ratio, 40, 0.1).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__categorical_abolition, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__categorical_abolition, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__categorical_abolition, base_extractiveness, 10, 0.87).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__categorical_abolition, base_extractiveness, 20, 0.88).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__categorical_abolition, base_extractiveness, 30, 0.89).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__categorical_abolition, base_extractiveness, 40, 0.9).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__categorical_abolition, base_extractiveness, 50, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__categorical_abolition, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__categorical_abolition, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__categorical_abolition, suppression_requirement, 20, 0.83).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__categorical_abolition, suppression_requirement, 30, 0.84).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__categorical_abolition, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(stat_su_t50, state_killing_authority__categorical_abolition, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__categorical_abolition, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
