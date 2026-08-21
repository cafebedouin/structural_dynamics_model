% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__abolition_reading, []).

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
 *   constraint_id: state_killing_legitimacy__abolition_reading
 *   human_readable: State Killing as Human Dignity Violation (Abolition Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'abolition_reading' of the
 *   'state_killing_legitimacy' kernel. From this perspective, state killing
 *   is a categorical violation of human dignity, regardless of any claims of
 *   desert or utility. It is modeled as a Snare due to its pure extraction of
 *   life and dignity, maintained by state power and active suppression of
 *   alternatives. The high extractiveness reflects the ultimate cost borne by
 *   the condemned, while high suppression reflects the state's active
 *   enforcement of its punitive authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.95).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.88).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "State Killing as Human Dignity Violation (Abolition Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, '26aea07a-2c99-48da-a002-2de35563a669').
narrative_ontology:cs_kernel_codification('26aea07a-2c99-48da-a002-2de35563a669', formalized).
narrative_ontology:cs_authority_grounding('26aea07a-2c99-48da-a002-2de35563a669', extraction).
narrative_ontology:cs_interpretation_layer_present('26aea07a-2c99-48da-a002-2de35563a669').
narrative_ontology:cs_reading_relation('26aea07a-2c99-48da-a002-2de35563a669', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('26aea07a-2c99-48da-a002-2de35563a669', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('26aea07a-2c99-48da-a002-2de35563a669', foundational, human_dignity_is_inviolable).
narrative_ontology:cs_axiom_status(human_dignity_is_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('26aea07a-2c99-48da-a002-2de35563a669', human_dignity_is_inviolable, deontological).
narrative_ontology:cs_axiom('26aea07a-2c99-48da-a002-2de35563a669', secondary, state_power_is_limited_by_rights).
narrative_ontology:cs_axiom_status(state_power_is_limited_by_rights, holdable).
narrative_ontology:cs_axiom_grounding('26aea07a-2c99-48da-a002-2de35563a669', state_power_is_limited_by_rights, deontological).
narrative_ontology:cs_reference_frame('26aea07a-2c99-48da-a002-2de35563a669', universal_human_rights_framework).
narrative_ontology:cs_drift_state('26aea07a-2c99-48da-a002-2de35563a669', contemporary_global_abolition_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('26aea07a-2c99-48da-a002-2de35563a669', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, state_punitive_authority).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, pro_death_penalty_public).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, condemned_persons).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, human_dignity_doctrine).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, universal_rights_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals sentenced to death, from whom the state extracts their life and dignity. They have no legal or practical exit from the constraint once a sentence is final, and their voice is often silenced.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, condemned_persons, payer,
    powerless, immediate, trapped, national).

% The legal and administrative apparatus that adjudicates, sentences, and carries out state killings. It operates within established legal frameworks and precedents, which constrain its ability to unilaterally abolish the practice.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_judicial_system, agenda_setter,
    institutional, generational, constrained, national).

% The abstract entity representing the state's power to exact ultimate punishment. It benefits from the perceived legitimacy and deterrent effect of capital punishment, reinforcing its authority over life and death.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_punitive_authority, beneficiary,
    institutional, civilizational, constrained, national).

% Individuals and organizations actively campaigning against state killing, arguing for its categorical violation of human rights. They engage in legal challenges, public education, and international advocacy.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, abolitionist_advocates, observer,
    organized, generational, mobile, global).

% Segments of the public who support capital punishment, often believing it provides justice for victims, deters crime, or is a necessary exercise of state power. They benefit from the perceived security and moral order it maintains.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, pro_death_penalty_public, beneficiary,
    organized, biographical, constrained, national).

% International and national bodies that monitor and advocate for human rights, consistently condemning state killing as a violation of fundamental dignity. While influential, they are often excluded from direct legislative or judicial decision-making within sovereign states.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, human_rights_organizations, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__abolition_reading, diffuse).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__abolition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint (state killing) claims to coordinate societal responses to heinous crimes by providing a final form of justice and deterrence, thereby maintaining social order and public safety.
% TRANSFER_FUNCTION: Transfers the life and inherent dignity of condemned persons to the state's punitive authority, ostensibly in exchange for societal retribution, deterrence, and a sense of justice for victims.
% ABSENT_VOICES: The condemned persons themselves, whose ultimate silencing is the constraint's outcome. Also, future generations who may view the practice as barbaric, and those whose moral frameworks categorically reject state-sanctioned violence.
% DISAPPEARANCE_RATIONALE: If state killing vanished overnight, the entire criminal justice system would require fundamental re-evaluation, sentencing guidelines would shift dramatically, and the philosophical underpinnings of state power would be reconfigured. It would necessitate a profound societal shift in how justice and punishment are conceived.
% FOUNDING_PROBLEM: Historically, state killing was established to enforce social order, exact retribution for severe crimes (lex talionis), and deter potential offenders, particularly in contexts where state authority was less consolidated or alternative punishments were limited.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of capital punishment, including some segments of the public and political leaders, argue that the founding problems of retribution and deterrence remain live. Abolitionist advocates, human rights organizations, and many legal scholars attest that the founding problems are either dead (e.g., lack of proven deterrence) or can be addressed by less extreme means (e.g., life imprisonment), and that the practice persists due to inertia and political will rather than genuine necessity. Empirical studies on deterrence and international legal trends support the contested status.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__abolition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__abolition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_killing_legitimacy__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__abolition_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near maximal (0.95) because the constraint involves the irreversible taking of a human life, which is the ultimate form of extraction. Suppression is high (0.88) as the state actively enforces its right to kill, often through legal and political means that limit challenges and alternatives. Theater ratio is low (0.1) because the act of state killing is a direct, unambiguous exercise of power, not primarily performative. Accessibility collapse is moderate (0.75) as legal and political alternatives (e.g., life imprisonment) exist but are often actively suppressed or dismissed. Resistance is high (0.8) due to persistent and organized global abolitionist movements.
 *
 * PERSPECTIVAL GAP:
 *   The state judicial system and pro-death penalty public perceive state killing as a legitimate, even necessary, exercise of justice or deterrence. In contrast, condemned persons experience it as ultimate extraction, and abolitionist advocates view it as a fundamental violation. The engine will compute these divergent classifications based on the structural roles and exit options declared.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons are the direct targets (victims) of the constraint, bearing the ultimate cost. The state punitive authority and pro-death penalty public are beneficiaries, gaining from the exercise of this power or the perceived benefits it provides. Abolitionist advocates and human rights organizations are observers or excluded parties, actively resisting the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately framed as the ''abolition_reading'' of the ''state_killing_legitimacy'' kernel?',
    'Analysis of the core normative claims and their alignment with established abolitionist philosophy and human rights jurisprudence.',
    'If misframed, the analysis of sibling relations and axiomatic grounding would be inaccurate, potentially misrepresenting the nature of the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the specific reading being instantiated.').

omega_variable(
    sibling_structural_delta_retribution,
    'How would the ''retributive_reading'' structurally alter the constraint''s beneficiary/victim declarations and claimed type?',
    'Authoring a separate constraint story for the ''state_killing_legitimacy__retributive_reading'' and comparing its structural properties, particularly its justification for extraction and identification of beneficiaries/victims.',
    'The retributive reading would likely identify the state as a beneficiary of ''justice served'' and the condemned as having ''forfeited'' rights, potentially shifting the claimed type towards a Tangled Rope or even a Rope (from its own perspective) by reframing the extraction as a deserved transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_structural_delta_retribution, conceptual, 'Structural differences introduced by the retributive reading.').

omega_variable(
    sibling_structural_delta_deterrence,
    'How would the ''deterrence_reading'' structurally alter the constraint''s beneficiary/victim declarations and claimed type?',
    'Authoring a separate constraint story for the ''state_killing_legitimacy__deterrence_reading'' and comparing its structural properties, focusing on its empirical claims and justification for extraction.',
    'The deterrence reading would likely identify ''society'' as a beneficiary of ''crime prevention'' and frame the extraction as a necessary cost for a greater good, potentially shifting the claimed type towards a Tangled Rope or Rope (from its own perspective) by reframing the extraction as a functional coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_structural_delta_deterrence, conceptual, 'Structural differences introduced by the deterrence reading.').

omega_variable(
    disagreement_location,
    'Is the core disagreement between the readings primarily about the moral permissibility of state killing, its empirical efficacy, or both?',
    'Analysis of legal arguments, philosophical texts, and public discourse surrounding capital punishment to identify the dominant axes of contention.',
    'If the disagreement is purely moral, empirical evidence against deterrence may not resolve the contest. If it''s also empirical, new data could shift public and legal opinion, influencing the constraint''s persistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location, conceptual, 'Identifies the fundamental nature of the kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__abolition_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1950, state_killing_legitimacy__abolition_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(stat_tr_t1970, state_killing_legitimacy__abolition_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(stat_tr_t1990, state_killing_legitimacy__abolition_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(stat_tr_t2010, state_killing_legitimacy__abolition_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(stat_tr_t2024, state_killing_legitimacy__abolition_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t1950, state_killing_legitimacy__abolition_reading, base_extractiveness, 1950, 0.9).
narrative_ontology:measurement(stat_be_t1970, state_killing_legitimacy__abolition_reading, base_extractiveness, 1970, 0.92).
narrative_ontology:measurement(stat_be_t1990, state_killing_legitimacy__abolition_reading, base_extractiveness, 1990, 0.93).
narrative_ontology:measurement(stat_be_t2010, state_killing_legitimacy__abolition_reading, base_extractiveness, 2010, 0.94).
narrative_ontology:measurement(stat_be_t2024, state_killing_legitimacy__abolition_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1950, state_killing_legitimacy__abolition_reading, suppression_requirement, 1950, 0.8).
narrative_ontology:measurement(stat_su_t1970, state_killing_legitimacy__abolition_reading, suppression_requirement, 1970, 0.85).
narrative_ontology:measurement(stat_su_t1990, state_killing_legitimacy__abolition_reading, suppression_requirement, 1990, 0.87).
narrative_ontology:measurement(stat_su_t2010, state_killing_legitimacy__abolition_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(stat_su_t2024, state_killing_legitimacy__abolition_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, criminal_sentencing_guidelines).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, human_rights_law).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_punitive_power).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'state_killing_legitimacy' kernel, alongside 'state_killing_legitimacy__retributive_reading' and 'state_killing_legitimacy__deterrence_reading'. Each reading offers a distinct structural analysis of state killing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
