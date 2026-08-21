% ============================================================================
% CONSTRAINT STORY: state_execution_authority__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__retributive_reading, []).

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
 *   constraint_id: state_execution_authority__retributive_reading
 *   human_readable: State Execution Authority (Retributive Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the retributive reading of state execution
 *   authority, where the state's power to execute is justified as a means to
 *   restore moral balance and impose proportionate punishment for heinous
 *   crimes. It is framed as a necessary function of justice, not merely a
 *   deterrent. The high extractiveness reflects the ultimate cost borne by
 *   the executed, while high suppression is required to maintain the state's
 *   monopoly on this form of justice against significant opposition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.85).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.92).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority (Retributive Reading)").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, '739ed772-b751-4cec-92ec-07242ffc7032').
narrative_ontology:cs_kernel_codification('739ed772-b751-4cec-92ec-07242ffc7032', formalized).
narrative_ontology:cs_authority_grounding('739ed772-b751-4cec-92ec-07242ffc7032', lineage).
narrative_ontology:cs_interpretation_layer_present('739ed772-b751-4cec-92ec-07242ffc7032').
narrative_ontology:cs_reading_relation('739ed772-b751-4cec-92ec-07242ffc7032', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('739ed772-b751-4cec-92ec-07242ffc7032', state_execution_authority__abolition_reading, forecloses).
narrative_ontology:cs_axiom('739ed772-b751-4cec-92ec-07242ffc7032', foundational, proportionate_punishment_axiom).
narrative_ontology:cs_axiom_status(proportionate_punishment_axiom, holdable).
narrative_ontology:cs_axiom_grounding('739ed772-b751-4cec-92ec-07242ffc7032', proportionate_punishment_axiom, deontological).
narrative_ontology:cs_axiom('739ed772-b751-4cec-92ec-07242ffc7032', foundational, moral_order_restoration_axiom).
narrative_ontology:cs_axiom_status(moral_order_restoration_axiom, holdable).
narrative_ontology:cs_axiom_grounding('739ed772-b751-4cec-92ec-07242ffc7032', moral_order_restoration_axiom, deontological).
narrative_ontology:cs_reference_frame('739ed772-b751-4cec-92ec-07242ffc7032', lex_talionis_framework).
narrative_ontology:cs_drift_state('739ed772-b751-4cec-92ec-07242ffc7032', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('739ed772-b751-4cec-92ec-07242ffc7032', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, retributive_justice_advocates).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, state_judicial_system).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, death_row_inmates).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, lex_talionis_principle).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, moral_order_restoration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the legal process of capital punishment, interpreting laws and constitutional provisions to ensure what it deems proportionate punishment. It claims to uphold moral order and justice through this authority.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, state_judicial_system, agenda_setter,
    institutional, civilizational, constrained, national).

% Seek closure and a sense of moral balance restored through the execution of those who committed heinous crimes against their loved ones. Their advocacy often influences policy and judicial decisions.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, victims_families, beneficiary,
    powerful, generational, constrained, local).

% Actively support capital punishment as a necessary tool for moral accountability and the restoration of societal order. They articulate the philosophical and ethical justifications for retribution.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, retributive_justice_advocates, beneficiary,
    organized, generational, mobile, national).

% Bear the ultimate cost of the constraint, their lives taken as the proportionate punishment for their crimes. From this reading, their death is a legitimate and necessary component of moral restoration.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% Await execution, living under the constant threat of the state's ultimate sanction. They are the direct targets of the system's coercive power, with limited legal avenues for reprieve.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, death_row_inmates, payer,
    powerless, biographical, trapped, local).

% Are structurally excluded from the retributive framing, as their core premise rejects the legitimacy of state killing regardless of crime severity. They would argue for the inherent immorality and practical failures of capital punishment.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, abolitionist_advocates, excluded,
    organized, generational, mobile, global).

% Critically observe state execution practices, often condemning them as violations of human rights. They exert moral and diplomatic pressure, but lack direct enforcement power over sovereign states.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__retributive_reading, retributive_justice_advocates).
narrative_ontology:fixing_cost_class(state_execution_authority__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a state-sanctioned mechanism for society to respond to heinous crimes with ultimate punishment, aiming to restore a perceived moral equilibrium and satisfy the demands of justice for victims and society.
% TRANSFER_FUNCTION: Transfers the life of the convicted offender to the state, in exchange for the perceived restoration of moral order and justice for victims' families and society, as well as the affirmation of the state's ultimate authority.
% ABSENT_VOICES: Abolitionist advocates and international human rights bodies are excluded from the retributive framing, as their core premise rejects the legitimacy of state killing. They would argue for alternative forms of justice and the inherent right to life.
% DISAPPEARANCE_RATIONALE: If state execution authority vanished, the criminal justice system would need to fundamentally re-evaluate its ultimate punishment, potentially leading to new forms of life imprisonment or other severe sanctions. The concept of moral balance through state-sanctioned killing would be lost, requiring a profound societal and legal reorganization of justice for heinous crimes.
% FOUNDING_PROBLEM: Society's need to respond to heinous crimes with a punishment perceived as proportionate and capable of restoring moral order, particularly for crimes that 'offend humanity' and demand ultimate accountability.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of retributive justice, victims' rights organizations, and some legal scholars attest to the ongoing need for ultimate punishment to satisfy justice and moral balance. Opponents (abolitionists, human rights groups) contest its efficacy and morality, arguing the problem is either solved by other means or was never validly addressed by execution.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_execution_authority__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__retributive_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the constraint demands the ultimate sacrifice (life) from the offender, which is considered non-negotiable for the claimed moral restoration. Suppression is very high (0.92) due to the state's absolute coercive power required to carry out executions and to defend the practice against legal challenges and public resistance. Theater ratio is low (0.1) because, from this reading, the act of execution is a genuine, solemn act of justice, not a performance. Resistance is moderate (0.6) reflecting ongoing, organized opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of victims' families and retributive justice advocates, the constraint is a necessary and just mechanism for moral restoration. From the perspective of the executed and death row inmates, it is the ultimate act of state violence and extraction. The state judicial system views it as upholding law and order, while abolitionists see it as an immoral and unjustifiable act.
 *
 * DIRECTIONALITY LOGIC:
 *   The state judicial system, victims' families, and retributive justice advocates are beneficiaries, as they gain the perceived moral balance and affirmation of justice. Executed offenders and death row inmates are the clear targets, bearing the ultimate cost. The high power of the state and the trapped status of offenders drive directionality towards maximum extraction for the targets.
 *
 * MANDATROPHY ANALYSIS:
 *   From the retributive reading, the mandate for execution is considered 'live' because the problem of moral imbalance from heinous crimes is seen as perpetual and requiring this ultimate response. Proponents argue that the function has not atrophied, but rather remains a fundamental pillar of justice. The high extractiveness and suppression are seen as inherent to this 'live' mandate, not as signs of decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_balance_objectivity,
    'Is ''moral balance'' an objectively restorable state through state execution, or a subjective perception of justice?',
    'Philosophical and ethical discourse, potentially informed by sociological studies on victim satisfaction and societal perceptions of justice post-execution, though ultimate resolution may remain conceptual.',
    'If purely subjective, the justification for high extraction based on ''moral balance'' weakens, potentially reclassifying the constraint as a Snare where the coordination story is cover for state power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_balance_objectivity, conceptual, 'Ambiguity of ''moral balance'' as a justification for execution.').

omega_variable(
    wrongful_execution_invalidation,
    'Does the occurrence of wrongful executions fundamentally invalidate the retributive framework, or is it considered a tragic but acceptable error within the system?',
    'Legal and ethical rulings on the ''fallibility'' of justice systems, and societal response to proven innocence cases. If the framework cannot tolerate any error, it is invalidated.',
    'If wrongful executions invalidate the framework, the constraint''s legitimacy collapses, and its persistence becomes pure coercion, shifting classification towards Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wrongful_execution_invalidation, conceptual, 'Impact of wrongful executions on retributive justification.').

omega_variable(
    alternative_punishment_sufficiency,
    'Can alternative punishments, such as life imprisonment without parole, achieve the same degree of ''moral balance'' and ''proportionate punishment'' as execution?',
    'Comparative analysis of justice systems with and without capital punishment, and philosophical debate on the nature of ''ultimate'' punishment. Empirical data on victim satisfaction and societal perceptions of justice.',
    'If alternatives are deemed sufficient, the necessity of execution for moral balance is undermined, reducing the perceived coordination function and increasing the effective extraction, pushing towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_punishment_sufficiency, empirical, 'Sufficiency of alternative punishments for retributive goals.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''retributive_reading'' of the ''state_execution_authority'' kernel. What structural elements would change under sibling readings?',
    'Analysis of the ''deterrence_reading'' (focus on future crime prevention) and ''abolition_reading'' (categorical rejection of state killing) constraints.',
    'The ''deterrence_reading'' would shift beneficiaries (e.g., ''potential victims'') and justifications. The ''abolition_reading'' would fundamentally alter the victim/beneficiary structure and eliminate the constraint entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identification of this constraint as a specific reading of the state execution kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_execution_authority__retributive_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(stat_tr_t1988, state_execution_authority__retributive_reading, theater_ratio, 1988, 0.09).
narrative_ontology:measurement(stat_tr_t2000, state_execution_authority__retributive_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(stat_tr_t2012, state_execution_authority__retributive_reading, theater_ratio, 2012, 0.11).
narrative_ontology:measurement(stat_tr_t2024, state_execution_authority__retributive_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_execution_authority__retributive_reading, base_extractiveness, 1976, 0.8).
narrative_ontology:measurement(stat_be_t1988, state_execution_authority__retributive_reading, base_extractiveness, 1988, 0.82).
narrative_ontology:measurement(stat_be_t2000, state_execution_authority__retributive_reading, base_extractiveness, 2000, 0.85).
narrative_ontology:measurement(stat_be_t2012, state_execution_authority__retributive_reading, base_extractiveness, 2012, 0.84).
narrative_ontology:measurement(stat_be_t2024, state_execution_authority__retributive_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_execution_authority__retributive_reading, suppression_requirement, 1976, 0.85).
narrative_ontology:measurement(stat_su_t1988, state_execution_authority__retributive_reading, suppression_requirement, 1988, 0.88).
narrative_ontology:measurement(stat_su_t2000, state_execution_authority__retributive_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(stat_su_t2012, state_execution_authority__retributive_reading, suppression_requirement, 2012, 0.91).
narrative_ontology:measurement(stat_su_t2024, state_execution_authority__retributive_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__deterrence_reading).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'state_execution_authority' kernel. Each reading presents a different structural justification and set of beneficiaries/victims, leading to different classifications. This retributive reading focuses on moral balance and proportionate punishment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
