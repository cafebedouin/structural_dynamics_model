% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__retributive_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: state_killing_legitimacy__retributive_reading
 *   human_readable: State Killing Legitimacy (Retributive Reading)
 *   domain: Criminal Justice / Political Philosophy / Legal Theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'retributive_reading' of the
 *   broader 'state_killing_legitimacy' kernel. It focuses on the
 *   justification of capital punishment as a proportional desert for murder
 *   (lex talionis). From this perspective, the state's act of taking a life
 *   is a necessary and just response to uphold moral order, with the
 *   convicted murderer forfeiting their right to life through their actions.
 *   The high extractiveness and suppression reflect the ultimate nature of
 *   the penalty, even when framed as just.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.9).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.95).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "State Killing Legitimacy (Retributive Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "Criminal Justice / Political Philosophy / Legal Theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, '7e52b4c2-5816-40bc-89c4-c1f1a83b4946').
narrative_ontology:cs_kernel_codification('7e52b4c2-5816-40bc-89c4-c1f1a83b4946', formalized).
narrative_ontology:cs_authority_grounding('7e52b4c2-5816-40bc-89c4-c1f1a83b4946', lineage).
narrative_ontology:cs_interpretation_layer_present('7e52b4c2-5816-40bc-89c4-c1f1a83b4946').
narrative_ontology:cs_reading_relation('7e52b4c2-5816-40bc-89c4-c1f1a83b4946', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e52b4c2-5816-40bc-89c4-c1f1a83b4946', state_killing_legitimacy__abolition_reading, forecloses).
narrative_ontology:cs_axiom('7e52b4c2-5816-40bc-89c4-c1f1a83b4946', foundational, proportional_desert_is_justice).
narrative_ontology:cs_axiom_status(proportional_desert_is_justice, holdable).
narrative_ontology:cs_axiom_grounding('7e52b4c2-5816-40bc-89c4-c1f1a83b4946', proportional_desert_is_justice, deontological).
narrative_ontology:cs_axiom('7e52b4c2-5816-40bc-89c4-c1f1a83b4946', foundational, state_has_right_to_exact_life_for_life).
narrative_ontology:cs_axiom_status(state_has_right_to_exact_life_for_life, holdable).
narrative_ontology:cs_axiom_grounding('7e52b4c2-5816-40bc-89c4-c1f1a83b4946', state_has_right_to_exact_life_for_life, conventional).
narrative_ontology:cs_reference_frame('7e52b4c2-5816-40bc-89c4-c1f1a83b4946', lex_talionis_principle).
narrative_ontology:cs_drift_state('7e52b4c2-5816-40bc-89c4-c1f1a83b4946', contemporary_human_rights_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7e52b4c2-5816-40bc-89c4-c1f1a83b4946', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, moral_order).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, society_at_large).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, victims_families).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, convicted_murderers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the legal framework for capital punishment, interpreting and applying the principle of proportional desert. It holds the authority to exact the ultimate penalty.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, state_judicial_system, agenda_setter,
    institutional, generational, analytical, national).

% Bear the ultimate cost of the constraint, forfeiting their life as retribution for their crime. They have no exit from the state's judgment and power.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, convicted_murderers, payer,
    powerless, immediate, trapped, local).

% May experience a sense of justice or closure from the execution of the convicted murderer, aligning with the retributive principle.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, victims_families, beneficiary,
    moderate, biographical, constrained, local).

% An abstract entity that is perceived to be upheld and restored by the application of proportional desert, reinforcing societal values of justice and accountability.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, moral_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(state_killing_legitimacy__retributive_reading, moral_order).

% Benefits from the perceived upholding of justice and moral principles, reinforcing the idea that heinous crimes are met with commensurate punishment, contributing to social cohesion and order.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, society_at_large, beneficiary,
    organized, generational, constrained, national).

% Actively oppose capital punishment on moral and ethical grounds, arguing it violates fundamental human rights. They are excluded from the direct application of this retributive framework but exert pressure for its repeal.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, abolitionist_advocates, excluded,
    organized, generational, mobile, global).

% Analyze the philosophical, legal, and ethical underpinnings of capital punishment, including the retributive justification, contributing to ongoing public and academic debate.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, legal_scholars, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__retributive_reading, moral_order).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for proportional punishment for heinous crimes, aiming to restore moral balance and uphold the sanctity of life by exacting a commensurate penalty from the offender.
% TRANSFER_FUNCTION: Transfers the life of the convicted murderer to the state/moral order as a form of proportional desert for the crime committed, satisfying the demand for retribution.
% ABSENT_VOICES: Abolitionist advocates are structurally excluded from the direct application of this constraint; they would argue that state killing is an inherent violation of human rights, regardless of the crime or retributive justification.
% DISAPPEARANCE_RATIONALE: If the principle of proportional desert for murder (leading to state killing) vanished overnight, the entire criminal justice system's legitimacy framework for severe crimes would need to be fundamentally re-evaluated, leading to significant societal and legal restructuring regarding punishment and justice.
% FOUNDING_PROBLEM: How to justly respond to the ultimate crime (murder) in a way that upholds moral order and provides proportional retribution, ensuring that offenders receive punishment commensurate with their actions.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of retributive justice, some victims' families, and certain historical and contemporary legal traditions attest to its ongoing relevance. While opponents contest its morality or efficacy, the underlying problem of how to respond to murder with justice remains a live debate.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_killing_legitimacy__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__retributive_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is set high (0.90) because the constraint involves the ultimate extraction of a human life. Suppression is also very high (0.95) due to the absolute power of the state in enforcing capital punishment, leaving no exit for the condemned. The theater ratio is low (0.10) as the act is generally regarded as a serious, functional punishment, not mere performance. Accessibility collapse is total (1.0) for the condemned. Resistance (0.70) reflects the ongoing, significant opposition from abolitionist movements and human rights advocates, even if the state proceeds with executions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'convicted_murderers', the constraint is an absolute, coercive extraction. From the perspective of the 'state_judicial_system' and 'society_at_large' (within this reading), it is a just and necessary act of retribution that upholds moral order. The engine will compute this divergence based on the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'state_judicial_system' acts as the agenda-setter, enforcing the constraint. 'Convicted_murderers' are the clear targets/payers, bearing the full cost. 'Moral_order' and 'society_at_large' are beneficiaries, as the constraint is seen to uphold justice and social cohesion. 'Victims_families' may also be beneficiaries through a sense of closure. 'Abolitionist_advocates' are excluded, as their arguments fundamentally challenge the constraint's premise.
 *
 * MANDATROPHY ANALYSIS:
 *   Within the retributive reading, the founding problem of justly responding to murder is considered 'live'. Therefore, from this perspective, the constraint's mandate has not outlived its function, and mandatrophy is not resolved. The persistence of the constraint is tied to the ongoing belief in proportional desert as a necessary component of justice for heinous crimes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''retributive_reading'' of the ''state_killing_legitimacy'' kernel, distinct from deterrence or abolitionist readings?',
    'Analysis of the primary justification invoked by proponents: if the core argument is proportional desert, the classification holds. If it shifts to crime prevention, it''s a deterrence reading; if it''s about inherent rights, it''s an abolitionist reading.',
    'Misidentification would lead to incorrect classification and an inaccurate mapping of the kernel''s contested landscape.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms this constraint as a specific reading of a contested kernel.').

omega_variable(
    proportional_desert_validity,
    'Is the concept of ''proportional desert'' (lex talionis) a universally valid and applicable principle for justifying state killing, or is it culturally and historically contingent?',
    'Cross-cultural and historical analysis of justice systems, philosophical debate on the foundations of retributive justice, and empirical studies on its societal impact.',
    'If found to be contingent, the foundational axiom of this reading would be weakened, potentially shifting its classification towards a more purely extractive snare if its legitimacy is undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportional_desert_validity, conceptual, 'Ambiguity regarding the universal validity of proportional desert.').

omega_variable(
    state_right_to_life_forfeiture,
    'Does the state legitimately possess the right to declare an individual''s life-right forfeited based on their actions, or is this an overreach of state power?',
    'Legal and political philosophy debates on the limits of state sovereignty, human rights jurisprudence, and constitutional interpretations regarding fundamental rights.',
    'If the state''s right is deemed illegitimate, the entire framework of this reading collapses, reclassifying the constraint as a pure snare with no legitimate coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_right_to_life_forfeiture, conceptual, 'Ambiguity regarding the state''s authority to exact life for life.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1950, state_killing_legitimacy__retributive_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(stat_tr_t1965, state_killing_legitimacy__retributive_reading, theater_ratio, 1965, 0.11).
narrative_ontology:measurement(stat_tr_t1980, state_killing_legitimacy__retributive_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(stat_tr_t1995, state_killing_legitimacy__retributive_reading, theater_ratio, 1995, 0.09).
narrative_ontology:measurement(stat_tr_t2010, state_killing_legitimacy__retributive_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(stat_tr_t2020, state_killing_legitimacy__retributive_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t1950, state_killing_legitimacy__retributive_reading, base_extractiveness, 1950, 0.9).
narrative_ontology:measurement(stat_be_t1965, state_killing_legitimacy__retributive_reading, base_extractiveness, 1965, 0.89).
narrative_ontology:measurement(stat_be_t1980, state_killing_legitimacy__retributive_reading, base_extractiveness, 1980, 0.9).
narrative_ontology:measurement(stat_be_t1995, state_killing_legitimacy__retributive_reading, base_extractiveness, 1995, 0.91).
narrative_ontology:measurement(stat_be_t2010, state_killing_legitimacy__retributive_reading, base_extractiveness, 2010, 0.9).
narrative_ontology:measurement(stat_be_t2020, state_killing_legitimacy__retributive_reading, base_extractiveness, 2020, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1950, state_killing_legitimacy__retributive_reading, suppression_requirement, 1950, 0.95).
narrative_ontology:measurement(stat_su_t1965, state_killing_legitimacy__retributive_reading, suppression_requirement, 1965, 0.94).
narrative_ontology:measurement(stat_su_t1980, state_killing_legitimacy__retributive_reading, suppression_requirement, 1980, 0.95).
narrative_ontology:measurement(stat_su_t1995, state_killing_legitimacy__retributive_reading, suppression_requirement, 1995, 0.96).
narrative_ontology:measurement(stat_su_t2010, state_killing_legitimacy__retributive_reading, suppression_requirement, 2010, 0.95).
narrative_ontology:measurement(stat_su_t2020, state_killing_legitimacy__retributive_reading, suppression_requirement, 2020, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, criminal_justice_system_legitimacy).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, human_rights_norms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
