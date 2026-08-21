% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__deterrence_instrument, []).

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
 *   constraint_id: state_killing_authority__deterrence_instrument
 *   human_readable: State Killing Authority: Deterrence Instrument Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'deterrence instrument' reading of state
 *   killing authority: capital punishment is justified if and only if it
 *   prevents future murders at acceptable cost. It frames the condemned as an
 *   instrumental cost and the state's authority as grounded in
 *   crime-prevention efficacy. This is one reading of the
 *   'state_killing_authority' kernel, alongside 'retributive_desert' and
 *   'categorical_abolition'. The metrics reflect a system that is
 *   substantially extractive and highly suppressive, with a growing
 *   theatrical component as empirical evidence for deterrence weakens.
 *
 * KEY AGENTS:
 *   - state_prosecutors: Agenda setter (institutional/constrained) — administers the process, advocates for its use.
 *   - political_leaders: Beneficiary (powerful/mobile) — benefits from 'tough on crime' image.
 *   - potential_future_victims: Beneficiary (powerless/trapped) — abstract group whose lives are purportedly saved.
 *   - condemned_persons: Payer (powerless/trapped) — direct target, loses life.
 *   - families_of_condemned: Payer (powerless/identity_locked) — bears emotional and social costs.
 *   - taxpayers: Payer (moderate/constrained) — bears financial costs.
 *   - abolitionist_advocates: Excluded (organized/constrained) — argues against, marginalized in policy.
 *   - social_scientists: Observer (analytical/analytical) — provides empirical evidence, often challenging deterrence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.65).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.9).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.65).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "State Killing Authority: Deterrence Instrument Reading").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, 'b178258b-f360-4933-b8be-54e639c267ff').
narrative_ontology:cs_kernel_codification('b178258b-f360-4933-b8be-54e639c267ff', formalized).
narrative_ontology:cs_authority_grounding('b178258b-f360-4933-b8be-54e639c267ff', extraction).
narrative_ontology:cs_interpretation_layer_present('b178258b-f360-4933-b8be-54e639c267ff').
narrative_ontology:cs_reading_relation('b178258b-f360-4933-b8be-54e639c267ff', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation('b178258b-f360-4933-b8be-54e639c267ff', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_axiom('b178258b-f360-4933-b8be-54e639c267ff', foundational, capital_punishment_prevents_future_murders).
narrative_ontology:cs_axiom_status(capital_punishment_prevents_future_murders, holdable).
narrative_ontology:cs_axiom_grounding('b178258b-f360-4933-b8be-54e639c267ff', capital_punishment_prevents_future_murders, empirically_contingent).
narrative_ontology:cs_axiom('b178258b-f360-4933-b8be-54e639c267ff', foundational, state_has_right_to_instrumentalize_life_for_public_safety).
narrative_ontology:cs_axiom_status(state_has_right_to_instrumentalize_life_for_public_safety, holdable).
narrative_ontology:cs_axiom_grounding('b178258b-f360-4933-b8be-54e639c267ff', state_has_right_to_instrumentalize_life_for_public_safety, instrumental).
narrative_ontology:cs_reference_frame('b178258b-f360-4933-b8be-54e639c267ff', utilitarian_crime_prevention_framework).
narrative_ontology:cs_drift_state('b178258b-f360-4933-b8be-54e639c267ff', contemporary_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b178258b-f360-4933-b8be-54e639c267ff', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, state_prosecutors).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, political_leaders).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, families_of_condemned).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for capital punishment, framing it as a necessary tool for public safety and crime prevention. Their careers and public image are often tied to securing death sentences, especially in high-profile cases. They administer the legal process that leads to execution.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, state_prosecutors, agenda_setter,
    institutional, biographical, constrained, national).

% Benefit from appearing 'tough on crime' and responsive to public demands for retribution, especially during election cycles. They use deterrence arguments to justify the policy, even when empirical evidence is weak. They are not directly involved in the execution process but authorize the legal framework.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, political_leaders, beneficiary,
    powerful, immediate, mobile, national).

% Are the abstract group whose lives are purportedly saved by the deterrent effect of capital punishment. They are not active agents but are central to the moral calculus of this reading. Their 'benefit' is a counterfactual absence of harm.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, potential_future_victims, beneficiary,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_non_agent(state_killing_authority__deterrence_instrument, potential_future_victims).

% Are the direct targets of the constraint, losing their lives as an instrumental cost for the purported benefit of others. Their agency is completely suppressed by the state's authority. They have no exit options once the sentence is final.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_persons, payer,
    powerless, immediate, trapped, local).

% Bear the emotional and social costs of the execution, often fighting legal battles for decades. Their identity is often fused with their condemned family member, making 'exit' from the struggle unthinkable. They pay through grief, stigma, and legal fees.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, families_of_condemned, payer,
    powerless, generational, identity_locked, local).

% Bear the substantial financial costs of capital punishment, which is often more expensive than life imprisonment due to lengthy appeals. They are diffuse payers with limited direct influence over the policy, but their collective resistance can influence political will.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, taxpayers, payer,
    moderate, biographical, constrained, national).

% Argue against capital punishment on moral and empirical grounds, often citing its ineffectiveness as a deterrent and its disproportionate application. They are excluded from the direct decision-making process but exert pressure through public discourse and legal challenges.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, abolitionist_advocates, excluded,
    organized, generational, constrained, global).

% Conduct empirical research on the deterrent effect of capital punishment. Their findings often challenge the core premise of this reading, but their influence on policy is indirect and often resisted by political actors.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, social_scientists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Purports to coordinate public safety by deterring potential murderers, thereby protecting the lives of law-abiding citizens. It offers a clear, albeit contested, mechanism for state response to extreme violence.
% TRANSFER_FUNCTION: Transfers the life of the condemned person as an instrumental cost, purportedly in exchange for a reduction in future murders (a benefit to potential victims). It also transfers significant financial resources from taxpayers to the legal and penal systems.
% ABSENT_VOICES: The condemned persons themselves are silenced by execution. Abolitionist advocates and many social scientists, whose empirical findings often contradict the deterrence claim, are systematically marginalized in policy debates, especially during periods of high public demand for punitive measures.
% DISAPPEARANCE_RATIONALE: If state authority to execute for deterrence vanished, the criminal justice system would need to fundamentally re-evaluate its punitive philosophy, sentencing guidelines, and public safety strategies. While the immediate murder rate might not change dramatically, the symbolic and structural role of capital punishment in the state's power would be reconfigured, leading to a rearrangement of legal and political discourse around crime and punishment.
% FOUNDING_PROBLEM: The problem of preventing heinous crimes and ensuring public safety, particularly against repeat offenders or those who inspire others to violence.
% FOUNDING_PROBLEM_CORROBORATION: State prosecutors and political leaders consistently attest that the problem of preventing future murders is live and that capital punishment is a necessary tool. However, social scientists and abolitionist advocates, from outside the benefiting parties, widely dispute the efficacy of capital punishment as a unique deterrent, arguing the problem is not solved by this mechanism.
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_killing_authority__deterrence_instrument, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__deterrence_instrument, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__deterrence_instrument_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__deterrence_instrument_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because it involves the ultimate cost (life) for a benefit (deterrence) that is empirically contested and often not realized. Suppression (0.90) is very high as the state's power over the condemned is absolute, and legal avenues for challenge are severely constrained. The theater ratio (0.40) reflects the increasing performative aspect of executions, especially as the empirical case for deterrence weakens; the spectacle serves political ends more than actual crime reduction. The cyclical pattern in extractiveness and theater ratio reflects periods of heightened public demand for executions (e.g., after high-profile crimes) followed by periods of legal challenge and declining public support, leading to fluctuations in the perceived utility and performative value of the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state prosecutors and political leaders, this constraint is a necessary, albeit costly, tool for public safety. From the perspective of condemned persons and their families, it is pure, absolute extraction. Taxpayers experience it as a costly, inefficient system. Social scientists often view it as a policy based on flawed empirical premises. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   State prosecutors and political leaders are beneficiaries (low d) as they gain political capital and maintain institutional power. Potential future victims are also beneficiaries, as the constraint purports to protect them. Condemned persons are the ultimate targets (high d), bearing the full cost. Families of the condemned and taxpayers are also targets, bearing significant emotional and financial costs. Abolitionist advocates and social scientists are observers or excluded, with their directionality determined by their ability to influence the system.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of capital punishment is a strong candidate for mandatrophy, as its core justification (deterrence) is widely disputed by empirical evidence. The constraint persists not primarily due to its functional efficacy, but due to political inertia, retributive impulses, and the institutional power of those who benefit from its existence. The high theater ratio and contested founding problem status are key indicators of this drift. The classification as a Tangled Rope captures the hybrid nature: a claimed coordination function (deterrence) coupled with clear asymmetric extraction (from the condemned and taxpayers) that requires active enforcement to maintain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrent_efficacy_ambiguity,
    'Does capital punishment actually deter future murders more effectively than life imprisonment?',
    'Rigorous, long-term, cross-jurisdictional empirical studies comparing murder rates in states with and without capital punishment, controlling for socioeconomic factors.',
    'If no unique deterrent effect is found, the core justification for this reading collapses, reclassifying it closer to a Snare (pure extraction) or Piton (theatrical maintenance). If a strong deterrent effect is proven, it would strengthen the Rope aspect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrent_efficacy_ambiguity, empirical, 'Empirical evidence for the unique deterrent effect of capital punishment.').

omega_variable(
    cost_benefit_proportionality,
    'Are the financial and social costs of capital punishment (legal fees, appeals, wrongful convictions) proportional to its purported benefits (deterrence)?',
    'Comprehensive economic analysis comparing the full lifecycle costs of capital punishment vs. life imprisonment, alongside a societal valuation of the purported deterrence benefit.',
    'If costs far outweigh benefits, it further weakens the ''acceptable cost'' clause, pushing the classification towards Snare or Piton. If costs are deemed acceptable, it supports the current classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_proportionality, empirical, 'Proportionality of costs to benefits for capital punishment.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine deterrence instrument, or is the deterrence claim a cover for retributive impulses?',
    'Analysis of judicial opinions and legislative debates: if the language consistently prioritizes ''just deserts'' or ''an eye for an eye'' over empirical deterrence, reclassify as a hybrid or a distinct ''retributive_deterrence'' reading.',
    'If deterrence is merely a cover, the true underlying constraint is retributive, and this reading''s classification would shift to reflect that (e.g., a ''retributive_desert'' reading, which might have different extractiveness and suppression dynamics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishing genuine deterrence from retributive motivations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__deterrence_instrument, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__deterrence_instrument, theater_ratio, 10, 0.28).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__deterrence_instrument, theater_ratio, 20, 0.35).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__deterrence_instrument, theater_ratio, 30, 0.4).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__deterrence_instrument, theater_ratio, 40, 0.42).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__deterrence_instrument, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__deterrence_instrument, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__deterrence_instrument, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__deterrence_instrument, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__deterrence_instrument, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__deterrence_instrument, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__deterrence_instrument, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__deterrence_instrument, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__deterrence_instrument, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__deterrence_instrument, suppression_requirement, 20, 0.9).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__deterrence_instrument, suppression_requirement, 30, 0.92).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__deterrence_instrument, suppression_requirement, 40, 0.91).
narrative_ontology:measurement(stat_su_t50, state_killing_authority__deterrence_instrument, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, criminal_sentencing_guidelines).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, prison_industrial_complex).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'state_killing_authority' kernel. Other readings include 'retributive_desert' and 'categorical_abolition', each representing a distinct constraint with its own structural properties and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
