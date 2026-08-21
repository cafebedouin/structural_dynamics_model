% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__deterrence_reading, []).

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
 *   constraint_id: state_execution_authority__deterrence_reading
 *   human_readable: State Execution Authority (Deterrence Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the deterrence reading of state execution
 *   authority, where capital punishment is justified as a means to prevent
 *   future murders by raising the cost of capital crimes. It frames the
 *   executed offender as an instrumental cost for a greater societal good
 *   (public safety). The high extractiveness reflects the ultimate cost of
 *   life, while the high suppression reflects the state's power to enforce
 *   this penalty. The claimed type is 'tangled_rope' because it asserts a
 *   coordination function (deterrence) alongside clear, asymmetric extraction
 *   (taking a life).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.9).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.9).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "State Execution Authority (Deterrence Reading)").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, '0aaa5ce8-99d7-4be0-8920-ee8c4dc843f5').
narrative_ontology:cs_kernel_codification('0aaa5ce8-99d7-4be0-8920-ee8c4dc843f5', formalized).
narrative_ontology:cs_authority_grounding('0aaa5ce8-99d7-4be0-8920-ee8c4dc843f5', extraction).
narrative_ontology:cs_interpretation_layer_present('0aaa5ce8-99d7-4be0-8920-ee8c4dc843f5').
narrative_ontology:cs_reading_relation('0aaa5ce8-99d7-4be0-8920-ee8c4dc843f5', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('0aaa5ce8-99d7-4be0-8920-ee8c4dc843f5', state_execution_authority__abolition_reading, forecloses).
narrative_ontology:cs_axiom('0aaa5ce8-99d7-4be0-8920-ee8c4dc843f5', foundational, capital_punishment_deters_crime).
narrative_ontology:cs_axiom_status(capital_punishment_deters_crime, holdable).
narrative_ontology:cs_axiom_grounding('0aaa5ce8-99d7-4be0-8920-ee8c4dc843f5', capital_punishment_deters_crime, empirically_contingent).
narrative_ontology:cs_axiom('0aaa5ce8-99d7-4be0-8920-ee8c4dc843f5', secondary, state_has_right_to_take_life_for_societal_protection).
narrative_ontology:cs_axiom_status(state_has_right_to_take_life_for_societal_protection, holdable).
narrative_ontology:cs_axiom_grounding('0aaa5ce8-99d7-4be0-8920-ee8c4dc843f5', state_has_right_to_take_life_for_societal_protection, deontological).
narrative_ontology:cs_reference_frame('0aaa5ce8-99d7-4be0-8920-ee8c4dc843f5', utilitarian_deterrence_framework).
narrative_ontology:cs_drift_state('0aaa5ce8-99d7-4be0-8920-ee8c4dc843f5', contemporary_criminological_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0aaa5ce8-99d7-4be0-8920-ee8c4dc843f5', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, potential_victims_of_capital_crimes).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, state_prosecutors).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, law_and_order_politicians).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, wrongfully_convicted_individuals).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, families_of_executed_offenders).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, utilitarian_justice_theory).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, deterrence_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and pursue capital sentences, arguing for their necessity in deterring future violent crime and protecting public safety. Their careers and public image are often tied to successful prosecutions.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, state_prosecutors, agenda_setter,
    institutional, biographical, constrained, national).

% Are the theoretical beneficiaries of the deterrence effect, as the constraint aims to reduce the likelihood of them becoming victims of capital crimes. Their safety is the primary justification for the constraint.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, potential_victims_of_capital_crimes, beneficiary,
    powerless, immediate, trapped, local).

% Bear the ultimate cost of the constraint, losing their lives. From the deterrence perspective, their execution is an instrumental cost to achieve a greater societal good.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% Face the risk of being executed for crimes they did not commit, representing a catastrophic failure of the system and an irreversible cost. Their existence highlights the inherent risks of the constraint.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, wrongfully_convicted_individuals, payer,
    powerless, immediate, trapped, local).

% Bear significant emotional and social costs associated with the execution of a family member, regardless of guilt. They often face stigma and a prolonged grieving process.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, families_of_executed_offenders, payer,
    powerless, biographical, trapped, local).

% Actively campaign against capital punishment, arguing it is morally wrong and ineffective as a deterrent. While they participate in public discourse and legal challenges, their core arguments are often excluded from the dominant 'tough on crime' policy framing.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, abolitionist_advocates, excluded,
    organized, generational, mobile, national).

% Benefit politically from advocating for capital punishment as a strong stance against crime, aligning with public sentiment in some regions. They leverage the deterrence narrative to gain support.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, law_and_order_politicians, beneficiary,
    powerful, biographical, mobile, national).

% Interpret the constitutionality of capital punishment, including its application and procedural safeguards. Their rulings shape the legal framework within which the deterrence claim operates, but they do not directly enforce or benefit from the executions themselves.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% Conduct empirical research on the effectiveness of capital punishment as a deterrent. Their findings often challenge the deterrence claim, providing evidence that informs public and legal debates.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, criminologists, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__deterrence_reading, diffuse).
narrative_ontology:fixing_cost_class(state_execution_authority__deterrence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate societal behavior by establishing a severe disincentive against capital crimes, thereby protecting potential victims and maintaining public order.
% TRANSFER_FUNCTION: Transfers the ultimate cost of capital crime (the life of the convicted offender) from potential victims to the offender, with the state acting as the agent of transfer and enforcement.
% ABSENT_VOICES: Executed offenders cannot speak; wrongfully convicted individuals are silenced by the system; abolitionist advocates are often marginalized in 'tough on crime' discourse, despite their organized efforts.
% DISAPPEARANCE_RATIONALE: If state execution authority vanished overnight, the criminal justice system would need to fundamentally re-evaluate its punitive framework. From the deterrence perspective, this could lead to an increase in capital crimes or a shift to life imprisonment as the maximum penalty, with significant societal debate and legal restructuring.
% FOUNDING_PROBLEM: To prevent heinous crimes and protect society by imposing the ultimate penalty, thereby deterring others from committing similar offenses.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (state prosecutors, law-and-order politicians) assert the problem is still live and that deterrence is effective. Criminologists and abolitionist groups (outside the benefiting parties) widely contest the deterrence effect, citing empirical studies that show no significant difference in murder rates between death penalty and non-death penalty states.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__deterrence_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.90) because the constraint involves the irreversible taking of a human life, the ultimate cost. Suppression is also very high (0.90) due to the state's monopoly on legitimate force and the legal machinery required to carry out executions. Theater ratio is low (0.12) as the process, while ritualized, is genuinely aimed at its stated purpose (deterrence and punishment), not primarily performance. Accessibility collapse is moderate-high (0.70) because while alternatives like life imprisonment exist, the deterrence argument attempts to collapse their perceived efficacy. Resistance is high (0.90) due to persistent and organized abolitionist movements.
 *
 * PERSPECTIVAL GAP:
 *   The state and its proponents (prosecutors, law-and-order politicians) view this constraint as a necessary, albeit severe, tool for societal protection. From the perspective of executed offenders, wrongfully convicted individuals, and abolitionist advocates, it is a system of extreme extraction and injustice, with its claimed coordination function (deterrence) being empirically unproven or morally indefensible.
 *
 * DIRECTIONALITY LOGIC:
 *   Potential victims, state prosecutors, and law-and-order politicians are beneficiaries, as the constraint is claimed to protect the former and empowers/benefits the latter. Executed offenders, wrongfully convicted individuals, and their families are clear targets/victims, bearing the ultimate costs. Criminologists and constitutional courts act as observers, analyzing the constraint's efficacy and legality without directly benefiting or paying.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_ambiguity,
    'Is capital punishment actually a superior deterrent to life imprisonment without parole, or are its deterrent effects negligible?',
    'Longitudinal, comparative empirical studies of murder rates in jurisdictions with and without capital punishment, controlling for other socioeconomic factors.',
    'If deterrence is proven negligible, the primary justification for this reading collapses, reclassifying the constraint closer to a pure snare. If a significant deterrent effect is robustly demonstrated, it would strengthen the ''tangled_rope'' classification by validating its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_ambiguity, empirical, 'Uncertainty regarding the empirical effectiveness of capital punishment as a deterrent.').

omega_variable(
    wrongful_execution_risk_impact,
    'How does the irreducible risk of wrongful execution impact the utilitarian calculus of deterrence, especially when considering the moral weight of an innocent life?',
    'Ethical and legal analysis integrating statistical probabilities of error with philosophical frameworks of justice and human rights, potentially leading to a re-evaluation of the ''acceptable'' cost.',
    'If the moral cost of wrongful execution is deemed too high, it could undermine the utilitarian justification, pushing the constraint towards a snare regardless of deterrence efficacy. It would also amplify the effective extraction for all targets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wrongful_execution_risk_impact, conceptual, 'The ethical dilemma posed by the risk of executing innocent individuals within a deterrence framework.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression primarily structural (legal framework, state power) or does it also involve internalized components (fear of state power, societal acceptance of ultimate punishment)?',
    'Sociological studies on public attitudes towards state power and punishment, and analysis of how fear of execution shapes individual and collective behavior beyond direct legal enforcement.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, making it more resilient to legal challenges and harder to dismantle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in capital punishment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_execution_authority__deterrence_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(stat_tr_t1986, state_execution_authority__deterrence_reading, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(stat_tr_t1996, state_execution_authority__deterrence_reading, theater_ratio, 1996, 0.11).
narrative_ontology:measurement(stat_tr_t2006, state_execution_authority__deterrence_reading, theater_ratio, 2006, 0.11).
narrative_ontology:measurement(stat_tr_t2016, state_execution_authority__deterrence_reading, theater_ratio, 2016, 0.12).
narrative_ontology:measurement(stat_tr_t2024, state_execution_authority__deterrence_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_execution_authority__deterrence_reading, base_extractiveness, 1976, 0.8).
narrative_ontology:measurement(stat_be_t1986, state_execution_authority__deterrence_reading, base_extractiveness, 1986, 0.82).
narrative_ontology:measurement(stat_be_t1996, state_execution_authority__deterrence_reading, base_extractiveness, 1996, 0.84).
narrative_ontology:measurement(stat_be_t2006, state_execution_authority__deterrence_reading, base_extractiveness, 2006, 0.86).
narrative_ontology:measurement(stat_be_t2016, state_execution_authority__deterrence_reading, base_extractiveness, 2016, 0.88).
narrative_ontology:measurement(stat_be_t2024, state_execution_authority__deterrence_reading, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_execution_authority__deterrence_reading, suppression_requirement, 1976, 0.85).
narrative_ontology:measurement(stat_su_t1986, state_execution_authority__deterrence_reading, suppression_requirement, 1986, 0.86).
narrative_ontology:measurement(stat_su_t1996, state_execution_authority__deterrence_reading, suppression_requirement, 1996, 0.87).
narrative_ontology:measurement(stat_su_t2006, state_execution_authority__deterrence_reading, suppression_requirement, 2006, 0.88).
narrative_ontology:measurement(stat_su_t2016, state_execution_authority__deterrence_reading, suppression_requirement, 2016, 0.89).
narrative_ontology:measurement(stat_su_t2024, state_execution_authority__deterrence_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, criminal_justice_system_legitimacy).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, due_process_protections).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'state_execution_authority' kernel, alongside the 'retributive_reading' and 'abolition_reading'. Each reading offers a distinct justification and structural analysis of capital punishment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
