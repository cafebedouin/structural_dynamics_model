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
 *   constraint_id: state_execution_authority__deterrence_reading
 *   human_readable: State Execution Authority (Deterrence Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'deterrence reading' of the state's
 *   authority to execute, where capital punishment is justified primarily by
 *   its presumed effect in preventing future murders by raising the cost of
 *   capital crimes. It is one reading of the broader
 *   'state_execution_authority' kernel, alongside retributive and
 *   abolitionist readings. The constraint is claimed as a Tangled Rope,
 *   reflecting its dual function of claiming to coordinate public safety
 *   while imposing severe, often contested, extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.65).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.85).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "State Execution Authority (Deterrence Reading)").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, '458a4cc9-8721-4a34-ae26-8c72081278ea').
narrative_ontology:cs_kernel_codification('458a4cc9-8721-4a34-ae26-8c72081278ea', formalized).
narrative_ontology:cs_authority_grounding('458a4cc9-8721-4a34-ae26-8c72081278ea', lineage).
narrative_ontology:cs_interpretation_layer_present('458a4cc9-8721-4a34-ae26-8c72081278ea').
narrative_ontology:cs_reading_relation('458a4cc9-8721-4a34-ae26-8c72081278ea', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('458a4cc9-8721-4a34-ae26-8c72081278ea', state_execution_authority__abolition_reading, forecloses).
narrative_ontology:cs_axiom('458a4cc9-8721-4a34-ae26-8c72081278ea', foundational, capital_punishment_deters_more_than_life_imprisonment).
narrative_ontology:cs_axiom_status(capital_punishment_deters_more_than_life_imprisonment, holdable).
narrative_ontology:cs_axiom_grounding('458a4cc9-8721-4a34-ae26-8c72081278ea', capital_punishment_deters_more_than_life_imprisonment, empirically_contingent).
narrative_ontology:cs_axiom('458a4cc9-8721-4a34-ae26-8c72081278ea', secondary, state_has_right_to_protect_citizens_through_punishment).
narrative_ontology:cs_axiom_status(state_has_right_to_protect_citizens_through_punishment, holdable).
narrative_ontology:cs_axiom_grounding('458a4cc9-8721-4a34-ae26-8c72081278ea', state_has_right_to_protect_citizens_through_punishment, deontological).
narrative_ontology:cs_reference_frame('458a4cc9-8721-4a34-ae26-8c72081278ea', utilitarian_crime_prevention_framework).
narrative_ontology:cs_drift_state('458a4cc9-8721-4a34-ae26-8c72081278ea', contemporary_criminological_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('458a4cc9-8721-4a34-ae26-8c72081278ea', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, future_potential_victims).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, state_prosecutors).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, wrongfully_convicted).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Represent the state's interest in public safety and crime deterrence. They seek capital sentences, arguing for their deterrent effect on future capital crimes. Their professional identity is tied to upholding the law and protecting citizens.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, state_prosecutors, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the ultimate cost of the constraint, their lives. From this seat, the constraint is pure extraction, regardless of its claimed deterrent function. Their agency is completely foreclosed.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% Are the theoretical beneficiaries of the deterrence mechanism, experiencing increased safety if capital punishment effectively prevents future murders. Their benefit is diffuse and statistical.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, future_potential_victims, beneficiary,
    powerless, generational, constrained, national).

% Bear the same ultimate cost as executed offenders, but without having committed the crime. Their existence highlights the irreducible error rate and the moral cost of the deterrence mechanism.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, wrongfully_convicted, payer,
    powerless, immediate, trapped, local).

% Actively campaign against capital punishment, often citing its ineffectiveness as a deterrent and the risk of wrongful execution. While organized, they are often excluded from the direct decision-making processes that maintain the constraint.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, abolitionist_advocates, excluded,
    organized, generational, mobile, national).

% Conduct empirical research on the deterrent effect of capital punishment. Their findings often challenge the core premise of this reading, but their influence on policy can be limited by political will.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, criminologists, observer,
    analytical, biographical, analytical, global).

% Interpret and apply laws related to capital punishment, including considering constitutional challenges based on cruel and unusual punishment or due process. Their decisions shape the operational boundaries of the constraint.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To deter capital crimes by imposing the ultimate penalty, thereby coordinating social behavior around the prohibition of murder and aiming to protect society from future violence.
% TRANSFER_FUNCTION: Transfers the life of the executed offender as a cost, aiming to transfer safety and security to potential victims by preventing future capital crimes.
% ABSENT_VOICES: The executed offender, whose voice is silenced by the very act of the constraint. Also, those who would argue for alternative, less severe, but equally effective deterrents (e.g., life without parole) are often marginalized in the public and policy debate.
% DISAPPEARANCE_RATIONALE: If the state's authority to execute for deterrence vanished overnight, the criminal justice system would need to fundamentally re-evaluate its approach to capital crimes. This would likely lead to a shift to life imprisonment as the maximum penalty, and potentially a re-evaluation of crime prevention strategies, causing a significant reorganization of legal and penal practices.
% FOUNDING_PROBLEM: To prevent heinous crimes, particularly murder, and ensure public safety through the imposition of the most severe possible punishment, thereby deterring others.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of capital punishment (e.g., some law enforcement officials, victims' families, and political figures) attest that the problem of deterring murder is still live and requires execution. However, criminologists and abolitionist groups, citing extensive empirical studies, attest that the founding problem is not effectively solved by execution, or that alternative solutions are equally effective, making the founding problem's status as 'live' for execution highly contested by independent analysis.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__deterrence_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The base extractiveness is set at a moderate-high level (0.65, rising to 0.72) because the efficacy of capital punishment as a superior deterrent is highly contested; if it does not deter more than life imprisonment, the extraction of life is largely unjustified. Suppression is very high (0.85, falling to 0.78) as it involves the state's ultimate coercive power, but shows a slight decline over time reflecting growing public and judicial resistance. The theater ratio is moderate (0.4, rising to 0.55) as the symbolic 'message' of deterrence often persists even when its empirical basis weakens, indicating a drift towards performative maintenance. Accessibility collapse is high (0.8) as the alternative of committing capital crimes without this ultimate penalty is severely constrained for potential offenders, and for the executed, all alternatives collapse. Resistance is moderate (0.5) due to active abolitionist movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state prosecutors, the constraint is a necessary tool for public safety and justice, a coordination mechanism. From the perspective of executed offenders or the wrongfully convicted, it is pure, irreversible extraction. Future potential victims perceive a benefit, while criminologists often see a lack of empirical support for the deterrence claim. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Future potential victims are beneficiaries (d near 0.0) as they are theoretically protected. State prosecutors are also beneficiaries (d near 0.0-0.15) as they fulfill their mandate. Executed offenders and the wrongfully convicted are full targets (d near 1.0) as they bear the ultimate cost. Abolitionist advocates are excluded, and criminologists are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to deter capital crimes. Its status is 'contested' because empirical evidence for its unique deterrent effect is weak. If the deterrence function is found to be 'dead' (i.e., no unique deterrent effect), but the practice persists, it would indicate mandatrophy, where the constraint continues due to inertia or for other unstated reasons (e.g., retribution, which is a different reading of the kernel). The rising theater ratio and extractiveness, coupled with falling suppression requirement, suggest a drift towards a more performative and less functionally justified constraint over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_empirical,
    'Does capital punishment have a statistically significant deterrent effect on capital crimes beyond that of life imprisonment?',
    'Comprehensive, methodologically rigorous meta-analyses of criminological data across multiple jurisdictions and time periods, controlling for confounding variables.',
    'If a unique deterrent effect is empirically established, the extractiveness of the constraint would be re-evaluated downward, supporting its coordination function. If no unique effect is found, the extraction of life would be largely unjustified, pushing the constraint closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_empirical, empirical, 'Empirical validity of the deterrence claim.').

omega_variable(
    wrongful_conviction_error_rate,
    'What is the irreducible rate of wrongful convictions in capital cases, and how does this error rate impact the utilitarian justification for deterrence?',
    'Ongoing legal and forensic reviews of past capital cases, DNA exonerations, and statistical modeling of error probabilities in the criminal justice system.',
    'A non-zero and significant error rate fundamentally undermines the utilitarian calculus of deterrence, as the ''cost'' includes innocent lives, making the extraction morally indefensible from this reading''s own premises and pushing it towards a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wrongful_conviction_error_rate, empirical, 'Impact of wrongful convictions on deterrence justification.').

omega_variable(
    alternative_readings_impact,
    'How would the classification of state execution authority change under the ''retributive_reading'' or ''abolition_reading'' of the same kernel?',
    'Analysis of separate constraint stories for each sibling reading, comparing their base properties, stakeholder structures, and computed classifications.',
    'The ''retributive_reading'' would likely emphasize moral balance over deterrence, potentially leading to a different extractiveness profile. The ''abolition_reading'' would likely classify it as a Snare due to its categorical impermissibility, regardless of any claimed coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_impact, conceptual, 'Comparison of classifications across kernel readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (state power to execute) structural (legal authority, physical enforcement) or internalized (fear of state power, social norms against capital crimes)?',
    'Sociological studies on public perception of state power, psychological research on fear of punishment, and analysis of crime rates in jurisdictions with and without capital punishment.',
    'If suppression is largely internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the threat persists even without direct enforcement. If primarily structural, changes in legal authority would directly impact its suppressive force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__deterrence_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__deterrence_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__deterrence_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__deterrence_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__deterrence_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__deterrence_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__deterrence_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__deterrence_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__deterrence_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__deterrence_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__deterrence_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__deterrence_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__deterrence_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__deterrence_reading, suppression_requirement, 10, 0.88).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__deterrence_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__deterrence_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__deterrence_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__deterrence_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, criminal_sentencing_guidelines).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, prison_industrial_complex).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'state_execution_authority' kernel. Each reading has a different primary justification and structural properties, leading to different ε values and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
