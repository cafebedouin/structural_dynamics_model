% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__deterrence_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: state_killing_legitimacy__deterrence_reading
 *   human_readable: State Killing Justified by Deterrence (Deterrence Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the 'deterrence reading' of state-sanctioned
 *   killing, where execution is justified as a rational signal to prevent
 *   future murders. It instrumentalizes the convicted offender as a means to
 *   a social end (public safety). The empirical evidence for this deterrent
 *   effect is highly contested, leading to a moderate-to-high extractiveness
 *   score despite the claimed coordination function. The constraint is
 *   actively enforced by the state, and resistance from abolitionist
 *   movements is significant.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.65).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.55).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "State Killing Justified by Deterrence (Deterrence Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, '40e77880-7e3f-40d7-8673-957a0d79a4ef').
narrative_ontology:cs_kernel_codification('40e77880-7e3f-40d7-8673-957a0d79a4ef', formalized).
narrative_ontology:cs_authority_grounding('40e77880-7e3f-40d7-8673-957a0d79a4ef', lineage).
narrative_ontology:cs_interpretation_layer_present('40e77880-7e3f-40d7-8673-957a0d79a4ef').
narrative_ontology:cs_reading_relation('40e77880-7e3f-40d7-8673-957a0d79a4ef', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('40e77880-7e3f-40d7-8673-957a0d79a4ef', state_killing_legitimacy__abolition_reading, forecloses).
narrative_ontology:cs_axiom('40e77880-7e3f-40d7-8673-957a0d79a4ef', foundational, execution_deters_future_crimes).
narrative_ontology:cs_axiom_status(execution_deters_future_crimes, holdable).
narrative_ontology:cs_axiom_grounding('40e77880-7e3f-40d7-8673-957a0d79a4ef', execution_deters_future_crimes, empirically_contingent).
narrative_ontology:cs_axiom('40e77880-7e3f-40d7-8673-957a0d79a4ef', foundational, state_has_right_to_protect_citizens_via_deterrence).
narrative_ontology:cs_axiom_status(state_has_right_to_protect_citizens_via_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('40e77880-7e3f-40d7-8673-957a0d79a4ef', state_has_right_to_protect_citizens_via_deterrence, instrumental).
narrative_ontology:cs_reference_frame('40e77880-7e3f-40d7-8673-957a0d79a4ef', utilitarian_social_protection_framework).
narrative_ontology:cs_drift_state('40e77880-7e3f-40d7-8673-957a0d79a4ef', contemporary_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('40e77880-7e3f-40d7-8673-957a0d79a4ef', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, state_prosecutors).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, public_order_advocates).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, convicted_offenders).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, abolitionist_advocates).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, utilitarian_justice_theory).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, social_contract_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for capital punishment as a necessary tool for public safety and order, believing it deters heinous crimes. They administer the legal process leading to execution and benefit from the perceived effectiveness of this ultimate sanction.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, state_prosecutors, agenda_setter,
    institutional, biographical, constrained, national).

% Are the direct targets of the constraint, losing their lives as a 'rational signal' to others. They have no exit options from the legal system once convicted and sentenced to death.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, convicted_offenders, payer,
    powerless, immediate, trapped, local).

% Are the theoretical beneficiaries of the deterrence mechanism, as their lives are purportedly saved by the deterrent effect of executions. This benefit is diffuse and difficult to quantify.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, potential_future_victims, beneficiary,
    powerless, generational, analytical, national).

% Bear the moral and emotional cost of state-sanctioned killing, which they view as a violation of human rights. They are actively excluded from the decision-making process regarding capital punishment, despite their organized efforts to oppose it.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, abolitionist_advocates, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__deterrence_reading, abolitionist_advocates, excluded).

% Support capital punishment as a means to maintain social stability and order, believing it reinforces the gravity of certain crimes and provides a sense of justice. They benefit from the perceived deterrent effect and the symbolic assertion of state power.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, public_order_advocates, beneficiary,
    organized, biographical, mobile, national).

% Analyze the legal, ethical, and empirical aspects of capital punishment, often scrutinizing the evidence for deterrence. They do not directly participate in its enforcement or suffer its direct consequences, but their analysis influences public and legal discourse.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__deterrence_reading, diffuse).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__deterrence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To deter potential murderers by demonstrating severe consequences for capital crimes, thereby coordinating social behavior towards non-violence and reinforcing the state's authority.
% TRANSFER_FUNCTION: Transfers the life of the convicted offender for the perceived safety of potential future victims and the maintenance of public order, as well as the symbolic assertion of state power.
% ABSENT_VOICES: Those who advocate for the inherent dignity of all human life, regardless of crime, and for rehabilitative or restorative justice. They are often marginalized in public discourse and legislative processes concerning capital punishment.
% DISAPPEARANCE_RATIONALE: If capital punishment and its deterrence justification vanished overnight, the legal and moral landscape of criminal justice would fundamentally change. States would need to rely solely on life imprisonment, and the symbolic power of execution as a deterrent would be absent, leading to a reorganization of legal strategies and public discourse on crime and punishment.
% FOUNDING_PROBLEM: To establish a supreme deterrent against heinous crimes, particularly murder, and to ensure public safety and order in a society where lesser punishments were deemed insufficient to prevent such acts.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (state prosecutors, some public officials) attest the problem of deterring heinous crimes is still live and requires capital punishment. Opponents (abolitionist groups, many legal scholars, international human rights bodies) contest its efficacy and moral legitimacy, citing empirical studies that often find no significant deterrent effect, supporting a shifted-function reading.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__deterrence_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the ultimate cost (life) is borne by the offender, and the empirical basis for the claimed benefit (deterrence) is weak and contested. Suppression (0.55) reflects the state's legal and political efforts to maintain capital punishment against opposition, but alternatives (life imprisonment) are widely available and debated. Theater ratio (0.40) indicates a growing performative aspect, where the symbolic assertion of justice and state power may outweigh the functional deterrent effect, especially as empirical support wanes. Resistance is high (0.80) due to strong, organized opposition.
 *
 * PERSPECTIVAL GAP:
 *   Proponents (state prosecutors, public order advocates) view capital punishment as a necessary, effective, and just deterrent, essential for public safety. Opponents (abolitionist advocates, many legal scholars) see it as a cruel, ineffective, and morally indefensible act that instrumentalizes human life and fails to achieve its stated coordination goal. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Convicted offenders are the full targets (d=1.0) as they bear the ultimate cost. Potential future victims and public order advocates are beneficiaries (d near 0.0) of the perceived deterrence. State prosecutors are agenda-setters and beneficiaries, gaining a powerful tool. Abolitionist advocates are payers (d near 1.0) as they bear the moral cost and face suppression of their views.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_deterrence_efficacy,
    'Does capital punishment actually deter murder more effectively than life imprisonment?',
    'Comprehensive, long-term, cross-jurisdictional empirical studies controlling for confounding variables, or a natural experiment from a jurisdiction abolishing capital punishment.',
    'If no significant deterrent effect is found, the coordination function of the constraint collapses, reclassifying it closer to a Snare. If a strong effect is found, it would strengthen the Rope aspect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_deterrence_efficacy, empirical, 'Uncertainty regarding the actual deterrent effect of capital punishment.').

omega_variable(
    moral_instrumentalization_legitimacy,
    'Is it morally legitimate to instrumentalize a human being (the convicted offender) as a means to a social end (deterrence)?',
    'Philosophical consensus on deontological ethics versus utilitarianism, or a shift in societal moral norms regarding human dignity.',
    'If instrumentalization is deemed illegitimate, the moral foundation of the deterrence reading collapses, pushing the constraint towards a Snare regardless of empirical efficacy. If deemed legitimate, the constraint''s moral standing is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_instrumentalization_legitimacy, conceptual, 'Ambiguity regarding the moral legitimacy of instrumentalizing offenders for deterrence.').

omega_variable(
    suppression_of_alternatives_necessity,
    'Is the suppression of life imprisonment as a sufficient alternative to capital punishment justified by the deterrence claim?',
    'Empirical evidence demonstrating the unique deterrent value of execution, or a legal/political shift accepting life imprisonment as an equivalent or superior alternative.',
    'If life imprisonment is proven to be an equally effective alternative, the suppression of this option becomes purely extractive, increasing the Snare-like qualities of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternatives_necessity, empirical, 'Whether the suppression of alternative punishments is justified by the deterrence claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_killing_legitimacy__deterrence_reading, theater_ratio, 1976, 0.25).
narrative_ontology:measurement(stat_tr_t1988, state_killing_legitimacy__deterrence_reading, theater_ratio, 1988, 0.3).
narrative_ontology:measurement(stat_tr_t2000, state_killing_legitimacy__deterrence_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(stat_tr_t2012, state_killing_legitimacy__deterrence_reading, theater_ratio, 2012, 0.4).
narrative_ontology:measurement(stat_tr_t2024, state_killing_legitimacy__deterrence_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_killing_legitimacy__deterrence_reading, base_extractiveness, 1976, 0.55).
narrative_ontology:measurement(stat_be_t1988, state_killing_legitimacy__deterrence_reading, base_extractiveness, 1988, 0.6).
narrative_ontology:measurement(stat_be_t2000, state_killing_legitimacy__deterrence_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(stat_be_t2012, state_killing_legitimacy__deterrence_reading, base_extractiveness, 2012, 0.66).
narrative_ontology:measurement(stat_be_t2024, state_killing_legitimacy__deterrence_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_killing_legitimacy__deterrence_reading, suppression_requirement, 1976, 0.5).
narrative_ontology:measurement(stat_su_t1988, state_killing_legitimacy__deterrence_reading, suppression_requirement, 1988, 0.58).
narrative_ontology:measurement(stat_su_t2000, state_killing_legitimacy__deterrence_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(stat_su_t2012, state_killing_legitimacy__deterrence_reading, suppression_requirement, 2012, 0.58).
narrative_ontology:measurement(stat_su_t2024, state_killing_legitimacy__deterrence_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, criminal_justice_system_legitimacy).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, human_rights_norms).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'state_killing_legitimacy' kernel, focusing on deterrence. The other readings (retributive and abolition) offer alternative justifications or rejections of state killing, forming a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
