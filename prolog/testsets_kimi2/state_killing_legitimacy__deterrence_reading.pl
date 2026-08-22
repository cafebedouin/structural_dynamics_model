% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: state_killing_legitimacy__deterrence_reading
 *   human_readable: State Killing Legitimacy â Deterrence Reading
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint story models the deterrence reading of state execution
 *   legitimacy: the claim that capital punishment is justified because it
 *   prevents future murders by altering the rational calculus of would-be
 *   offenders. Under this reading, the condemned offender is instrumentalized
 *   as a means to a social end, prospective homicide victims are the nominal
 *   beneficiary class, and the constraint's persistence depends on active
 *   carceral and judicial enforcement. The empirical foundation is
 *   contestedâpanel studies and meta-analyses disagree on whether execution
 *   rates suppress homicide ratesâproducing moderate extractiveness and a
 *   structural ambiguity between genuine coordination and state-administered
 *   extraction.
 *
 * KEY AGENTS:
 *   - state_execution_apparatus: Primary agenda-setter (institutional/analytical) â administers capital punishment and frames it as public safety.
 *   - condemned_offenders: Primary target (powerless/trapped) â bears the ultimate extraction.
 *   - prospective_homicide_victims: Nominal beneficiary (moderate/constrained) â receives claimed deterrence benefit.
 *   - abolitionist_movement: Excluded voice (organized/analytical) â contests the empirical and moral basis.
 *   - criminological_research_community: Analytical observer (institutional/analytical) â generates contested empirical findings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.58).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.72).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "State Killing Legitimacy â Deterrence Reading").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, '2ad329dd-19bb-4443-b72f-080974d8cdf2').
narrative_ontology:cs_kernel_codification('2ad329dd-19bb-4443-b72f-080974d8cdf2', formalized).
narrative_ontology:cs_authority_grounding('2ad329dd-19bb-4443-b72f-080974d8cdf2', lineage).
narrative_ontology:cs_interpretation_layer_present('2ad329dd-19bb-4443-b72f-080974d8cdf2').
narrative_ontology:cs_reading_relation('2ad329dd-19bb-4443-b72f-080974d8cdf2', state_killing_legitimacy__abolition_reading, forecloses).
narrative_ontology:cs_reading_relation('2ad329dd-19bb-4443-b72f-080974d8cdf2', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_axiom('2ad329dd-19bb-4443-b72f-080974d8cdf2', foundational, execution_prevents_future_homicide_net).
narrative_ontology:cs_axiom_status(execution_prevents_future_homicide_net, holdable).
narrative_ontology:cs_axiom_grounding('2ad329dd-19bb-4443-b72f-080974d8cdf2', execution_prevents_future_homicide_net, empirically_contingent).
narrative_ontology:cs_axiom('2ad329dd-19bb-4443-b72f-080974d8cdf2', foundational, offender_instrumentalization_permissible).
narrative_ontology:cs_axiom_status(offender_instrumentalization_permissible, holdable).
narrative_ontology:cs_axiom_grounding('2ad329dd-19bb-4443-b72f-080974d8cdf2', offender_instrumentalization_permissible, instrumental).
narrative_ontology:cs_reference_frame('2ad329dd-19bb-4443-b72f-080974d8cdf2', rational_penal_deterrence).
narrative_ontology:cs_drift_state('2ad329dd-19bb-4443-b72f-080974d8cdf2', contemporary_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2ad329dd-19bb-4443-b72f-080974d8cdf2', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, prospective_homicide_victims).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, condemned_offenders).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, deterrence_hypothesis).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, rational_actor_theory).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, state_monopoly_on_legitimate_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers capital sentencing and execution through statutory criminal law, prosecutorial charging decisions, and carceral institutions. Frames the practice as a rational public-safety signal and funds the appellate and lethal-infrastructure pipeline. Can reform or abolish the practice through legislative or administrative action but currently maintains it.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Bear the irreversible cost of the constraint: once sentenced, they are confined on death row while appeals run, with procedural exits narrowing over time. They are instrumentalized as a means to the claimed social end of future homicide prevention and do not consent to the role.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, condemned_offenders, payer,
    powerless, immediate, trapped, local).

% Receive a claimed, probabilistic safety benefit from the deterrent effect of execution on would-be murderers. They do not choose the constraint, cannot opt out of the jurisdiction, and have no direct receipt of the extraction; the benefit is diffuse and empirically contested.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, prospective_homicide_victims, beneficiary,
    moderate, biographical, constrained, national).

% Argues that state killing is categorically impermissible and that the deterrence hypothesis is empirically unsupported. Their voices are formally acknowledged in democratic discourse but are treated as external moral objections rather than as internal corrections to the cost-benefit framework that justifies the constraint.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, abolitionist_movement, excluded,
    organized, generational, analytical, national).

% Produces panel studies, meta-analyses, and natural experiments on the marginal deterrent effect of execution. Findings are deeply contested: some report null or negative effects, others report small positive effects. Their work is the epistemic foundation of the constraint's coordination claim but does not adjudicate it.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, criminological_research_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents future homicides by imposing a cost (forfeiture of life) that exceeds the prospective gain of murder, thereby shifting the rational calculus of would-be offenders and stabilizing public safety expectations through a credible signal of maximal state sanction.
% TRANSFER_FUNCTION: Transfers the life of the condemned offender to the claimed security of the public, mediated by the state's monopoly on legitimate lethal force and justified by an expected reduction in future victimization.
% ABSENT_VOICES: The condemned themselves are silenced by the sentence; abolitionist movements and international human rights bodies are present in discourse but excluded from the deterrence reading's internal cost-benefit calculus, treated as raising external moral noise rather than refuting the empirical premise.
% DISAPPEARANCE_RATIONALE: If the deterrence justification vanished as a legitimating frame, capital statutes would be repealed or narrowed, prosecutorial charging patterns would shift to life-without-parole, the death-row carceral apparatus would close, and the empirical research agenda would migrate to non-lethal sanctions.
% FOUNDING_PROBLEM: Chronic interpersonal lethal violence that informal social controls and lesser criminal penalties fail to prevent; the perceived need for a sanction severe enough to credibly alter the rational choice of would-be murderers.
% FOUNDING_PROBLEM_CORROBORATION: Independent criminologists and the National Research Council (outside the beneficiary and agenda-setting classes) attest that the evidence for a marginal deterrent effect is not reliable; prosecutorial associations and law-enforcement lobbies (inside the agenda-setting class) assert the problem is still live.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__deterrence_reading, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.58) is moderate because the deterrence claim is structurally genuineâif empirically valid, the constraint would coordinate public safetyâbut the evidence is contested and the offender bears an irreversible, total cost. Suppression (0.72) is high because the constraint requires active judicial, carceral, and lethal enforcement to hold, and alternatives (commutation, abolition) are structurally blocked by legal finality. Accessibility collapse (0.60) reflects that once a death sentence is finalized, procedural exits narrow drastically; resistance (0.55) captures sustained abolitionist, legal, and international opposition. Theater ratio (0.25) is moderate: modern executions are bureaucratized, but the surrounding sentencing rhetoric retains performative signaling value. The measurement series tracks a slow rise in extractiveness and suppression as empirical challenges accumulate but the enforcement apparatus hardens.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state apparatus) experiences the constraint as a necessary enforcement tool for public safety; the payer seat (condemned offender) experiences it as total instrumentalization and extraction. The beneficiary seat (prospective victims) experiences a contested, probabilistic safety gain. The engine computes these divergent classifications from the same structural data: the state apparatus and prospective victims may compute toward coordination, while the condemned offender computes toward extraction or snare, producing the tangled-rope profile at the story level.
 *
 * DIRECTIONALITY LOGIC:
 *   The state execution apparatus sits near the beneficiary end of directionality (low d): it administers the constraint and does not bear its personal costs, though it invests institutional resources. Prospective homicide victims sit at low-to-moderate d: they are the claimed beneficiaries of the deterrent effect, but the benefit is probabilistic and diffuse. Condemned offenders sit at the full-target end (d near 1.0): they bear the total, irreversible cost of the constraint and have no exit. The abolitionist movement and research community occupy analytical seats with no direct cost or benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The deterrence reading prevents mislabeling pure extraction by requiring a coordination function: the prevention of future homicides. If the empirical claim were fully vindicated, the constraint would remain tangled rope (genuine coordination + irreducible offender cost) but not snare. If the empirical claim is falsified and the constraint persists, the coordination function dies and the constraint drifts toward piton (theatrical maintenance of a dead function) or snare (state killing as pure extraction of institutional legitimacy). The founding problemâuncontrolled lethal violenceâmay be live, but the specific solution (execution) is contested, so the R5 genealogy flags possible mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_empirical_foundation,
    'Does executing offenders produce a marginal deterrent effect beyond non-lethal sanctions such as life without parole?',
    'Comparative panel studies across jurisdictions with and without capital punishment, controlling for socioeconomic confounders, and meta-analyses published by independent statistical bodies.',
    'If the effect is zero or negative, the coordination claim collapses and the constraint reclassifies toward snare or piton; if positive and significant, the tangled_rope classification stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_empirical_foundation, empirical, 'Empirical foundation of the deterrence reading''s coordination claim.').

omega_variable(
    offender_instrumentalization_status,
    'Is the condemned offender''s death a necessary cost of the coordination function, or is it extractive surplus that could be avoided with equivalent non-lethal sanctions?',
    'Comparative criminological analysis measuring homicide rates in jurisdictions before and after replacing capital punishment with life-without-parole.',
    'If non-lethal sanctions produce equivalent deterrence, the offender''s execution is surplus extraction; if not, it is an unavoidable coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(offender_instrumentalization_status, conceptual, 'Whether offender death is coordination cost or extractive surplus.').

omega_variable(
    kernel_reading_separability,
    'Is the deterrence reading structurally separable from the retributive reading within penal practice, or do they function as an inseparable hybrid in actual sentencing justification?',
    'Discourse analysis of judicial opinions and prosecutorial charging documents to determine whether deterrence and retributive claims appear independently or only in combination.',
    'If inseparable, the constraint is a hybrid kernel reading that cannot be evaluated in isolation; if separable, the pure deterrence reading is a valid standalone constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_separability, conceptual, 'Structural separability of deterrence and retributive justifications in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__deterrence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t10, state_killing_legitimacy__deterrence_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(stat_tr_t20, state_killing_legitimacy__deterrence_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(stat_tr_t30, state_killing_legitimacy__deterrence_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__deterrence_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(stat_tr_t50, state_killing_legitimacy__deterrence_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__deterrence_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(stat_be_t10, state_killing_legitimacy__deterrence_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(stat_be_t20, state_killing_legitimacy__deterrence_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(stat_be_t30, state_killing_legitimacy__deterrence_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__deterrence_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement(stat_be_t50, state_killing_legitimacy__deterrence_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__deterrence_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(stat_su_t10, state_killing_legitimacy__deterrence_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(stat_su_t20, state_killing_legitimacy__deterrence_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(stat_su_t30, state_killing_legitimacy__deterrence_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__deterrence_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(stat_su_t50, state_killing_legitimacy__deterrence_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the state_killing_legitimacy kernel, decomposed from the natural-language concept of capital punishment justification per the Îµ-invariance principle. Sibling readings (retributive_reading, abolition_reading) share the same kernel but instantiate structurally distinct constraints with different beneficiary/victim structures and Îµ values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
