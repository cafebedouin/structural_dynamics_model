% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Intrinsic Value of Human Life Prohibition (Sanctity Reading)
 *   domain: Medical Ethics / Bioethics / End-of-Life Policy
 *
 * SUMMARY:
 *   This constraint represents the 'sanctity of life' reading of the broader
 *   'end-of-life authority' kernel. It posits that human life has intrinsic
 *   value, prohibiting intentional life-ending regardless of individual
 *   preference. This reading is often codified in legal and ethical
 *   guidelines, actively enforced, and aims to protect vulnerable populations
 *   from perceived coercion, while simultaneously denying agency to those who
 *   seek assisted dying. It is a Tangled Rope because it claims a
 *   coordination function (protection of the vulnerable) but imposes
 *   significant extraction (denial of choice) through active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.85).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.9).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Intrinsic Value of Human Life Prohibition (Sanctity Reading)").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "Medical Ethics / Bioethics / End-of-Life Policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, '142dd42a-a57f-486f-aa87-294e9b0ad855').
narrative_ontology:cs_kernel_codification('142dd42a-a57f-486f-aa87-294e9b0ad855', formalized).
narrative_ontology:cs_authority_grounding('142dd42a-a57f-486f-aa87-294e9b0ad855', lineage).
narrative_ontology:cs_interpretation_layer_present('142dd42a-a57f-486f-aa87-294e9b0ad855').
narrative_ontology:cs_reading_relation('142dd42a-a57f-486f-aa87-294e9b0ad855', end_of_life_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('142dd42a-a57f-486f-aa87-294e9b0ad855', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('142dd42a-a57f-486f-aa87-294e9b0ad855', foundational, human_life_has_intrinsic_value).
narrative_ontology:cs_axiom_status(human_life_has_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('142dd42a-a57f-486f-aa87-294e9b0ad855', human_life_has_intrinsic_value, deontological).
narrative_ontology:cs_axiom('142dd42a-a57f-486f-aa87-294e9b0ad855', foundational, intentional_life_ending_is_morally_impermissible).
narrative_ontology:cs_axiom_status(intentional_life_ending_is_morally_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('142dd42a-a57f-486f-aa87-294e9b0ad855', intentional_life_ending_is_morally_impermissible, deontological).
narrative_ontology:cs_reference_frame('142dd42a-a57f-486f-aa87-294e9b0ad855', traditional_medical_ethics_life_preservation).
narrative_ontology:cs_drift_state('142dd42a-a57f-486f-aa87-294e9b0ad855', contemporary_autonomy_advocacy_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('142dd42a-a57f-486f-aa87-294e9b0ad855', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, pro_life_advocates).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, healthcare_systems).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, terminally_ill_patients_seeking_assisted_dying).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, elderly_disabled_vulnerable_at_coercion_risk).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, physicians_seeking_to_offer_assisted_dying).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Upholds and advocates for the intrinsic value of human life from conception to natural death, viewing intentional life-ending as morally impermissible. Actively lobbies for legal prohibitions and influences public discourse.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, religious_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefits from the legal and moral framework that prohibits intentional life-ending, aligning with their core beliefs. They see this as protecting vulnerable lives and upholding a fundamental moral principle.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, pro_life_advocates, beneficiary,
    organized, biographical, identity_locked, national).

% Benefits from a clear, categorical prohibition that simplifies complex ethical and legal dilemmas around end-of-life care, limiting the scope of medical practice to life preservation and palliative care, avoiding the need to develop frameworks for assisted dying.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, healthcare_systems, beneficiary,
    institutional, generational, constrained, national).

% Responsible for codifying and enforcing laws that reflect or challenge this prohibition. They face pressure from both pro-life and autonomy advocates, often navigating a contested political landscape.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, legislators, agenda_setter,
    institutional, biographical, constrained, national).

% Bears the direct cost of this prohibition by being denied the option to end their suffering on their own terms. Their choices are limited to continued palliative care or natural death, even if suffering is unbearable.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, terminally_ill_patients_seeking_assisted_dying, payer,
    powerless, immediate, trapped, local).

% Are theoretically protected by the prohibition from perceived pressure to end their lives, but also lose the option of self-determination if they genuinely desire it. Their agency is constrained by the categorical rule.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, elderly_disabled_vulnerable_at_coercion_risk, payer,
    powerless, biographical, trapped, local).

% Faces legal and ethical constraints that prevent them from offering assisted dying, even when it aligns with a patient's wishes and their own professional judgment. Their role is strictly limited to life preservation and palliation.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, physicians_seeking_to_offer_assisted_dying, payer,
    moderate, biographical, constrained, national).

% Actively campaigns for individual self-determination in end-of-life decisions but is structurally excluded from the core premise of this constraint, which prioritizes collective moral principles over individual choice. Their arguments are often framed as a threat to the vulnerable.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, autonomy_advocates, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__sanctity_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate societal values around the intrinsic worth of human life, preventing its perceived devaluation and protecting vulnerable individuals (elderly, disabled, economically disadvantaged) from potential coercion or pressure to end their lives.
% TRANSFER_FUNCTION: Transfers the ultimate authority over the timing and manner of death from the individual to a collective moral and legal framework, imposing a categorical prohibition on intentional life-ending. This transfers the burden of continued life onto individuals who may wish to end suffering.
% ABSENT_VOICES: Individuals experiencing unbearable suffering who wish to exercise agency over their death, and physicians who believe in offering assisted dying as a compassionate option, are often marginalized or framed as misguided within this framework. Autonomy advocates are actively campaigning but are not at the table where the core premise is set.
% DISAPPEARANCE_RATIONALE: If this categorical prohibition vanished overnight, legal and ethical frameworks for assisted dying would rapidly emerge or expand in many jurisdictions. End-of-life care policies, medical practice, and societal norms around death would undergo significant reorganization, with a greater emphasis on individual choice.
% FOUNDING_PROBLEM: To prevent the devaluation of human life, protect vulnerable individuals from perceived pressure or coercion to end their lives, and uphold a moral order based on the intrinsic and inviolable worth of every human life.
% FOUNDING_PROBLEM_CORROBORATION: Religious texts, long-standing philosophical traditions, and some bioethical schools of thought consistently corroborate this founding problem. Opponents, however, argue that while protection of the vulnerable is a valid concern, the categorical prohibition is an overreach that denies individual agency and reframes the problem to justify control.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__sanctity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because it imposes a categorical prohibition that denies individuals the choice to end their suffering, even when facing unbearable conditions. Suppression is very high (0.90) due to legal prohibitions, professional ethical codes, and strong societal norms that actively prevent assisted dying and suppress advocacy for it. Theater ratio is low (0.10) because the prohibition is genuinely held and actively enforced; there is little performative maintenance without real function. Accessibility collapse is near total (0.95) as the option of assisted dying is almost entirely removed. Resistance is high (0.70) due to ongoing advocacy for individual autonomy and the legalization of assisted dying.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, this constraint is a protective measure, a moral imperative safeguarding human dignity and preventing societal harms. From the perspective of victims, it is an oppressive imposition that denies fundamental autonomy and prolongs suffering, often under the guise of protection. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutions, pro-life advocates, and healthcare systems are beneficiaries, as the constraint aligns with their moral frameworks, simplifies ethical dilemmas, and reinforces their institutional roles. Terminally ill patients seeking assisted dying, vulnerable populations at risk of coercion, and physicians seeking to offer assisted dying are victims, as their choices and professional agency are curtailed by the categorical prohibition.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not mandatrophied; its mandate (upholding the sanctity of life and protecting the vulnerable) is actively and vigorously pursued by its beneficiaries. The high suppression and low theater ratio indicate active maintenance, not inertial decay. The contest is over the validity and scope of its founding problem, not its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''end_of_life_authority'' kernel, or is it merely a policy outcome of a more fundamental moral constraint?',
    'Analysis of the internal coherence and independent grounding of the ''sanctity of life'' principle as a distinct commitment system, separate from its policy manifestations.',
    'If it''s a distinct reading, its classification stands. If it''s merely a policy outcome, the underlying moral constraint would be the primary object of analysis, and this constraint would be reclassified as a derivative Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Clarifies whether this is a kernel reading or a policy derivative.').

omega_variable(
    vulnerability_coercion_empirical_basis,
    'To what extent is the ''protection of the vulnerable from coercion'' an empirically substantiated risk, versus a rhetorical justification for a categorical prohibition?',
    'Comparative empirical studies from jurisdictions with and without assisted dying laws, analyzing rates of coercion, abuse, and the demographic profiles of those seeking assisted dying.',
    'If coercion risks are low or manageable, the ''coordination'' function of this Tangled Rope would be significantly weakened, pushing it closer to a pure Snare. If risks are high, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerability_coercion_empirical_basis, empirical, 'Assesses the empirical basis for the ''protection of the vulnerable'' claim.').

omega_variable(
    definition_of_life_ending,
    'Does ''intentional life-ending'' include the withdrawal of life-sustaining treatment, or is it strictly limited to active interventions?',
    'Legal and ethical consensus on the distinction between ''killing'' and ''letting die'' in medical practice, and the scope of ''intentionality'' in end-of-life decisions.',
    'If withdrawal of treatment is included, the constraint''s scope and suppressive force are significantly broader, impacting a wider range of medical decisions. If excluded, its scope is narrower, allowing more patient autonomy in passive decisions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definition_of_life_ending, conceptual, 'Clarifies the definitional boundary of ''intentional life-ending''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t1980, end_of_life_authority__sanctity_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(end__tr_t1990, end_of_life_authority__sanctity_reading, theater_ratio, 1990, 0.11).
narrative_ontology:measurement(end__tr_t2000, end_of_life_authority__sanctity_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(end__tr_t2010, end_of_life_authority__sanctity_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(end__tr_t2024, end_of_life_authority__sanctity_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(end__be_t1980, end_of_life_authority__sanctity_reading, base_extractiveness, 1980, 0.8).
narrative_ontology:measurement(end__be_t1990, end_of_life_authority__sanctity_reading, base_extractiveness, 1990, 0.82).
narrative_ontology:measurement(end__be_t2000, end_of_life_authority__sanctity_reading, base_extractiveness, 2000, 0.83).
narrative_ontology:measurement(end__be_t2010, end_of_life_authority__sanctity_reading, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(end__be_t2024, end_of_life_authority__sanctity_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1980, end_of_life_authority__sanctity_reading, suppression_requirement, 1980, 0.85).
narrative_ontology:measurement(end__su_t1990, end_of_life_authority__sanctity_reading, suppression_requirement, 1990, 0.87).
narrative_ontology:measurement(end__su_t2000, end_of_life_authority__sanctity_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(end__su_t2010, end_of_life_authority__sanctity_reading, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(end__su_t2024, end_of_life_authority__sanctity_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
