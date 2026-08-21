% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__sanctity_reading, []).

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
 *   constraint_id: end_of_life_decision_authority__sanctity_reading
 *   human_readable: Intrinsic Value of Human Life (Sanctity Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint represents the 'sanctity of life' reading of end-of-life
 *   decision authority, asserting that human life possesses intrinsic value
 *   independent of individual will, and that intentional life-ending violates
 *   this value. It is a foundational principle in many ethical and legal
 *   systems, particularly in medical ethics. This story instantiates one
 *   reading of the 'end_of_life_decision_authority' kernel, focusing on its
 *   implications for individuals and institutions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.68).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.75).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, rope).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Intrinsic Value of Human Life (Sanctity Reading)").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, '43d0375b-8459-4dc5-aff3-21d3290af548').
narrative_ontology:cs_kernel_codification('43d0375b-8459-4dc5-aff3-21d3290af548', formalized).
narrative_ontology:cs_authority_grounding('43d0375b-8459-4dc5-aff3-21d3290af548', lineage).
narrative_ontology:cs_interpretation_layer_present('43d0375b-8459-4dc5-aff3-21d3290af548').
narrative_ontology:cs_reading_relation('43d0375b-8459-4dc5-aff3-21d3290af548', end_of_life_decision_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('43d0375b-8459-4dc5-aff3-21d3290af548', end_of_life_decision_authority__vulnerability_protection_reading, coexists_with).
narrative_ontology:cs_axiom('43d0375b-8459-4dc5-aff3-21d3290af548', foundational, human_life_intrinsic_value).
narrative_ontology:cs_axiom_status(human_life_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('43d0375b-8459-4dc5-aff3-21d3290af548', human_life_intrinsic_value, deontological).
narrative_ontology:cs_axiom('43d0375b-8459-4dc5-aff3-21d3290af548', secondary, intentional_life_ending_violates_value).
narrative_ontology:cs_axiom_status(intentional_life_ending_violates_value, holdable).
narrative_ontology:cs_axiom_grounding('43d0375b-8459-4dc5-aff3-21d3290af548', intentional_life_ending_violates_value, deontological).
narrative_ontology:cs_reference_frame('43d0375b-8459-4dc5-aff3-21d3290af548', absolute_prohibition_on_killing).
narrative_ontology:cs_drift_state('43d0375b-8459-4dc5-aff3-21d3290af548', contemporary_autonomy_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('43d0375b-8459-4dc5-aff3-21d3290af548', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, society_as_a_whole).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, medical_profession).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, vulnerable_patients).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, suffering_patients).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, autonomy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the perceived moral order and the upholding of a fundamental respect for human life, which is seen as a cornerstone of ethical society. This perspective views the constraint as protecting a universal good.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, society_as_a_whole, beneficiary,
    institutional, civilizational, analytical, global).

% Operates within a clear ethical framework that prioritizes preserving life and alleviating suffering, but not intentionally ending life. This provides role clarity and protects against moral injury from participating in life-ending acts. They enforce the constraint through professional codes and practices.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, medical_profession, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, medical_profession, beneficiary).

% Bear the cost of prolonged suffering when their desire for an intentional end to life is denied. Their autonomy over their own death is suppressed by the principle, leading to a feeling of being trapped by their condition and the medical system.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, suffering_patients, payer,
    powerless, immediate, trapped, local).

% Actively resist the constraint, viewing it as an infringement on individual liberty and the right to self-determination. They advocate for legal and ethical frameworks that permit medical aid in dying or euthanasia for competent adults.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, autonomy_advocates, payer,
    organized, biographical, constrained, national).

% Are seen as protected by this constraint from potential coercion or pressure to end their lives, especially if they are elderly, disabled, or otherwise dependent. The constraint ensures their lives are not devalued or prematurely ended by others.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, vulnerable_patients, beneficiary,
    powerless, immediate, trapped, local).

% Analyze the ethical implications of the sanctity of life principle in contemporary medical practice, weighing its benefits against the demands of patient autonomy and the reality of suffering. They do not directly enforce or pay, but their analysis influences policy and public discourse.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, bioethicists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__sanctity_reading, society_as_a_whole).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared moral and legal boundary against intentional life-ending, providing a consistent ethical framework for medical practice and societal norms regarding the value of human life.
% TRANSFER_FUNCTION: Transfers the ultimate authority over life-ending decisions from the individual to a collective moral principle, imposing a duty to preserve life and prohibiting medical professionals from participating in its intentional termination.
% ABSENT_VOICES: Individuals who believe in absolute personal sovereignty over death, or those whose suffering is so profound that they prioritize relief through life-ending options, are often marginalized or excluded from the policy-making conversation where this principle is dominant.
% DISAPPEARANCE_RATIONALE: If the principle of intrinsic life value and its prohibition on intentional life-ending vanished, societies would face a profound re-evaluation of medical ethics, legal frameworks, and cultural norms around death. Medical aid in dying and euthanasia would likely become widely available, fundamentally altering the role of physicians and the experience of dying.
% FOUNDING_PROBLEM: The founding problem was to prevent arbitrary killing, protect the weak and vulnerable from being coerced into ending their lives, and uphold a fundamental respect for human life against utilitarian or instrumentalist views.
% FOUNDING_PROBLEM_CORROBORATION: Religious institutions, some medical ethics bodies, and disability rights advocates consistently attest that the founding problems of protecting vulnerable lives and preventing devaluation of human life remain live and pressing concerns in contemporary society.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__sanctity_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_decision_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the constraint denies individuals the choice to end their suffering, imposing a continued existence against their will. Suppression is also high, as legal and professional frameworks actively prohibit and punish intentional life-ending. Accessibility collapse is near total for those seeking to end their lives. Resistance is substantial from autonomy advocates. Theater ratio is low, as the principle is genuinely held and enforced, not merely performative. The claimed type is 'rope' because proponents frame it as a coordination mechanism for moral order and protection, even as its operation is highly extractive.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'society_as_a_whole' and the 'medical_profession', this constraint functions as a protective 'rope', upholding a fundamental moral good and providing clear ethical boundaries. However, from the perspective of 'suffering_patients' and 'autonomy_advocates', it operates as a 'snare', trapping individuals in unwanted suffering and suppressing their fundamental right to self-determination.
 *
 * DIRECTIONALITY LOGIC:
 *   Society and the medical profession are beneficiaries, gaining moral order and role clarity. Vulnerable patients are also beneficiaries, protected from coercion. Suffering patients and autonomy advocates are victims, bearing the cost of denied choice and prolonged suffering. The constraint's active enforcement targets those who would facilitate or seek intentional life-ending.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    is_life_inherently_valuable,
    'Is human life''s intrinsic value truly independent of individual will, or is its value partly constituted by the individual''s experience and desire?',
    'Philosophical and theological debate, societal consensus shifts over generations.',
    'If value is partly constituted by individual experience, the constraint''s justification for overriding individual will weakens, potentially reclassifying it as more extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(is_life_inherently_valuable, conceptual, 'The philosophical grounding of life''s intrinsic value.').

omega_variable(
    coercion_vs_protection,
    'Is the constraint primarily protecting vulnerable individuals from coercion, or is it coercing suffering individuals into unwanted prolonged existence?',
    'Empirical studies on the prevalence of coercion in jurisdictions with legal euthanasia, and qualitative studies on the experiences of suffering patients denied end-of-life options.',
    'If coercion of the suffering is found to be dominant, the constraint''s ''beneficiary'' claims for vulnerable patients would be undermined, increasing its effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_protection, empirical, 'The primary effect of the constraint: protection or coercion.').

omega_variable(
    sanctity_autonomy_conflict,
    'This constraint is the sanctity_reading of the end_of_life_decision_authority kernel. The autonomy_reading would shift ultimate authority to the individual, changing the victim set and reducing extraction for suffering patients.',
    'Legal and ethical reforms that prioritize individual self-determination in end-of-life decisions.',
    'If the autonomy_reading were adopted, this constraint would be superseded, and the classification of end-of-life decision-making would shift to reflect individual choice as the primary driver.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanctity_autonomy_conflict, conceptual, 'Conflict between sanctity and autonomy readings of end-of-life authority.').

omega_variable(
    sanctity_vulnerability_conflict,
    'This constraint is the sanctity_reading of the end_of_life_decision_authority kernel. The vulnerability_protection_reading would distribute authority across institutional checkpoints to prevent both denial and coercion, potentially altering the enforcement mechanisms and reducing suppression.',
    'Implementation and evaluation of distributed authority models in end-of-life care, such as multi-disciplinary review boards.',
    'If the vulnerability_protection_reading were adopted, the constraint''s enforcement would become more nuanced, potentially reducing suppression while maintaining protective functions, leading to a less extractive classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctity_vulnerability_conflict, conceptual, 'Conflict between sanctity and vulnerability protection readings of end-of-life authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t1950, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(end__tr_t1965, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(end__tr_t1980, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1980, 0.09).
narrative_ontology:measurement(end__tr_t1995, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1995, 0.09).
narrative_ontology:measurement(end__tr_t2010, end_of_life_decision_authority__sanctity_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(end__tr_t2025, end_of_life_decision_authority__sanctity_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(end__be_t1950, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(end__be_t1965, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1965, 0.58).
narrative_ontology:measurement(end__be_t1980, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1980, 0.62).
narrative_ontology:measurement(end__be_t1995, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1995, 0.65).
narrative_ontology:measurement(end__be_t2010, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(end__be_t2025, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1950, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(end__su_t1965, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1965, 0.68).
narrative_ontology:measurement(end__su_t1980, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(end__su_t1995, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1995, 0.72).
narrative_ontology:measurement(end__su_t2010, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(end__su_t2025, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
