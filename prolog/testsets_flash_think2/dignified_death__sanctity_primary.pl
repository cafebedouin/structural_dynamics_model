% ============================================================================
% CONSTRAINT STORY: dignified_death__sanctity_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__sanctity_primary, []).

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
 *   constraint_id: dignified_death__sanctity_primary
 *   human_readable: Dignity as Sanctity of Life (Primary Reading)
 *   domain: Bioethics/Medical Law/Political Philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'sanctity of life' reading of dignity,
 *   where life's intrinsic value is paramount, and intentional termination is
 *   morally prohibited regardless of consent. It is one reading of the
 *   broader 'dignified_death' kernel. The constraint is claimed as a Snare
 *   because, while framed as protection, its active enforcement against
 *   patient autonomy leads to coercive prolongation of suffering for
 *   identifiable victims. The metrics reflect high extraction and
 *   suppression, with low theater, indicating direct and effective
 *   enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__sanctity_primary, 0.6).
domain_priors:suppression_score(dignified_death__sanctity_primary, 0.75).
domain_priors:theater_ratio(dignified_death__sanctity_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, extractiveness, 0.6).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Dignity as Sanctity of Life (Primary Reading)").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "Bioethics/Medical Law/Political Philosophy").

domain_priors:requires_active_enforcement(dignified_death__sanctity_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, '38209567-ca66-4d15-8707-a6ca12427e4e').
narrative_ontology:cs_kernel_codification('38209567-ca66-4d15-8707-a6ca12427e4e', formalized).
narrative_ontology:cs_authority_grounding('38209567-ca66-4d15-8707-a6ca12427e4e', lineage).
narrative_ontology:cs_interpretation_layer_present('38209567-ca66-4d15-8707-a6ca12427e4e').
narrative_ontology:cs_reading_relation('38209567-ca66-4d15-8707-a6ca12427e4e', dignified_death__autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('38209567-ca66-4d15-8707-a6ca12427e4e', dignified_death__relational_autonomy, forecloses).
narrative_ontology:cs_axiom('38209567-ca66-4d15-8707-a6ca12427e4e', foundational, life_has_intrinsic_value).
narrative_ontology:cs_axiom_status(life_has_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('38209567-ca66-4d15-8707-a6ca12427e4e', life_has_intrinsic_value, deontological).
narrative_ontology:cs_axiom('38209567-ca66-4d15-8707-a6ca12427e4e', foundational, intentional_killing_is_wrong).
narrative_ontology:cs_axiom_status(intentional_killing_is_wrong, holdable).
narrative_ontology:cs_axiom_grounding('38209567-ca66-4d15-8707-a6ca12427e4e', intentional_killing_is_wrong, deontological).
narrative_ontology:cs_reference_frame('38209567-ca66-4d15-8707-a6ca12427e4e', intrinsic_value_of_life).
narrative_ontology:cs_drift_state('38209567-ca66-4d15-8707-a6ca12427e4e', contemporary_patient_autonomy_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('38209567-ca66-4d15-8707-a6ca12427e4e', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, moral_order_advocates).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, religious_institutions).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, vulnerable_patients).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, suffering_individuals).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, medical_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and defend the intrinsic value of life, advocating for legal and ethical frameworks that prohibit intentional life-termination regardless of consent. They see themselves as protecting a fundamental moral truth and vulnerable populations.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, moral_order_advocates, agenda_setter,
    institutional, generational, analytical, global).

% Benefit from the constraint's alignment with their theological doctrines regarding the sanctity of life. They provide moral authority and social support for the constraint, reinforcing its persistence through their communities and advocacy.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, religious_institutions, beneficiary,
    institutional, civilizational, analytical, global).

% Are theoretically protected by the constraint from coercion, but in practice may find their autonomy overridden, leading to prolonged suffering against their wishes. Their options for ending suffering are limited by the constraint's prohibitions.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, vulnerable_patients, payer,
    powerless, immediate, trapped, local).

% Experience the constraint as a denial of their right to self-determination in the face of unbearable suffering. They are forced to endure conditions they deem undignified, with no legal or medical recourse for an intentional, peaceful end to life.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, suffering_individuals, payer,
    powerless, immediate, trapped, local).

% Are legally and ethically bound by the constraint, preventing them from assisting patients in intentional life-termination even when faced with profound suffering and explicit patient requests. This creates moral distress and limits their ability to provide what they might consider compassionate care.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, medical_professionals, payer,
    moderate, biographical, constrained, national).

% Represent individuals who prioritize self-determination and the right to choose the timing and manner of their death. Their arguments for patient autonomy are structurally excluded by this reading's foundational premise of intrinsic life value, forcing them to operate outside or in opposition to the dominant framework.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, autonomy_advocates, excluded,
    organized, biographical, mobile, global).

% Analyze the legal and ethical implications of the sanctity of life principle, its historical application, and its tension with evolving concepts of patient autonomy. They document the constraint's effects but do not directly enforce or suffer from it.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, legal_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal moral boundary against intentional life-termination, aiming to protect all human life as intrinsically valuable and prevent its devaluation or instrumentalization.
% TRANSFER_FUNCTION: Transfers ultimate decision-making authority over end-of-life choices from individuals to a transcendent moral framework, enforced by legal and social structures, thereby prolonging lives that individuals might wish to end.
% ABSENT_VOICES: Individuals advocating for self-determination in end-of-life decisions, and those who believe quality of life can outweigh mere biological existence, are structurally excluded from the framework's core tenets. Their perspectives are dismissed as undermining the sanctity of life.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, legal and medical systems would rapidly re-evaluate end-of-life care, likely leading to widespread legalization of assisted dying and a significant shift in societal norms around death and dying, prioritizing individual choice and relief of suffering.
% FOUNDING_PROBLEM: Preventing the devaluation of human life and protecting vulnerable individuals from coercion or societal pressure to end their lives, particularly in contexts of historical abuses like eugenics.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (e.g., moral order advocates, religious institutions) cite historical abuses (eugenics, forced euthanasia) as ongoing justification for the constraint. Opponents (e.g., patient rights groups, some bioethicists) argue that modern safeguards make these concerns less relevant, and that the constraint now primarily functions to prolong suffering against individual will; legislative-hearing testimony and independent ethical analyses from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dignified_death__sanctity_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__sanctity_primary, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__sanctity_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__sanctity_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__sanctity_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the constraint imposes a significant cost on suffering individuals by denying them agency over their death, forcing them to endure unwanted prolongation of life. Suppression is also high, as legal and medical systems actively enforce the prohibition, limiting exit options for those seeking to end their suffering. Theater ratio is low because the enforcement is direct and functional, not merely performative. Resistance is moderate-high due to ongoing advocacy for patient autonomy and the legalization of assisted dying.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of moral order advocates, this constraint is a necessary protection, a 'Rope' or even a 'Mountain' reflecting natural law. From the perspective of suffering individuals, it is a 'Snare' that traps them in unwanted existence. The engine's classification will highlight this divergence by computing a Snare from the victim's seat, despite the claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Moral order advocates and religious institutions are beneficiaries, as the constraint aligns with and reinforces their core tenets, granting them moral authority. Vulnerable patients and suffering individuals are clear victims, bearing the direct cost of denied autonomy and prolonged suffering. Medical professionals are also payers, experiencing moral distress and legal constraints on their practice. Autonomy advocates are excluded, their arguments systematically marginalized by the constraint's framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''dignified_death'' kernel, or merely a variant of a broader ''pro-life'' stance?',
    'Analysis of philosophical arguments and legal precedents: if the core premise (intrinsic value of life) is consistently applied to end-of-life decisions in a way that distinguishes it from other pro-life arguments (e.g., abortion), it is a distinct reading.',
    'If not distinct, it might be subsumed under a broader ''pro_life_doctrine'' constraint, affecting its network relationships and classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint as a specific reading of the ''dignified_death'' kernel.').

omega_variable(
    autonomy_foreclosure_justification,
    'Does the ''sanctity_primary'' reading genuinely foreclose the ''autonomy_primary'' reading, or do they merely coexist as competing ethical frameworks?',
    'Examination of the logical structure of arguments: if the intrinsic value of life is asserted as an absolute, non-negotiable principle that overrides individual consent, then it logically forecloses a framework where individual consent is the final authority.',
    'If they merely coexist, the ''forecloses'' relation would shift to ''coexists_with'', altering the commitment system''s internal consistency assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_foreclosure_justification, conceptual, 'Assesses the logical incompatibility between sanctity-of-life and autonomy-based dignity readings.').

omega_variable(
    vulnerable_protection_vs_coercion,
    'To what extent does the constraint genuinely protect vulnerable populations from coercion, versus coercing suffering individuals into unwanted prolongation of life?',
    'Empirical studies on end-of-life decision-making outcomes in jurisdictions with and without this constraint, focusing on patient reported experiences and incidence of unwanted medical interventions.',
    'If the coercive aspect outweighs the protective, the ''snare'' classification is strongly reinforced, and the justification for the constraint''s existence is undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_protection_vs_coercion, empirical, 'Distinguishes protective function from coercive outcome for vulnerable populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t1980, dignified_death__sanctity_primary, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(dign_tr_t1990, dignified_death__sanctity_primary, theater_ratio, 1990, 0.14).
narrative_ontology:measurement(dign_tr_t2000, dignified_death__sanctity_primary, theater_ratio, 2000, 0.13).
narrative_ontology:measurement(dign_tr_t2010, dignified_death__sanctity_primary, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(dign_tr_t2020, dignified_death__sanctity_primary, theater_ratio, 2020, 0.11).

% Extraction over time
narrative_ontology:measurement(dign_be_t1980, dignified_death__sanctity_primary, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(dign_be_t1990, dignified_death__sanctity_primary, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(dign_be_t2000, dignified_death__sanctity_primary, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(dign_be_t2010, dignified_death__sanctity_primary, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(dign_be_t2020, dignified_death__sanctity_primary, base_extractiveness, 2020, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t1980, dignified_death__sanctity_primary, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(dign_su_t1990, dignified_death__sanctity_primary, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(dign_su_t2000, dignified_death__sanctity_primary, suppression_requirement, 2000, 0.73).
narrative_ontology:measurement(dign_su_t2010, dignified_death__sanctity_primary, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(dign_su_t2020, dignified_death__sanctity_primary, suppression_requirement, 2020, 0.77).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
