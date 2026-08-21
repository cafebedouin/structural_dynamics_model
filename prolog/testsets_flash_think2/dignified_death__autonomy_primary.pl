% ============================================================================
% CONSTRAINT STORY: dignified_death__autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__autonomy_primary, []).

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
 *   constraint_id: dignified_death__autonomy_primary
 *   human_readable: Right to Dignified Death (Autonomy Primary Reading)
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'autonomy primary' reading of the
 *   'dignified_death' kernel. It asserts that dignity fundamentally resides
 *   in an individual's self-determination, granting a suffering individual
 *   final authority over the timing and method of their death. The constraint
 *   is framed as a 'tangled_rope' because while it aims to coordinate
 *   individual will with medical practice, it is entangled with significant
 *   legal prohibitions and medical gatekeeping that extract a cost from those
 *   seeking to exercise this autonomy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__autonomy_primary, 0.55).
domain_priors:suppression_score(dignified_death__autonomy_primary, 0.75).
domain_priors:theater_ratio(dignified_death__autonomy_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, extractiveness, 0.55).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Right to Dignified Death (Autonomy Primary Reading)").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, '32510282-a761-48bc-a603-3cc316176f35').
narrative_ontology:cs_kernel_codification('32510282-a761-48bc-a603-3cc316176f35', formalized).
narrative_ontology:cs_authority_grounding('32510282-a761-48bc-a603-3cc316176f35', lineage).
narrative_ontology:cs_interpretation_layer_present('32510282-a761-48bc-a603-3cc316176f35').
narrative_ontology:cs_reading_relation('32510282-a761-48bc-a603-3cc316176f35', dignified_death__sanctity_primary, forecloses).
narrative_ontology:cs_reading_relation('32510282-a761-48bc-a603-3cc316176f35', dignified_death__relational_autonomy, coexists_with).
narrative_ontology:cs_axiom('32510282-a761-48bc-a603-3cc316176f35', foundational, individual_self_ownership).
narrative_ontology:cs_axiom_status(individual_self_ownership, holdable).
narrative_ontology:cs_axiom_grounding('32510282-a761-48bc-a603-3cc316176f35', individual_self_ownership, deontological).
narrative_ontology:cs_axiom('32510282-a761-48bc-a603-3cc316176f35', secondary, right_to_avoid_unbearable_suffering).
narrative_ontology:cs_axiom_status(right_to_avoid_unbearable_suffering, holdable).
narrative_ontology:cs_axiom_grounding('32510282-a761-48bc-a603-3cc316176f35', right_to_avoid_unbearable_suffering, empirically_contingent).
narrative_ontology:cs_reference_frame('32510282-a761-48bc-a603-3cc316176f35', enlightenment_autonomy_ideal).
narrative_ontology:cs_drift_state('32510282-a761-48bc-a603-3cc316176f35', contemporary_medical_legal_context, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('32510282-a761-48bc-a603-3cc316176f35', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, suffering_individual).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, pro_autonomy_clinicians).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, suffering_individual).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, pro_autonomy_clinicians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, sanctity_of_life_advocates).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, palliative_care_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks to exercise final authority over the timing and method of their death to avoid prolonged, unbearable suffering. Benefits when this autonomy is respected; pays the cost of prolonged suffering when denied. Their identity is locked by their medical condition and desire for self-determination.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, suffering_individual, beneficiary,
    powerless, immediate, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, suffering_individual, payer).

% Advocates for and seeks to facilitate patient self-determination in end-of-life decisions. Benefits when able to provide care aligned with patient will; pays the cost of legal and ethical conflict when state prohibitions or institutional policies prevent them from doing so.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, pro_autonomy_clinicians, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, pro_autonomy_clinicians, beneficiary).

% Enforces legal prohibitions against assisted dying, often citing public safety, protection of vulnerable populations, or moral objections. Bears the societal cost of prolonged care for individuals who wish to end their lives but are legally prevented.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, state_legal_system, agenda_setter,
    institutional, generational, constrained, national).

% Opposes assisted dying on moral, religious, or philosophical grounds, emphasizing the intrinsic value of life. Benefits when legal prohibitions are maintained and their ethical framework is upheld.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, sanctity_of_life_advocates, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, sanctity_of_life_advocates, beneficiary).

% Critiques the pure individual autonomy model, emphasizing that dignity and decision-making capacity are shaped by social relationships and context. Advocates for shared decision-making and robust procedural safeguards, rather than absolute individual authority.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, relational_autonomy_advocates, observer,
    moderate, generational, analytical, national).

% Provides comfort, pain management, and holistic support to individuals nearing the end of life. Benefits when their services are sought and valued, but their mandate does not typically include facilitating hastened death, which can put them in conflict with patient autonomy requests.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, palliative_care_providers, beneficiary,
    moderate, biographical, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__autonomy_primary, suffering_individual).
narrative_ontology:fixing_cost_class(dignified_death__autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a framework where an individual's self-determination regarding end-of-life decisions is respected and facilitated within a medical and legal context, ensuring that their will is paramount in their final moments.
% TRANSFER_FUNCTION: Transfers ultimate decision-making authority over the timing and method of death from medical and state institutions to the suffering individual. When this autonomy is denied, it transfers the burden of prolonged suffering from the individual to the state/society, which must provide care against the individual's will.
% ABSENT_VOICES: Individuals who are suffering but lack the capacity to express their will clearly, or those who are denied access to the means of dignified death due to systemic barriers (e.g., poverty, lack of access to medical professionals willing to assist).
% DISAPPEARANCE_RATIONALE: If the principle of individual autonomy as the primary authority over death vanished overnight, the legal and medical frameworks around end-of-life care would fundamentally shift. This would likely lead to more paternalistic or religiously-driven prohibitions, denying individuals control over their final moments and potentially prolonging suffering against their expressed will. The entire bioethical landscape would reorganize.
% FOUNDING_PROBLEM: Individuals facing unbearable suffering at the end of life were denied agency over their own death, leading to prolonged suffering, loss of dignity, and a sense of powerlessness in their final moments.
% FOUNDING_PROBLEM_CORROBORATION: Patient advocacy groups, pro-autonomy medical associations, and legal scholars consistently attest to the ongoing problem of individuals being denied the right to choose a dignified death. They frequently cite cases of prolonged suffering against a patient's expressed will, supported by legislative-hearing testimony and independent ethical analyses from outside the benefiting parties.
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dignified_death__autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__autonomy_primary, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate-high, reflecting the significant cost borne by individuals denied their desired end-of-life choices due to legal and institutional barriers. Suppression (0.75) is high, driven by active legal enforcement of prohibitions and the medical system's gatekeeping role, which limits access to means of hastened death. Theater ratio (0.15) is low, as the debate is genuine and deeply moral, not performative. Accessibility collapse (0.6) is moderate, as alternatives like palliative care exist, but the specific desired alternative (hastened death by choice) is often legally collapsed. Resistance (0.7) is high, reflecting ongoing advocacy and legal challenges for the right to choose.
 *
 * PERSPECTIVAL GAP:
 *   The 'suffering_individual' and 'pro_autonomy_clinicians' experience this constraint as a struggle against an extractive and suppressive system that denies fundamental rights. Conversely, 'state_legal_system' and 'sanctity_of_life_advocates' perceive the constraint as a necessary safeguard, protecting vulnerable lives and upholding moral order. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'suffering_individual' is both a beneficiary (when their autonomy is respected) and a victim (when denied, bearing the cost of prolonged suffering). 'Pro_autonomy_clinicians' are beneficiaries when they can act in accordance with patient will, but victims when constrained by law. 'State_legal_system' and 'sanctity_of_life_advocates' are beneficiaries of the current prohibitive framework, as it aligns with their institutional or moral mandates. The 'identity_locked' exit option for the suffering individual reflects their profound entanglement with their condition and their desire for self-determination.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_capacity_ambiguity,
    'To what extent can an individual experiencing severe suffering and medical dependence truly exercise ''autonomous'' choice regarding their death, free from internal or external coercion?',
    'Development of robust, standardized psychological and medical assessment protocols for decision-making capacity in end-of-life contexts, coupled with longitudinal studies of individuals'' stated preferences over time.',
    'If capacity is frequently compromised, the ''autonomy_primary'' reading''s justification weakens, potentially shifting the classification towards a ''relational_autonomy'' or even ''sanctity_primary'' framework that prioritizes protection over absolute self-determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_capacity_ambiguity, empirical, 'Ambiguity regarding the true capacity for autonomous choice in severe suffering.').

omega_variable(
    medical_gatekeeping_necessity,
    'Is medical gatekeeping (e.g., physician involvement, eligibility criteria) a necessary safeguard to prevent abuse and ensure proper assessment, or does it function as an extractive barrier to legitimate autonomous choice?',
    'Comparative analysis of jurisdictions with varying levels of medical gatekeeping for assisted dying, assessing rates of abuse, patient satisfaction, and outcomes. Ethical review of the ''cost'' of safeguards versus the ''cost'' of denial.',
    'If gatekeeping is found to be primarily an extractive barrier, the constraint''s suppression and extractiveness metrics would be re-evaluated upwards, reinforcing its ''snare'' or ''tangled_rope'' characteristics. If found necessary, it supports the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_gatekeeping_necessity, conceptual, 'Whether medical gatekeeping is a safeguard or an extractive barrier.').

omega_variable(
    kernel_reading_distinction,
    'This constraint is one reading of the ''dignified_death'' kernel, emphasizing individual self-determination. What would be the structural changes if a sibling reading (e.g., ''sanctity_primary'' or ''relational_autonomy'') were adopted as the dominant framework?',
    'Analysis of legal and policy changes in jurisdictions that have shifted their dominant ethical framework for end-of-life care, or counterfactual modeling of such shifts.',
    'Adoption of ''sanctity_primary'' would drastically increase suppression and extractiveness for individuals seeking hastened death. Adoption of ''relational_autonomy'' would shift the locus of decision-making, potentially reducing individual autonomy but increasing procedural safeguards and shared responsibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Impact of alternative kernel readings on constraint structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t1970, dignified_death__autonomy_primary, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(dign_tr_t1980, dignified_death__autonomy_primary, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(dign_tr_t1990, dignified_death__autonomy_primary, theater_ratio, 1990, 0.13).
narrative_ontology:measurement(dign_tr_t2000, dignified_death__autonomy_primary, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(dign_tr_t2010, dignified_death__autonomy_primary, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(dign_tr_t2025, dignified_death__autonomy_primary, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(dign_be_t1970, dignified_death__autonomy_primary, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(dign_be_t1980, dignified_death__autonomy_primary, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(dign_be_t1990, dignified_death__autonomy_primary, base_extractiveness, 1990, 0.51).
narrative_ontology:measurement(dign_be_t2000, dignified_death__autonomy_primary, base_extractiveness, 2000, 0.53).
narrative_ontology:measurement(dign_be_t2010, dignified_death__autonomy_primary, base_extractiveness, 2010, 0.54).
narrative_ontology:measurement(dign_be_t2025, dignified_death__autonomy_primary, base_extractiveness, 2025, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t1970, dignified_death__autonomy_primary, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(dign_su_t1980, dignified_death__autonomy_primary, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement(dign_su_t1990, dignified_death__autonomy_primary, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(dign_su_t2000, dignified_death__autonomy_primary, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(dign_su_t2010, dignified_death__autonomy_primary, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(dign_su_t2025, dignified_death__autonomy_primary, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__autonomy_primary, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
