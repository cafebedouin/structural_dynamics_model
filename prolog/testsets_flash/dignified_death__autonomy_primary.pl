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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dignified_death__autonomy_primary
 *   human_readable: Dignified Death: Autonomy Primary Reading
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'autonomy primary' reading of dignified
 *   death, asserting that an individual's self-determination, especially in
 *   the face of suffering, grants them final authority over the timing and
 *   method of their death. It is a contested principle within bioethics and
 *   medical law, often clashing with other values like the sanctity of life
 *   or the relational aspects of autonomy. The constraint operates as a
 *   tangled rope because while it aims to coordinate the individual's will
 *   with medical practice, it often involves significant gatekeeping and
 *   legal restrictions that can become extractive for those seeking to
 *   exercise this autonomy.
 *
 * KEY AGENTS:
 *   - autonomous_suffering_individual: Primary beneficiary (powerful/constrained) — seeks to exercise final authority
 *   - suffering_individual_denied_exit: Primary victim (powerless/trapped) — bears prolonged suffering against will
 *   - medical_professionals_constrained_by_law: Payer (institutional/constrained) — navigate legal and ethical boundaries, may be compelled to prolong life or deny requests
 *   - state_legislatures_and_courts: Agenda setter (institutional/arbitrage) — define legal boundaries, eligibility criteria, and safeguards for assisted dying
 *   - advocacy_groups_for_autonomy: Beneficiary (organized/mobile) — promote and defend the right to self-determination in end-of-life decisions
 *   - advocacy_groups_for_sanctity_of_life: Excluded (organized/mobile) — oppose the principle of self-determination in end-of-life decisions, advocating for life preservation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__autonomy_primary, 0.55).
domain_priors:suppression_score(dignified_death__autonomy_primary, 0.7).
domain_priors:theater_ratio(dignified_death__autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, extractiveness, 0.55).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Dignified Death: Autonomy Primary Reading").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, 'fd560053-04c3-49fb-97a0-db533bef201d').
narrative_ontology:cs_kernel_codification('fd560053-04c3-49fb-97a0-db533bef201d', formalized).
narrative_ontology:cs_authority_grounding('fd560053-04c3-49fb-97a0-db533bef201d', lineage).
narrative_ontology:cs_interpretation_layer_present('fd560053-04c3-49fb-97a0-db533bef201d').
narrative_ontology:cs_reading_relation('fd560053-04c3-49fb-97a0-db533bef201d', dignified_death__sanctity_primary, forecloses).
narrative_ontology:cs_reading_relation('fd560053-04c3-49fb-97a0-db533bef201d', dignified_death__relational_autonomy, coexists_with).
narrative_ontology:cs_axiom('fd560053-04c3-49fb-97a0-db533bef201d', foundational, individual_self_determination_is_paramount).
narrative_ontology:cs_axiom_status(individual_self_determination_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('fd560053-04c3-49fb-97a0-db533bef201d', individual_self_determination_is_paramount, deontological).
narrative_ontology:cs_axiom('fd560053-04c3-49fb-97a0-db533bef201d', secondary, unbearable_suffering_justifies_exit).
narrative_ontology:cs_axiom_status(unbearable_suffering_justifies_exit, holdable).
narrative_ontology:cs_axiom_grounding('fd560053-04c3-49fb-97a0-db533bef201d', unbearable_suffering_justifies_exit, empirically_contingent).
narrative_ontology:cs_reference_frame('fd560053-04c3-49fb-97a0-db533bef201d', enlightenment_individual_rights).
narrative_ontology:cs_drift_state('fd560053-04c3-49fb-97a0-db533bef201d', contemporary_bioethics_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('fd560053-04c3-49fb-97a0-db533bef201d', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, autonomous_suffering_individual).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, suffering_individual_denied_exit).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, medical_professionals_constrained_by_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, advocacy_groups_for_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A person experiencing unbearable suffering who wishes to exercise their right to self-determination regarding the timing and method of their death. They are the primary subject of this constraint's intended benefit, but their ability to realize it is often constrained by external factors.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, autonomous_suffering_individual, beneficiary,
    powerful, immediate, constrained, local).

% A person experiencing unbearable suffering whose request for assisted dying is denied due to legal restrictions, medical gatekeeping, or lack of access. They are forced to prolong their suffering against their will, bearing the full cost of the constraint's limitations.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, suffering_individual_denied_exit, payer,
    powerless, immediate, trapped, local).

% Physicians, nurses, and other healthcare providers who must navigate complex legal frameworks and ethical guidelines regarding end-of-life care. They may be compelled to prolong life against a patient's wishes or deny requests for assisted dying due to legal prohibitions, even if they personally support the patient's autonomy.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, medical_professionals_constrained_by_law, payer,
    institutional, biographical, constrained, national).

% The governmental bodies responsible for enacting and interpreting laws related to end-of-life care, including assisted dying. They define the legal boundaries, eligibility criteria, and safeguards, thereby shaping the extent to which individual autonomy can be exercised.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, state_legislatures_and_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% Organizations and movements that actively campaign for the recognition and expansion of individual rights to self-determination in end-of-life decisions. They benefit from the legal and ethical advancements that align with the autonomy-primary principle.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, advocacy_groups_for_autonomy, beneficiary,
    organized, generational, mobile, global).

% Organizations and movements that oppose assisted dying on the grounds of the intrinsic value of life. They are structurally excluded from the direct implementation of this constraint, as their core principles are often in direct opposition to the autonomy-primary reading.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, advocacy_groups_for_sanctity_of_life, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__autonomy_primary, autonomous_suffering_individual).
narrative_ontology:fixing_cost_class(dignified_death__autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To align medical practice and legal frameworks with the expressed will of a suffering individual regarding their end-of-life decisions, ensuring that their final wishes are respected and facilitated.
% TRANSFER_FUNCTION: Transfers the ultimate decision-making authority over the timing and method of death from medical or state authorities to the suffering individual. When denied, it transfers prolonged suffering and loss of control to the individual.
% ABSENT_VOICES: Advocacy groups for the sanctity of life and some religious institutions are often excluded from the direct legislative and medical implementation of this constraint, as their core tenets fundamentally oppose the principle of self-determination in ending life. They would argue for the preservation of life regardless of suffering or consent.
% DISAPPEARANCE_RATIONALE: If the principle of autonomy as primary in end-of-life decisions vanished, medical practice would revert to a default of life preservation, potentially prolonging suffering. Legal frameworks for assisted dying would dissolve, and individuals would lose a recognized right, leading to a significant reorganization of ethical and legal landscapes around death.
% FOUNDING_PROBLEM: The problem of individuals experiencing unbearable suffering with no legal or medical recourse to end their lives with dignity, leading to prolonged agony and loss of control over their final moments.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live, attested by ongoing patient suffering, legal challenges, and public discourse in jurisdictions where assisted dying is restricted or illegal. Medical ethicists and patient advocacy groups, outside the direct beneficiaries of the current legal frameworks, corroborate the persistence of this problem.
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dignified_death__autonomy_primary, 'none', 1).

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
 *   The extractiveness (0.55) stems from the costs imposed on individuals whose requests for assisted dying are denied or delayed by legal and medical gatekeeping, prolonging suffering against their will. Suppression (0.7) is high due to legal prohibitions, medical ethical codes, and institutional resistance that limit access to assisted dying, effectively suppressing the exercise of self-determination. Theater ratio (0.1) is low, as the mechanisms (laws, medical protocols) are genuinely functional in either enabling or restricting access, not merely performative. The 'tangled_rope' classification reflects the dual nature: a genuine coordination function (aligning medical care with patient wishes) entangled with significant extraction (denial of autonomy, prolonged suffering) and active enforcement (legal penalties for non-compliance).
 *
 * PERSPECTIVAL GAP:
 *   The autonomous suffering individual experiences this as a fundamental right, often denied, leading to high perceived extraction. Medical professionals, while potentially sympathetic, experience it as a constraint on their practice, balancing patient autonomy with legal and ethical obligations. State legislatures and courts, as agenda setters, view it as a complex policy challenge requiring careful balancing of competing values, where their decisions define the boundaries of both coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'autonomous_suffering_individual' is the intended beneficiary, aiming for full control over their end-of-life. However, when denied, they become a victim. 'Suffering_individual_denied_exit' are clear victims, bearing the full cost of prolonged suffering. 'Medical_professionals_constrained_by_law' are payers, as they must navigate complex legal frameworks and ethical dilemmas, sometimes against their personal convictions or patient wishes. 'State_legislatures_and_courts' are agenda setters, defining the scope of this autonomy. 'Advocacy_groups_for_autonomy' are beneficiaries as they champion the principle. 'Advocacy_groups_for_sanctity_of_life' are excluded, as their core tenet is often directly opposed to this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as the underlying 'problem' (suffering and the desire for self-determination in death) remains live. However, the 'tangled_rope' classification prevents mislabeling it as a 'rope' (pure coordination) by highlighting the significant extraction and suppression inherent in its current implementation, particularly for those denied access. It also prevents mislabeling as a 'snare' by acknowledging the genuine coordination function of aligning end-of-life care with patient wishes, even if imperfectly realized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine expression of individual autonomy, or is it a specific reading of the ''dignified_death'' kernel that prioritizes self-determination above other values?',
    'Analysis of legal and ethical frameworks that explicitly balance or subordinate individual autonomy to other principles (e.g., sanctity of life, community welfare, medical ethics).',
    'If it is merely one reading, its classification as a ''tangled_rope'' highlights the contestability of its underlying principles and the potential for alternative, less extractive, or more coordinative framings. If it were a universal principle, it would approach a ''mountain'' for the individual.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''autonomy_primary'' reading of the ''dignified_death'' kernel. Sibling readings (''sanctity_primary'', ''relational_autonomy'') would shift the victim/beneficiary sets and the claimed type.').

omega_variable(
    medical_gatekeeping_necessity,
    'To what extent is medical gatekeeping (e.g., eligibility criteria, psychological evaluation) a necessary coordination function for ensuring ''dignified'' death, versus an extractive mechanism that prolongs suffering or denies self-determination?',
    'Comparative analysis of jurisdictions with varying levels of medical gatekeeping for assisted dying, assessing patient outcomes, perceived dignity, and rates of denied requests.',
    'If gatekeeping is primarily extractive, the constraint''s extractiveness and suppression are higher than currently measured. If it''s a necessary coordination, the ''tangled_rope'' classification is more robust, reflecting genuine coordination costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_gatekeeping_necessity, empirical, 'Ambiguity in the role of medical gatekeeping in assisted dying: coordination vs. extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dign_tr_t5, dignified_death__autonomy_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(dign_tr_t10, dignified_death__autonomy_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(dign_tr_t15, dignified_death__autonomy_primary, theater_ratio, 15, 0.1).
narrative_ontology:measurement(dign_tr_t20, dignified_death__autonomy_primary, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__autonomy_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dign_be_t5, dignified_death__autonomy_primary, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(dign_be_t10, dignified_death__autonomy_primary, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(dign_be_t15, dignified_death__autonomy_primary, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(dign_be_t20, dignified_death__autonomy_primary, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__autonomy_primary, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(dign_su_t5, dignified_death__autonomy_primary, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(dign_su_t10, dignified_death__autonomy_primary, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(dign_su_t15, dignified_death__autonomy_primary, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(dign_su_t20, dignified_death__autonomy_primary, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__autonomy_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(dignified_death__autonomy_primary, 0.08).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, medical_ethics_codes).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, palliative_care_access).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, sanctity_primary).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, relational_autonomy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dignified_death' kernel. Its ε value (0.55) is distinct from the 'sanctity_primary' reading (which would have lower extractiveness for the state, higher for the individual seeking exit) and the 'relational_autonomy' reading (which would distribute extraction differently across the triad).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
