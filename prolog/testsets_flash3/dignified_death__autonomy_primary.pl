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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Dignified Death: Autonomy Primary Reading
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story represents the 'autonomy primary' reading of
 *   dignified death, where an individual's self-determination is paramount in
 *   end-of-life decisions. It asserts that a suffering individual has the
 *   final authority over the timing and method of their death. The constraint
 *   itself is the denial of this autonomy by external forces (state, medical
 *   institutions). The metrics reflect the ongoing struggle to establish this
 *   autonomy against legal and institutional prohibitions, which are seen as
 *   extractive and suppressive. The claimed type is 'tangled_rope' because
 *   the underlying norm of individual autonomy is entangled with complex
 *   medical gatekeeping and eligibility criteria, creating a structure that
 *   both coordinates (end-of-life care) and extracts (prolonged suffering).
 *
 * KEY AGENTS:
 *   - autonomous_suffering_individual: Primary beneficiary (powerless/trapped) — seeks to exercise autonomy
 *   - suffering_individual_denied_exit: Primary victim (powerless/trapped) — bears prolonged suffering
 *   - medical_professionals_constrained_by_prohibition: Payer (organized/constrained) — moral distress from legal limits
 *   - state_legal_system: Agenda-setter (institutional/constrained) — enforces prohibitions
 *   - advocacy_groups_for_autonomy: Beneficiary (organized/mobile) — lobbies for change
 *   - religious_institutions_and_pro_life_groups: Excluded (organized/identity_locked) — oppose autonomy-primary view
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__autonomy_primary, 0.55).
domain_priors:suppression_score(dignified_death__autonomy_primary, 0.7).
domain_priors:theater_ratio(dignified_death__autonomy_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, extractiveness, 0.55).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Dignified Death: Autonomy Primary Reading").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, '89f88038-d784-402b-8e50-dde5fc33bda4').
narrative_ontology:cs_kernel_codification('89f88038-d784-402b-8e50-dde5fc33bda4', distributed).
narrative_ontology:cs_authority_grounding('89f88038-d784-402b-8e50-dde5fc33bda4', distributed).
narrative_ontology:cs_reading_relation('89f88038-d784-402b-8e50-dde5fc33bda4', dignified_death__sanctity_primary, forecloses).
narrative_ontology:cs_reading_relation('89f88038-d784-402b-8e50-dde5fc33bda4', dignified_death__relational_autonomy, coexists_with).
narrative_ontology:cs_axiom('89f88038-d784-402b-8e50-dde5fc33bda4', foundational, individual_self_determination_is_paramount).
narrative_ontology:cs_axiom_status(individual_self_determination_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('89f88038-d784-402b-8e50-dde5fc33bda4', individual_self_determination_is_paramount, deontological).
narrative_ontology:cs_axiom('89f88038-d784-402b-8e50-dde5fc33bda4', secondary, suffering_beyond_relief_justifies_exit).
narrative_ontology:cs_axiom_status(suffering_beyond_relief_justifies_exit, holdable).
narrative_ontology:cs_axiom_grounding('89f88038-d784-402b-8e50-dde5fc33bda4', suffering_beyond_relief_justifies_exit, empirically_contingent).
narrative_ontology:cs_reference_frame('89f88038-d784-402b-8e50-dde5fc33bda4', enlightenment_individual_rights).
narrative_ontology:cs_drift_state('89f88038-d784-402b-8e50-dde5fc33bda4', contemporary_bioethics_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('89f88038-d784-402b-8e50-dde5fc33bda4', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, autonomous_suffering_individual).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, suffering_individual_denied_exit).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, medical_professionals_constrained_by_prohibition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, advocacy_groups_for_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks to exercise final authority over their own death to avoid prolonged suffering, aligning with their conception of dignity. Currently trapped by legal prohibitions and medical gatekeeping.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, autonomous_suffering_individual, beneficiary,
    powerless, immediate, trapped, local).

% Bears the cost of prolonged suffering against their will due to legal and institutional barriers preventing self-determined death. Their dignity is violated by the denial of autonomy.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, suffering_individual_denied_exit, payer,
    powerless, immediate, trapped, local).

% Bound by legal and ethical frameworks that prohibit assisting in self-determined death, even when a patient's suffering is intractable and their autonomy is clear. They experience moral distress and professional constraint.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, medical_professionals_constrained_by_prohibition, payer,
    organized, biographical, constrained, national).

% Enforces prohibitions against assisted dying, often citing public safety, sanctity of life, or protection of vulnerable populations. It sets the legal framework that constrains individual autonomy in end-of-life decisions.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, state_legal_system, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the recognition and expansion of individual self-determination in end-of-life care, as it aligns with their core mission. They actively lobby for legislative change and support legal challenges.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, advocacy_groups_for_autonomy, beneficiary,
    organized, generational, mobile, national).

% Strongly oppose self-determined death based on sanctity of life principles. From the autonomy-primary reading, their views are seen as external impositions that deny individual rights, and they are excluded from the decision-making process of the suffering individual.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, religious_institutions_and_pro_life_groups, excluded,
    organized, civilizational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the individual's right to self-determination with the medical system's capacity to provide compassionate end-of-life care, ensuring that a suffering individual's final wishes regarding the timing and method of death are respected.
% TRANSFER_FUNCTION: Transfers ultimate decision-making authority over one's own death from external legal/medical/moral authorities to the suffering individual, thereby transferring the 'cost' of prolonged suffering (and the 'benefit' of a dignified exit) to the individual's control.
% ABSENT_VOICES: Religious institutions and pro-life groups, who would argue for the intrinsic value of life regardless of suffering or consent, are structurally excluded from the individual's self-determination process in this reading. Their arguments are seen as undermining the very concept of autonomy at the end of life.
% DISAPPEARANCE_RATIONALE: If the constraint (denial of self-determination) vanished, the legal and medical frameworks would rapidly reorganize to accommodate individual choice in end-of-life decisions. New protocols for assisted dying would emerge, and the power dynamic between patients and institutions would fundamentally shift.
% FOUNDING_PROBLEM: The historical problem of individuals suffering prolonged, intractable pain or indignity at the end of life, with no legal or medical recourse to hasten their death in accordance with their wishes.
% FOUNDING_PROBLEM_CORROBORATION: Patient advocacy groups, bioethicists, and a growing segment of the medical community attest that the problem of unalleviated suffering and denial of autonomy at life's end remains a live and pressing issue, supported by patient testimonials and surveys.
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.45 at end) reflects the cost of denied autonomy and prolonged suffering. Suppression (0.6 at end) is high due to legal prohibitions and institutional resistance to assisted dying. Theater ratio (0.3 at end) indicates that while some medical protocols genuinely aim to alleviate suffering, a portion of the 'care' is performative maintenance of a system that denies self-determination. The trend shows decreasing extractiveness and suppression, and increasing theater, reflecting gradual legal changes in some jurisdictions and growing public debate.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the suffering individual, the constraint is a snare, trapping them in unwanted suffering. From the state's perspective, it's a rope, coordinating public safety and moral order. Medical professionals experience it as a tangled rope, balancing patient autonomy with legal and ethical duties. The engine's per-seat classification will capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The autonomous_suffering_individual is the direct beneficiary of this reading's assertion, but also a target of the existing constraint (denial of autonomy). The suffering_individual_denied_exit is a clear victim. Medical professionals are payers, bearing the moral and professional costs of the prohibition. The state legal system is the agenda-setter, enforcing the constraint. Advocacy groups for autonomy are beneficiaries, as their mission aligns with the constraint's resolution. Religious institutions are excluded, as their views are seen as external to the individual's self-determination.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a tangled_rope prevents mislabeling the constraint as pure extraction (snare) by acknowledging the genuine coordination function of end-of-life care, while simultaneously highlighting the asymmetric extraction of autonomy and prolonged suffering. It also avoids mislabeling it as a pure rope by recognizing the active enforcement and suppression required to maintain the current prohibitions. The 'contested' status of the founding problem further supports the tangled_rope classification, indicating an ongoing struggle over the constraint's true function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_autonomy,
    'What are the legitimate boundaries of individual self-determination in end-of-life decisions, particularly concerning mental capacity, coercion, and vulnerability?',
    'Development of robust legal and medical safeguards, clear criteria for assessing capacity, and empirical studies on the prevalence of coercion in end-of-life requests.',
    'If boundaries are narrowly defined, the effective scope of autonomy is reduced, increasing extractiveness for those deemed ineligible. If broadly defined, extractiveness decreases. This would shift the balance between individual rights and societal protection, potentially altering the constraint''s classification towards a more or less extractive form.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_autonomy, conceptual, 'Ambiguity regarding the precise scope and limits of individual autonomy in end-of-life decisions.').

omega_variable(
    medical_professional_role,
    'To what extent should medical professionals be obligated to participate in or facilitate self-determined death, given their professional ethics and personal beliefs?',
    'Development of clear conscience clauses, referral systems, and professional guidelines that balance patient autonomy with clinician integrity, potentially through legislative action or professional body consensus.',
    'If participation is largely optional, access to self-determined death may remain constrained, increasing extractiveness for individuals. If participation is a professional obligation (with safeguards), extractiveness decreases. This impacts the ''payer'' role of medical professionals and the overall accessibility of the constraint''s resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_professional_role, preference, 'Uncertainty regarding the role and obligations of medical professionals in facilitating self-determined death.').

omega_variable(
    natural_vs_constructed_prohibition,
    'Is the prohibition against self-determined death a ''natural law'' reflecting intrinsic moral order, or a ''constructed constraint'' reflecting societal values and institutional power?',
    'Philosophical and theological debate, legal precedent, and cross-cultural comparison of end-of-life practices. The presence of identifiable beneficiaries (e.g., institutions that benefit from maintaining the status quo) would strongly suggest a constructed constraint.',
    'If viewed as natural law, the constraint''s extractiveness might be re-interpreted as a necessary cost of moral order. If constructed, its extractiveness is more clearly seen as a product of power dynamics, strengthening the case for reform and reclassification towards a snare or tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_prohibition, conceptual, 'Ambiguity over whether the prohibition on self-determined death is a natural moral law or a human-made construct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t1970, dignified_death__autonomy_primary, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(dign_tr_t1985, dignified_death__autonomy_primary, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(dign_tr_t2000, dignified_death__autonomy_primary, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(dign_tr_t2010, dignified_death__autonomy_primary, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(dign_tr_t2024, dignified_death__autonomy_primary, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(dign_be_t1970, dignified_death__autonomy_primary, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(dign_be_t1985, dignified_death__autonomy_primary, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(dign_be_t2000, dignified_death__autonomy_primary, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(dign_be_t2010, dignified_death__autonomy_primary, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(dign_be_t2024, dignified_death__autonomy_primary, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t1970, dignified_death__autonomy_primary, suppression_requirement, 1970, 0.8).
narrative_ontology:measurement(dign_su_t1985, dignified_death__autonomy_primary, suppression_requirement, 1985, 0.75).
narrative_ontology:measurement(dign_su_t2000, dignified_death__autonomy_primary, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(dign_su_t2010, dignified_death__autonomy_primary, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(dign_su_t2024, dignified_death__autonomy_primary, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__autonomy_primary, attachment_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
