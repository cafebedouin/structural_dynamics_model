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
 *   human_readable: Dignified Death: Autonomy Primary Reading
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'autonomy primary' reading of the
 *   'dignified death' kernel. It posits that dignity in end-of-life decisions
 *   is fundamentally rooted in the individual's self-determination, granting
 *   the suffering individual final authority over the timing and method of
 *   their death. The constraint is framed as a tangled rope because while it
 *   aims to coordinate individual autonomy with medical practice, it is
 *   entangled with significant gatekeeping and eligibility criteria, and its
 *   persistence relies on active enforcement against competing ethical
 *   frameworks and legal prohibitions. The metrics reflect the ongoing
 *   struggle to establish and protect this autonomy against existing
 *   suppressive forces.
 *
 * KEY AGENTS:
 *   - autonomous_suffering_individual: Primary beneficiary (powerless/identity_locked)
 *   - suffering_individual_denied_exit: Primary victim (powerless/trapped)
 *   - medical_professionals_constrained_by_prohibition: Payer (organized/constrained)
 *   - advocacy_groups_for_autonomy: Agenda setter (organized/mobile)
 *   - state_legal_frameworks: Agenda setter (institutional/constrained)
 *   - religious_institutions: Excluded (institutional/identity_locked)
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
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Dignified Death: Autonomy Primary Reading").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, '656ba18b-5278-4ca6-8d58-166eb26b7edb').
narrative_ontology:cs_kernel_codification('656ba18b-5278-4ca6-8d58-166eb26b7edb', formalized).
narrative_ontology:cs_authority_grounding('656ba18b-5278-4ca6-8d58-166eb26b7edb', lineage).
narrative_ontology:cs_interpretation_layer_present('656ba18b-5278-4ca6-8d58-166eb26b7edb').
narrative_ontology:cs_reading_relation('656ba18b-5278-4ca6-8d58-166eb26b7edb', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_reading_relation('656ba18b-5278-4ca6-8d58-166eb26b7edb', dignified_death__relational_autonomy, coexists_with).
narrative_ontology:cs_axiom('656ba18b-5278-4ca6-8d58-166eb26b7edb', foundational, individual_self_determination_is_paramount).
narrative_ontology:cs_axiom_status(individual_self_determination_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('656ba18b-5278-4ca6-8d58-166eb26b7edb', individual_self_determination_is_paramount, deontological).
narrative_ontology:cs_axiom('656ba18b-5278-4ca6-8d58-166eb26b7edb', secondary, relief_of_suffering_is_a_moral_imperative).
narrative_ontology:cs_axiom_status(relief_of_suffering_is_a_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('656ba18b-5278-4ca6-8d58-166eb26b7edb', relief_of_suffering_is_a_moral_imperative, deontological).
narrative_ontology:cs_reference_frame('656ba18b-5278-4ca6-8d58-166eb26b7edb', enlightenment_liberal_autonomy).
narrative_ontology:cs_drift_state('656ba18b-5278-4ca6-8d58-166eb26b7edb', contemporary_bioethics_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('656ba18b-5278-4ca6-8d58-166eb26b7edb', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, autonomous_suffering_individual).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, suffering_individual_denied_exit).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, medical_professionals_constrained_by_prohibition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The individual whose self-determination is paramount, seeking to exercise final authority over their death to avoid prolonged suffering. Their identity is locked into their suffering and desire for control.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, autonomous_suffering_individual, beneficiary,
    powerless, immediate, identity_locked, local).

% An individual whose request for assistance in dying is denied due to legal or medical prohibitions, forcing them to endure suffering against their will. They are trapped by the existing legal framework.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, suffering_individual_denied_exit, payer,
    powerless, immediate, trapped, local).

% Physicians and other healthcare providers who, under current legal frameworks, are prohibited from assisting in a patient's death, even when aligned with the patient's autonomous wishes. They face ethical dilemmas and legal risks.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, medical_professionals_constrained_by_prohibition, payer,
    organized, biographical, constrained, national).

% Organizations and activists who champion the right to self-determination in end-of-life decisions, lobbying for legislative changes and supporting individuals seeking this right. They actively shape the discourse and legal landscape.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, advocacy_groups_for_autonomy, agenda_setter,
    organized, generational, mobile, national).

% The legal and regulatory systems that currently prohibit or severely restrict assisted dying, often citing public safety, sanctity of life, or protection of vulnerable populations. They enforce the existing constraints.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, state_legal_frameworks, agenda_setter,
    institutional, generational, constrained, national).

% Organizations that uphold the sanctity of life and oppose assisted dying on theological grounds. While influential in public discourse, their direct authority is excluded from the legal framework of this specific reading.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, religious_institutions, excluded,
    institutional, civilizational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the individual's right to self-determination with the medical system's capacity to provide compassionate end-of-life care, ensuring that a suffering individual's final wishes are respected and facilitated.
% TRANSFER_FUNCTION: Transfers the ultimate decision-making authority regarding the timing and method of death from external authorities (state, medical, religious) to the suffering individual, and transfers the burden of prolonged suffering from the individual to the collective (if denied).
% ABSENT_VOICES: Those who prioritize the sanctity of life or relational aspects of autonomy are often excluded from the primary legal and medical discourse that centers individual self-determination. They would argue for different ethical frameworks.
% DISAPPEARANCE_RATIONALE: If the principle of individual self-determination in end-of-life decisions vanished, the legal and medical landscape would fundamentally shift. Individuals would lose a crucial right, and medical practice would revert to a more paternalistic model, leading to significant societal reorganization around death and dying.
% FOUNDING_PROBLEM: The problem of individuals suffering prolonged, intractable pain or debilitating conditions without the agency to choose the timing and manner of their death, leading to a loss of dignity and control.
% FOUNDING_PROBLEM_CORROBORATION: Patient advocacy groups, bioethicists, and a growing number of medical professionals attest that this problem remains live, citing cases of individuals enduring unwanted suffering. Public opinion polls in many jurisdictions also corroborate the desire for greater individual control over end-of-life decisions, from outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.55) reflects the significant cost borne by individuals denied their autonomous choice, prolonged suffering, and the ethical burden on medical professionals. Suppression (0.7) is high due to strong legal prohibitions and societal norms that actively prevent or restrict assisted dying. The 'tangled rope' classification acknowledges the genuine coordination function (respecting autonomy) but highlights the asymmetric extraction and active enforcement required to maintain the current, often restrictive, balance. The slight increase in extractiveness and suppression requirement towards the end of the interval reflects renewed legal and ethical challenges in some jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the autonomous suffering individual, the constraint (or its absence) is a matter of fundamental rights and dignity. From the state's perspective, it's a matter of public policy, balancing individual rights with societal protections. Medical professionals navigate the tension between patient advocacy and legal/ethical boundaries. These divergent views lead to different experiences of the constraint's extractiveness and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   The autonomous suffering individual is the direct beneficiary of this reading's principles (d near 0.0), but if denied, becomes a victim (d near 1.0). State legal frameworks, by prohibiting or restricting, act as targets of advocacy but also as enforcers of the status quo, extracting compliance. Medical professionals are caught in the middle, bearing costs of ethical conflict and legal risk (d near 0.7).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'rope' (simple coordination) by highlighting the active enforcement and identifiable victims. It also avoids mislabeling it as a 'snare' by acknowledging the genuine coordination problem of respecting individual autonomy in end-of-life decisions. The 'tangled rope' accurately captures the entanglement of a valid coordination function with asymmetric extraction and the need for active maintenance against resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_vulnerability,
    'How can the principle of individual self-determination be upheld without inadvertently creating pressure on vulnerable individuals to choose death?',
    'Empirical studies on the impact of assisted dying legislation on vulnerable populations, combined with robust procedural safeguards and independent review processes.',
    'If safeguards are insufficient, the constraint''s effective extractiveness on vulnerable populations could be higher than measured, potentially shifting its classification towards a snare for that specific group. If robust, it strengthens the tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_vs_vulnerability, empirical, 'Balancing individual autonomy with protection of vulnerable persons.').

omega_variable(
    medical_professional_conscience,
    'To what extent should medical professionals be compelled to participate in end-of-life decisions that conflict with their moral or religious beliefs?',
    'Legal and ethical frameworks that define the scope of conscientious objection, ensuring patient access to care while respecting professional integrity.',
    'If professionals are compelled without adequate safeguards, their effective extractiveness increases, potentially leading to a ''snare'' for them. If robust conscience protections exist, it maintains the ''tangled rope'' by acknowledging the coordination of diverse values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_professional_conscience, preference, 'Reconciling patient autonomy with medical professional conscience.').

omega_variable(
    kernel_reading_sanctity_primary_delta,
    'What would be the structural changes if the ''sanctity_primary'' reading of the dignified_death kernel were adopted instead of ''autonomy_primary''?',
    'Conceptual analysis of legal and ethical frameworks based on intrinsic value of life, and comparison with existing ''autonomy_primary'' frameworks.',
    'The victim set would shift to those who choose to end their lives, and the beneficiary set would include those whose lives are prolonged. The extractiveness of state prohibitions would be inverted, and the constraint would likely classify as a Mountain or Rope from the perspective of those upholding sanctity of life.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sanctity_primary_delta, conceptual, 'Structural impact of adopting the ''sanctity_primary'' reading.').

omega_variable(
    kernel_reading_relational_autonomy_delta,
    'What would be the structural changes if the ''relational_autonomy'' reading of the dignified_death kernel were adopted instead of ''autonomy_primary''?',
    'Conceptual analysis of legal and ethical frameworks that emphasize shared decision-making and relational context, and comparison with ''autonomy_primary'' frameworks.',
    'The decision-making authority would be distributed, potentially reducing the ''identity_locked'' exit option for the individual but increasing the power of family and clinicians. The extractiveness would be lower for individuals who prefer shared decision-making but higher for those who insist on sole individual authority. The constraint would likely remain a tangled_rope but with different beneficiaries and victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relational_autonomy_delta, conceptual, 'Structural impact of adopting the ''relational_autonomy'' reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(dign_be_t1970, dignified_death__autonomy_primary, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(dign_be_t1985, dignified_death__autonomy_primary, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(dign_be_t2000, dignified_death__autonomy_primary, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(dign_be_t2010, dignified_death__autonomy_primary, base_extractiveness, 2010, 0.53).
narrative_ontology:measurement(dign_be_t2024, dignified_death__autonomy_primary, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t1970, dignified_death__autonomy_primary, suppression_requirement, 1970, 0.8).
narrative_ontology:measurement(dign_su_t1985, dignified_death__autonomy_primary, suppression_requirement, 1985, 0.75).
narrative_ontology:measurement(dign_su_t2000, dignified_death__autonomy_primary, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(dign_su_t2010, dignified_death__autonomy_primary, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(dign_su_t2024, dignified_death__autonomy_primary, suppression_requirement, 2024, 0.7).


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
