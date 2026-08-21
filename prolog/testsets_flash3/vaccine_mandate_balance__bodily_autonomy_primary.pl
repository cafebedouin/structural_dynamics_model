% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__bodily_autonomy_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary in Public Health Mandates
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story models the 'bodily_autonomy_primary' reading of the
 *   'vaccine_mandate_balance' kernel. It asserts that individual consent to
 *   medical intervention is inviolable, and the state cannot compel such
 *   intervention, even for collective benefit. From this perspective, vaccine
 *   mandates are highly extractive and suppressive, targeting individuals who
 *   assert their autonomy. The immunocompromised, while at risk, are not
 *   considered victims of the constraint itself, as their risk is framed as
 *   an inherent aspect of a free society where individual liberty is
 *   paramount.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, 0.85).
domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, 0.9).
domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "Bodily Autonomy as Primary in Public Health Mandates").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, 'bb7da91c-d83c-41c7-9653-a5c155e33949').
narrative_ontology:cs_kernel_codification('bb7da91c-d83c-41c7-9653-a5c155e33949', formalized).
narrative_ontology:cs_authority_grounding('bb7da91c-d83c-41c7-9653-a5c155e33949', lineage).
narrative_ontology:cs_interpretation_layer_present('bb7da91c-d83c-41c7-9653-a5c155e33949').
narrative_ontology:cs_reading_relation('bb7da91c-d83c-41c7-9653-a5c155e33949', vaccine_mandate_balance__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('bb7da91c-d83c-41c7-9653-a5c155e33949', vaccine_mandate_balance__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('bb7da91c-d83c-41c7-9653-a5c155e33949', foundational, bodily_autonomy_inviolable).
narrative_ontology:cs_axiom_status(bodily_autonomy_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('bb7da91c-d83c-41c7-9653-a5c155e33949', bodily_autonomy_inviolable, deontological).
narrative_ontology:cs_axiom('bb7da91c-d83c-41c7-9653-a5c155e33949', foundational, state_compulsion_illegitimate_medical).
narrative_ontology:cs_axiom_status(state_compulsion_illegitimate_medical, holdable).
narrative_ontology:cs_axiom_grounding('bb7da91c-d83c-41c7-9653-a5c155e33949', state_compulsion_illegitimate_medical, deontological).
narrative_ontology:cs_reference_frame('bb7da91c-d83c-41c7-9653-a5c155e33949', individual_rights_supremacy).
narrative_ontology:cs_drift_state('bb7da91c-d83c-41c7-9653-a5c155e33949', contemporary_pandemic_response, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('bb7da91c-d83c-41c7-9653-a5c155e33949', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, state_public_health_authorities).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_individuals_coerced).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, healthcare_workers_mandated).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, employees_facing_termination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who face loss of employment, education, or access to public spaces due to vaccine mandates, despite their assertion of bodily autonomy. They bear the direct costs of non-compliance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_individuals_coerced, payer,
    powerless, immediate, trapped, national).

% Entities that issue and enforce vaccine mandates, believing they are acting for the collective good. They benefit from increased public health compliance and reduced disease burden, but face legal challenges.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Healthcare professionals who are required to be vaccinated to maintain their employment. Their professional identity and career path make exit difficult, even if they object to the mandate on autonomy grounds.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, healthcare_workers_mandated, payer,
    moderate, biographical, identity_locked, local).

% Workers in various sectors who face job loss if they do not comply with vaccine mandates. Their economic security is directly threatened, limiting their practical exit options.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, employees_facing_termination, payer,
    powerless, immediate, constrained, local).

% Vulnerable individuals who rely on herd immunity for protection. From this reading's perspective, their exposure risk is an inherent part of a free society where individual liberty is paramount, and they are not considered 'victims' of the constraint itself, but rather of the disease environment.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_exposed, excluded,
    powerless, immediate, trapped, local).

% Organizations and individuals who champion individual rights against state overreach. They actively challenge vaccine mandates in courts and public discourse, aligning with the bodily autonomy primary reading.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading asserts no legitimate coordination function for state-compelled medical intervention, viewing it as an infringement on fundamental rights rather than a coordination problem.
% TRANSFER_FUNCTION: Transfers the right to make personal medical decisions from the individual to the state, in exchange for perceived collective health benefits. It also transfers the burden of disease risk from the general population to those who assert autonomy.
% ABSENT_VOICES: The collective voice of those who prioritize public health over individual autonomy in extreme circumstances is absent from this reading's core premise, as their concerns are deemed secondary to individual rights.
% DISAPPEARANCE_RATIONALE: If the principle of inviolable individual consent vanished, the state's power to compel medical interventions would expand dramatically, fundamentally altering the relationship between citizens and government regarding health decisions. This would lead to a complete reorganization of public health policy and individual rights frameworks.
% FOUNDING_PROBLEM: The historical problem of state overreach and infringement on individual liberties, particularly concerning personal bodily integrity and medical decisions.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars specializing in constitutional rights, civil liberties organizations, and historical precedents of medical ethics (e.g., Nuremberg Code) corroborate the ongoing relevance of protecting individual autonomy against state compulsion, from outside the immediate beneficiary set of state authorities.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_balance__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the profound impact on individual liberty and the imposition of medical procedures against consent. The high suppression (0.90) stems from the severe penalties for non-compliance (job loss, exclusion from public life) and the lack of viable alternatives for those who wish to remain in society. The low theater ratio (0.10) indicates that the enforcement is direct and functional, not performative; the state genuinely intends to compel compliance. The claimed type is 'snare' because the coordination story (collective health) is seen as a cover for extraction of individual rights, maintained through coercion and suppression of alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state public health authorities, the mandates might be seen as a 'rope' or 'scaffold' for collective coordination. However, from the 'bodily_autonomy_primary' reading, the same structure is experienced as a 'snare' due to the fundamental violation of individual rights and the coercive mechanisms employed.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated individuals, healthcare workers, and employees facing termination are direct targets (payers) of the constraint, experiencing high directionality (d near 1.0) due to the direct imposition and severe consequences. State public health authorities are beneficiaries (d near 0.0) as they achieve their policy goals through the constraint. Immunocompromised individuals are excluded from the direct beneficiary/victim analysis of this constraint, as their situation is framed as a consequence of the disease environment rather than the mandate itself under this specific reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling coercive state action as mere 'coordination.' By identifying the constraint as a snare, it highlights the extractive nature of compelling medical interventions against individual will, even if framed as a public good. It emphasizes that the mandate's persistence relies on coercion, not voluntary participation, and that there are clear victims whose autonomy is suppressed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_benefit_vs_individual_harm,
    'Does the collective benefit of vaccine mandates (reduced disease transmission, protection of vulnerable populations) genuinely outweigh the individual harm (infringement on bodily autonomy, economic penalties for non-compliance)?',
    'Empirical studies on the efficacy of mandates in achieving public health goals versus documented individual harms, combined with a normative framework for weighing these values.',
    'If collective benefit is demonstrably low or individual harm is disproportionately high, the ''snare'' classification is strengthened. If the collective benefit is overwhelming and individual harm minimal, this reading''s premise would be challenged, potentially shifting the constraint towards a ''tangled_rope'' or ''scaffold'' from a different perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_benefit_vs_individual_harm, empirical, 'The balance between public health outcomes and individual rights.').

omega_variable(
    natural_right_vs_social_contract,
    'Is bodily autonomy an absolute, inviolable natural right, or is it a right that can be limited by a social contract for the collective good under specific circumstances?',
    'Philosophical and legal debate on the foundations of rights, and the interpretation of constitutional protections in public health emergencies.',
    'If autonomy is absolute, the ''snare'' classification is robust. If it is context-dependent, the constraint''s classification could shift towards a ''tangled_rope'' or ''scaffold'' if the conditions for limitation are met, as argued by sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_right_vs_social_contract, conceptual, 'The philosophical grounding of bodily autonomy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (e.g., job loss, exclusion) primarily structural (external barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-mandate-removal trajectory: if suppression (e.g., self-exclusion from certain activities) persists after the legal/economic mandate is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the snare more insidious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for vaccine mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__bodily_autonomy_primary, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 5, 0.12).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 5, 0.83).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 10, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 5, 0.88).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 10, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__bodily_autonomy_primary, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vaccine_mandate_balance' kernel, focusing on individual bodily autonomy. Other readings (public_health_primary, proportionality_reading) offer alternative structural interpretations of the same underlying societal challenge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
