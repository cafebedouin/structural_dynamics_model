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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Bodily Autonomy as Primary Right Against Compelled Medical Intervention
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'bodily_autonomy_primary' reading
 *   of the 'vaccine_mandate_balance' kernel. It posits that individual
 *   consent to medical intervention is inviolable, and the state cannot
 *   compel such intervention regardless of perceived collective benefit. The
 *   story focuses on the structural reality when this principle is challenged
 *   by state-imposed vaccine mandates, leading to the coercion of
 *   individuals. The 'claimed_type' of Snare reflects the experience of
 *   individuals when this foundational principle is violated through active
 *   enforcement and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, 0.9).
domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, 0.85).
domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, extractiveness, 0.9).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "Bodily Autonomy as Primary Right Against Compelled Medical Intervention").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, 'e6fa81b6-3477-4abf-bc10-c18c41c3044c').
narrative_ontology:cs_kernel_codification('e6fa81b6-3477-4abf-bc10-c18c41c3044c', formalized).
narrative_ontology:cs_authority_grounding('e6fa81b6-3477-4abf-bc10-c18c41c3044c', lineage).
narrative_ontology:cs_interpretation_layer_present('e6fa81b6-3477-4abf-bc10-c18c41c3044c').
narrative_ontology:cs_reading_relation('e6fa81b6-3477-4abf-bc10-c18c41c3044c', vaccine_mandate_balance__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('e6fa81b6-3477-4abf-bc10-c18c41c3044c', vaccine_mandate_balance__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('e6fa81b6-3477-4abf-bc10-c18c41c3044c', foundational, individual_bodily_autonomy_inviolable).
narrative_ontology:cs_axiom_status(individual_bodily_autonomy_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('e6fa81b6-3477-4abf-bc10-c18c41c3044c', individual_bodily_autonomy_inviolable, deontological).
narrative_ontology:cs_axiom('e6fa81b6-3477-4abf-bc10-c18c41c3044c', secondary, state_cannot_compel_medical_intervention).
narrative_ontology:cs_axiom_status(state_cannot_compel_medical_intervention, holdable).
narrative_ontology:cs_axiom_grounding('e6fa81b6-3477-4abf-bc10-c18c41c3044c', state_cannot_compel_medical_intervention, deontological).
narrative_ontology:cs_reference_frame('e6fa81b6-3477-4abf-bc10-c18c41c3044c', liberal_constitutional_order_individual_rights_supremacy).
narrative_ontology:cs_drift_state('e6fa81b6-3477-4abf-bc10-c18c41c3044c', public_health_crisis_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e6fa81b6-3477-4abf-bc10-c18c41c3044c', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who face direct state compulsion for medical intervention (e.g., vaccination) against their consent, with severe consequences for non-compliance such as loss of employment, exclusion from public spaces, or denial of services. From this reading's perspective, they are the direct targets of extraction when the principle of bodily autonomy is violated.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    powerless, immediate, trapped, national).

% Government bodies and officials responsible for public health policy who implement and enforce vaccine mandates. They justify their actions as necessary for collective benefit and disease control, but from this reading's perspective, they are the agents compelling intervention against individual consent.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, state_public_health_authorities, agenda_setter,
    institutional, biographical, constrained, national).

% Organizations and legal experts who champion individual rights and constitutional protections, actively challenging state overreach in medical matters. They analyze and litigate against policies that infringe upon bodily autonomy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% Individuals with weakened immune systems who face increased health risks from infectious diseases, including those spread by unvaccinated individuals. From the 'bodily_autonomy_primary' reading, their exposure risk is framed as an inherent aspect of a free society where individual liberty is paramount, and thus they are not considered direct victims of the *bodily autonomy constraint* itself, but rather of the disease environment.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_individuals, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This constraint, as a principle of inviolable individual consent, functions to *prevent* state-led coordination that would compel medical intervention, rather than to facilitate it.
% TRANSFER_FUNCTION: When violated, this constraint describes the transfer of the fundamental right to make personal medical decisions from the individual to the state, under duress and compulsion.
% ABSENT_VOICES: Those who advocate for a utilitarian calculus where collective benefit can, under certain circumstances, override individual autonomy. Their arguments for public health primacy or proportionality are structurally excluded by the foundational premise of this reading.
% DISAPPEARANCE_RATIONALE: If the principle of inviolable individual consent vanished overnight, the state would gain unlimited power to compel any medical intervention, fundamentally altering the relationship between citizens and the state, and eradicating a core tenet of liberal constitutional orders.
% FOUNDING_PROBLEM: To prevent state overreach and protect individual liberty and bodily integrity against coercive power, particularly in matters of personal medical decisions and public health emergencies.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, human rights organizations, and historical precedents in medical ethics and jurisprudence consistently attest to the enduring nature of the tension between individual liberty and state power, especially during public health crises. This corroboration comes from sources independent of state public health authorities.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vaccine_mandate_balance__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 0.9, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.9) and suppression (0.85) reflect the severe impact on individuals when the state compels medical intervention against their will, effectively extracting their bodily autonomy. Accessibility collapse is high (0.9) because state mandates often leave individuals with no viable alternatives to compliance without facing significant penalties. Resistance is moderate-high (0.7) due to the deeply held value of bodily autonomy. The theater ratio is low (0.1) because state compulsion is a direct, functional exercise of power, not primarily performative. The temporal measurements show a clear increase in extractiveness and suppression over the interval, reflecting a period where the principle of bodily autonomy was increasingly challenged and violated by state actions.
 *
 * PERSPECTIVAL GAP:
 *   The state public health authorities would likely perceive their actions as a necessary 'rope' or 'scaffold' for public good, aiming to coordinate collective health. However, from the perspective of 'unvaccinated_coerced_individuals' and the 'bodily_autonomy_primary' reading, the same actions constitute a 'snare' that extracts fundamental rights. The engine's computation of per-seat classification from the structural data will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   From this reading's perspective, 'unvaccinated_coerced_individuals' are the direct targets (payers) of the constraint's violation, experiencing high extraction. 'State_public_health_authorities' act as the agenda-setters, enforcing the mandates that violate the principle. 'Civil_liberties_advocates' serve as analytical observers. 'Immunocompromised_individuals' are explicitly 'excluded' as direct victims of *this constraint* because this reading frames their risk as a consequence of a free society's inherent liberties, not as an extraction by the bodily autonomy principle itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal penalties, economic sanctions) or internalized (social pressure, fear of ostracization)?',
    'Post-mandate trajectory of compliance and resistance: if compliance persists or resistance diminishes even after formal mandates are lifted, it suggests a significant internalized component of suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them even in the absence of overt enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of medical mandates.').

omega_variable(
    natural_law_vs_construct_bodily_autonomy,
    'Is individual bodily autonomy a self-evident natural right, or a legal/philosophical construct that can be reinterpreted or limited by societal needs?',
    'Conceptual analysis of foundational legal and ethical texts, and cross-cultural comparative studies of rights frameworks. This is a philosophical, not empirical, resolution.',
    'If a natural right, its violation is a fundamental injustice. If a construct, its limits might be more amenable to redefinition based on collective benefit, potentially lowering the perceived extractiveness of mandates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_construct_bodily_autonomy, conceptual, 'The ontological status of bodily autonomy as a right.').

omega_variable(
    framing_underdetermination_vaccine_mandates,
    'Does the ''bodily_autonomy_primary'' framing accurately capture the core conflict, or does a ''public_health_primary'' or ''proportionality_reading'' offer a more defensible framing of the vaccine mandate kernel?',
    'Analysis of which framing best accounts for the full range of stakeholder experiences and ethical considerations, and which leads to the most coherent and just policy outcomes. This is a preference-driven resolution.',
    'Adopting an alternative framing would fundamentally alter the classification of state actions: what is a ''snare'' under this reading might become a ''rope'' or ''scaffold'' under another, with different beneficiaries and victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_vaccine_mandates, preference, 'Under-determination of the core framing for vaccine mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__bodily_autonomy_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(vacc_tr_t15, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 15, 0.1).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 15, 0.8).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 20, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 5, 0.3).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(vacc_su_t15, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 20, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'vaccine_mandate_balance' kernel, each with different ε values and structural properties. This reading emphasizes individual bodily autonomy as primary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
