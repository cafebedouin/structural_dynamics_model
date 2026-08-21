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
 *   constraint_id: vaccine_mandate_balance__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary in Medical Mandates
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'bodily_autonomy_primary' reading
 *   of the 'vaccine_mandate_balance' kernel. From this perspective,
 *   individual consent to medical intervention is an inviolable right, and
 *   the state cannot compel such intervention, regardless of any claimed
 *   collective benefit. The constraint is viewed as a fundamental barrier
 *   against state overreach, and any attempt to impose mandates is seen as a
 *   highly extractive and suppressive act against individuals.
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
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "Bodily Autonomy as Primary in Medical Mandates").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, 'b323b0ee-1a53-4420-83c2-b0c27f32a6af').
narrative_ontology:cs_kernel_codification('b323b0ee-1a53-4420-83c2-b0c27f32a6af', formalized).
narrative_ontology:cs_authority_grounding('b323b0ee-1a53-4420-83c2-b0c27f32a6af', lineage).
narrative_ontology:cs_interpretation_layer_present('b323b0ee-1a53-4420-83c2-b0c27f32a6af').
narrative_ontology:cs_reading_relation('b323b0ee-1a53-4420-83c2-b0c27f32a6af', vaccine_mandate_balance__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('b323b0ee-1a53-4420-83c2-b0c27f32a6af', vaccine_mandate_balance__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('b323b0ee-1a53-4420-83c2-b0c27f32a6af', foundational, bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('b323b0ee-1a53-4420-83c2-b0c27f32a6af', bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('b323b0ee-1a53-4420-83c2-b0c27f32a6af', secondary, state_cannot_compel_medical_acts).
narrative_ontology:cs_axiom_status(state_cannot_compel_medical_acts, holdable).
narrative_ontology:cs_axiom_grounding('b323b0ee-1a53-4420-83c2-b0c27f32a6af', state_cannot_compel_medical_acts, conventional).
narrative_ontology:cs_reference_frame('b323b0ee-1a53-4420-83c2-b0c27f32a6af', post_nuremberg_code_era).
narrative_ontology:cs_drift_state('b323b0ee-1a53-4420-83c2-b0c27f32a6af', contemporary_pandemic_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b323b0ee-1a53-4420-83c2-b0c27f32a6af', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, individual_liberty_advocates).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_individuals_under_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals and groups champion the principle of absolute bodily autonomy, viewing any state compulsion for medical intervention as a fundamental violation of rights. They benefit from the upholding of this principle, even if it means accepting collective risks.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, individual_liberty_advocates, beneficiary,
    organized, civilizational, analytical, global).

% Individuals who, for various reasons, choose not to receive mandated medical interventions (e.g., vaccines) and face direct coercion, penalties, or exclusion from public life. From this reading's perspective, they are the direct victims of state overreach.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_individuals_under_mandate, payer,
    powerless, immediate, trapped, national).

% Government bodies responsible for public health and law enforcement. They are seen as the agents attempting to compel medical interventions, thereby violating individual autonomy. Their actions are the source of the constraint's extraction and suppression.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, state_authorities, agenda_setter,
    institutional, biographical, constrained, national).

% Experts and administrators tasked with protecting collective health. While they may act with good intentions, from this reading's perspective, their recommendations for mandates contribute to the violation of individual rights.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, public_health_officials, agenda_setter,
    institutional, biographical, constrained, national).

% Individuals whose health is directly threatened by the presence of unvaccinated persons. In this reading, their vulnerability is acknowledged but not prioritized over the absolute right to bodily autonomy; their safety is seen as a risk accepted in a free society, not a basis for compelling others.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_individuals, excluded,
    powerless, immediate, trapped, local).

% Legal and philosophical experts who analyze the constitutional and ethical implications of state power versus individual rights. They observe and interpret the contest without directly participating in the enforcement or suffering its effects.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__bodily_autonomy_primary, individual_liberty_advocates).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Upholds the fundamental principle of individual bodily autonomy and consent against state overreach, ensuring that medical decisions remain solely with the individual.
% TRANSFER_FUNCTION: Prevents the transfer of control over one's body and medical decisions from the individual to the state, thereby protecting personal liberty.
% ABSENT_VOICES: Advocates for 'public_health_primary' and 'proportionality_reading' would argue that collective well-being and the protection of vulnerable populations are being neglected. Immunocompromised individuals, whose safety is directly impacted, are also excluded from the primary consideration of this reading.
% DISAPPEARANCE_RATIONALE: If the principle of inviolable individual consent vanished, states could compel medical interventions without limit, fundamentally altering the relationship between citizens and the state regarding health, privacy, and personal liberty. This would lead to a profound societal reorganization.
% FOUNDING_PROBLEM: Preventing state tyranny and medical authoritarianism over the individual body and conscience, particularly in the wake of historical abuses of medical power.
% FOUNDING_PROBLEM_CORROBORATION: Historical documents (e.g., Nuremberg Code, Universal Declaration of Human Rights), constitutional amendments protecting individual liberties, and numerous legal precedents from outside state authorities consistently corroborate the ongoing relevance of this founding problem.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.85) because any compelled medical intervention is considered a severe violation of individual rights, extracting fundamental autonomy. Suppression is also very high (0.90) as state mandates typically involve legal penalties, social exclusion, or loss of employment, effectively coercing compliance. Theater ratio is low (0.10) because the constraint is about a core, non-performative principle; its defense or violation is direct. Accessibility collapse is moderate (0.60) as physical alternatives (refusing intervention) exist, but legal and social alternatives are severely curtailed. Resistance is high (0.80) due to the strong opposition from those who champion individual liberty and those directly affected by mandates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individual liberty advocates, this constraint is a vital 'rope' or even a 'mountain' protecting fundamental rights. However, from the perspective of state authorities or public health advocates (who are not the primary focus of this reading), the same principle might be seen as a 'snare' preventing necessary collective action. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual liberty advocates are the primary beneficiaries, as the constraint upholds their core principle (d near 0.0). Unvaccinated individuals under mandate are the direct targets/victims, bearing the full cost of any compulsion (d near 1.0). State and public health authorities are the agenda-setters, whose actions are seen as the source of extraction. Immunocompromised individuals are excluded, as their collective safety is not the primary consideration in this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_benefit_definition,
    'How is ''collective benefit'' defined and measured, and does its definition inherently conflict with individual rights in a way that makes compromise impossible?',
    'Philosophical analysis of rights theory and empirical studies on the actual impact of collective health measures versus individual liberty infringements.',
    'If ''collective benefit'' is found to be inherently incommensurable with absolute individual rights, this reading''s ''snare'' classification for mandates is strengthened. If a commensurable framework is possible, the conflict might be reclassified as a ''tangled_rope'' with a coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_benefit_definition, conceptual, 'Ambiguity in the definition and measurement of ''collective benefit'' versus individual harm.').

omega_variable(
    nature_of_compulsion,
    'What constitutes ''compulsion'' in this context? Is it only direct physical force, or does it include indirect pressures like social exclusion, employment termination, or denial of services?',
    'Legal precedent and ethical consensus on the threshold of coercion. Analysis of the ''exit options'' available to individuals under various mandate regimes.',
    'If indirect pressures are considered compulsion, the ''suppression'' and ''extractiveness'' metrics are robustly justified. If only direct physical force is considered compulsion, these metrics might be lower, potentially shifting the classification towards a ''tangled_rope'' if a coordination function is also acknowledged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nature_of_compulsion, empirical, 'Defining the scope of ''compulsion'' in medical mandates.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, distinct reading of the ''vaccine_mandate_balance'' kernel, or is it merely an extreme point on a continuum with the ''proportionality_reading''?',
    'Analysis of the foundational axioms: if this reading''s axioms are truly incommensurable with those of the proportionality reading, it is distinct. If they share underlying premises but differ only in degree, it''s a continuum.',
    'If distinct, the ''forecloses'' relationship with ''public_health_primary'' is robust. If a continuum, the ''coexists_with'' relationship with ''proportionality_reading'' might become ''influences'', indicating a less fundamental divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Clarifying the distinctness of this kernel reading from its siblings.').


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
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 10, 0.83).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 15, 0.84).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 10, 0.88).
narrative_ontology:measurement(vacc_su_t15, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 15, 0.89).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 20, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__bodily_autonomy_primary, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
