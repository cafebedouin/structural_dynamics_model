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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: vaccine_mandate_balance__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary in Medical Mandates
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the reading of the vaccine mandate debate that
 *   prioritizes individual bodily autonomy as an inviolable right, asserting
 *   that the state cannot compel medical intervention regardless of potential
 *   collective benefit. It frames any state-imposed mandate as a violation of
 *   this fundamental right, leading to high perceived extraction and
 *   suppression for those subject to mandates. The immunocompromised, while
 *   at risk, are not considered 'victims' of this constraint, as their risk
 *   is seen as an inherent part of a free society where individual liberty is
 *   paramount.
 *
 * KEY AGENTS:
 *   - state_public_health_authorities: Agenda setter (institutional/constrained) — seeks to implement mandates.
 *   - unvaccinated_coerced: Payer/Victim (powerless/trapped) — bears the direct costs of mandates.
 *   - individuals_asserting_autonomy: Beneficiary (organized/mobile) — benefits from the principle being upheld.
 *   - immunocompromised_exposed: Excluded (powerless/constrained) — bears risk but not considered a victim of this specific constraint's operation.
 *   - constitutional_courts: Observer (institutional/analytical) — adjudicates the balance of rights.
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
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, mountain).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "Bodily Autonomy as Primary in Medical Mandates").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).
domain_priors:emerges_naturally(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, '46f98ea2-7f6d-4a70-bbb5-79dbaf622624').
narrative_ontology:cs_kernel_codification('46f98ea2-7f6d-4a70-bbb5-79dbaf622624', formalized).
narrative_ontology:cs_authority_grounding('46f98ea2-7f6d-4a70-bbb5-79dbaf622624', lineage).
narrative_ontology:cs_interpretation_layer_present('46f98ea2-7f6d-4a70-bbb5-79dbaf622624').
narrative_ontology:cs_reading_relation('46f98ea2-7f6d-4a70-bbb5-79dbaf622624', vaccine_mandate_balance__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('46f98ea2-7f6d-4a70-bbb5-79dbaf622624', vaccine_mandate_balance__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('46f98ea2-7f6d-4a70-bbb5-79dbaf622624', foundational, bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('46f98ea2-7f6d-4a70-bbb5-79dbaf622624', bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('46f98ea2-7f6d-4a70-bbb5-79dbaf622624', foundational, state_power_subordinate_to_individual_rights).
narrative_ontology:cs_axiom_status(state_power_subordinate_to_individual_rights, holdable).
narrative_ontology:cs_axiom_grounding('46f98ea2-7f6d-4a70-bbb5-79dbaf622624', state_power_subordinate_to_individual_rights, deontological).
narrative_ontology:cs_reference_frame('46f98ea2-7f6d-4a70-bbb5-79dbaf622624', unconditional_bodily_autonomy).
narrative_ontology:cs_drift_state('46f98ea2-7f6d-4a70-bbb5-79dbaf622624', contemporary_pandemic_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('46f98ea2-7f6d-4a70-bbb5-79dbaf622624', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, individuals_asserting_autonomy).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__bodily_autonomy_primary, individual_liberty_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__bodily_autonomy_primary, informed_consent_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for public health outcomes, they seek to implement measures like vaccine mandates. From this reading's perspective, they are attempting to override a fundamental right, making them a target of the autonomy principle itself.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Individuals who face legal penalties, job loss, or exclusion from public spaces for refusing medical interventions. They bear the direct costs of mandates and experience high suppression.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced, payer,
    powerless, immediate, trapped, local).

% Advocates and groups who actively defend the principle of bodily autonomy. They benefit when this principle is upheld and enforced, even if they are not directly subject to a mandate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, individuals_asserting_autonomy, beneficiary,
    organized, biographical, mobile, national).

% Individuals who face severe health risks from infectious diseases due to compromised immune systems. While they bear the consequences of low vaccination rates, this reading does not frame them as 'victims' of the autonomy principle, but rather as accepting a societal risk inherent to liberty.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_exposed, excluded,
    powerless, biographical, constrained, local).

% Judicial bodies tasked with interpreting constitutional rights, including bodily autonomy, against state powers. They adjudicate disputes and can uphold or strike down mandates based on their reading of fundamental rights.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__bodily_autonomy_primary, individuals_asserting_autonomy).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This constraint primarily coordinates individual liberty by establishing a clear boundary against state overreach in medical decisions, ensuring that individuals can make choices about their own bodies without coercion.
% TRANSFER_FUNCTION: It transfers the burden of collective health risk from the individual to the collective, asserting that the individual's right to refuse medical intervention takes precedence over the state's interest in public health. It also transfers the cost of non-compliance (e.g., job loss, social exclusion) to the individual when mandates are imposed.
% ABSENT_VOICES: The 'public_health_primary' and 'proportionality_reading' perspectives are largely absent from the core framing of this constraint, as their arguments for collective benefit or conditional mandates are rejected by the absolute nature of this autonomy claim. The 'immunocompromised_exposed' are also effectively silenced in this framing, as their vulnerability is not seen as a justification for overriding individual autonomy.
% DISAPPEARANCE_RATIONALE: If the principle of inviolable bodily autonomy disappeared overnight, the state's power to compel medical interventions would expand dramatically. This would fundamentally alter the relationship between the individual and the state, leading to a reorganization of legal frameworks, public health policies, and individual rights.
% FOUNDING_PROBLEM: The constraint was built to address the historical problem of state or medical authority overriding individual consent, particularly in contexts of forced sterilization, experimentation, or public health measures that disregarded individual rights.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as 'live' by human rights organizations, civil liberties advocates, and historical records of medical abuses. Constitutional scholars and legal precedents from outside the immediate beneficiaries also corroborate the ongoing need to protect individual autonomy against state power, even in public health emergencies.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_balance__bodily_autonomy_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, ExtMetricName, E),
    domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(vaccine_mandate_balance__bodily_autonomy_primary),
    narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) and suppression (0.9) reflect the view that compelling medical intervention is a severe infringement on individual liberty, regardless of the state's justification. The 'mountain' claim reflects the belief in the fundamental, unchangeable nature of bodily autonomy. The low theater ratio (0.1) indicates that the enforcement of mandates is seen as a direct, functional imposition, not a performance. The rising extractiveness and suppression over time reflect the increasing intensity of mandate enforcement and the perceived erosion of autonomy during public health crises.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'unvaccinated_coerced', the constraint is a snare, actively extracting their autonomy. From 'individuals_asserting_autonomy', it is a mountain, a fundamental principle to be defended. The state, as 'agenda_setter', views it as a tangled rope, balancing individual rights with collective welfare, but this reading rejects that balance.
 *
 * DIRECTIONALITY LOGIC:
 *   'Individuals_asserting_autonomy' are beneficiaries (d=0.0-0.1) as the constraint upholds their core principle. 'Unvaccinated_coerced' are clear targets (d=0.9-1.0) as they bear the direct costs of compelled intervention. 'State_public_health_authorities' are agenda-setters, but from this reading's perspective, they are acting against the fundamental constraint, making them indirect targets (d=0.7-0.8) of the autonomy principle itself when they attempt to compel. 'Immunocompromised_exposed' are excluded from the direct beneficiary/victim calculus of this specific constraint, as their risk is framed as a consequence of a free society, not a direct extraction by the autonomy principle.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the assertion of bodily autonomy as mere coordination. By framing it as a 'mountain' (albeit one with beneficiaries, triggering FSM), it highlights the claim of fundamental, unchangeable principle. If the underlying principle of autonomy were to atrophy (e.g., through repeated state overrides), it would shift from a contested mountain to a piton or snare, where the 'right' is merely theatrical or a cover for state power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_autonomy,
    'Is bodily autonomy an irreducible natural law, or a constructed legal/ethical principle that benefits identifiable agents?',
    'Philosophical analysis of foundational rights vs. social contract theory; historical analysis of the evolution of consent in medical ethics.',
    'If a natural law, its ''mountain'' classification is robust. If constructed, the presence of beneficiaries (individuals asserting autonomy) would trigger a false summit reclassification to a ''tangled_rope'' or ''snare'' depending on the degree of extraction from those whose autonomy is overridden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_autonomy, conceptual, 'Ambiguity between natural law and constructed principle for bodily autonomy.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''vaccine_mandate_balance'' kernel. Does it accurately represent the ''bodily_autonomy_primary'' reading?',
    'Comparison with canonical texts and judicial interpretations that prioritize individual consent over collective health mandates.',
    'If the reading is misidentified, the classification of this specific constraint would be inaccurate, and its relationship to sibling readings (public_health_primary, proportionality_reading) would be distorted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verifies this constraint as the ''bodily_autonomy_primary'' reading of the ''vaccine_mandate_balance'' kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, job loss) or internalized (social pressure, fear of ostracization)?',
    'Post-mandate suppression trajectory: if suppression persists after legal mandates are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for vaccine mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__bodily_autonomy_primary, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 5, 0.12).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 10, 0.11).
narrative_ontology:measurement(vacc_tr_t15, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 15, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 5, 0.8).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(vacc_su_t15, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 15, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__bodily_autonomy_primary, identity_coordination).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vaccine_mandate_balance' kernel, focusing on bodily autonomy. It is linked to sibling readings that prioritize public health or proportionality, as these readings directly contest the scope and application of individual rights in public health crises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
