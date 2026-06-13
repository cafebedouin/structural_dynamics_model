% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__proportionality_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Vaccine Mandate Proportionality Principle
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint defines the conditions under which vaccine mandates are
 *   ethically and legally permissible, emphasizing proportionality. It
 *   asserts that mandates are justified only when the severity of the
 *   disease, its transmission risk, and the safety and efficacy of the
 *   vaccine meet specific, high thresholds. Furthermore, it requires that
 *   robust exemptions be available for individuals with medical
 *   contraindications or deeply held conscientious objections. This is one
 *   reading of the broader 'vaccine_mandate_balance' kernel, which also
 *   includes readings prioritizing 'public_health_primary' and
 *   'bodily_autonomy_primary'.
 *
 * KEY AGENTS:
 *   - public_health_authorities: Agenda setter (institutional/generational) — sets and enforces mandate policy based on proportionality.
 *   - vulnerable_populations: Beneficiary (powerless/generational) — benefits from reduced disease transmission due to mandates.
 *   - individuals_with_conscientious_objections: Payer (powerless/biographical) — bears the cost of compliance or exclusion if mandates are applied without robust exemptions.
 *   - individuals_with_medical_contraindications: Payer (powerless/biographical) — bears the cost of exclusion if mandates are applied without robust exemptions.
 *   - judicial_bodies: Observer (institutional/generational) — reviews mandate legality and proportionality, shaping its application.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.4).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.3).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Vaccine Mandate Proportionality Principle").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, 'cce0cf0d-2931-4209-b40c-46ba0199aef6').
narrative_ontology:cs_kernel_codification('cce0cf0d-2931-4209-b40c-46ba0199aef6', formalized).
narrative_ontology:cs_authority_grounding('cce0cf0d-2931-4209-b40c-46ba0199aef6', lineage).
narrative_ontology:cs_interpretation_layer_present('cce0cf0d-2931-4209-b40c-46ba0199aef6').
narrative_ontology:cs_reading_relation('cce0cf0d-2931-4209-b40c-46ba0199aef6', vaccine_mandate_balance__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('cce0cf0d-2931-4209-b40c-46ba0199aef6', vaccine_mandate_balance__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('cce0cf0d-2931-4209-b40c-46ba0199aef6', foundational, state_power_conditional_on_necessity).
narrative_ontology:cs_axiom_status(state_power_conditional_on_necessity, holdable).
narrative_ontology:cs_axiom_grounding('cce0cf0d-2931-4209-b40c-46ba0199aef6', state_power_conditional_on_necessity, deontological).
narrative_ontology:cs_axiom('cce0cf0d-2931-4209-b40c-46ba0199aef6', foundational, individual_rights_defeasible_by_proportional_harm).
narrative_ontology:cs_axiom_status(individual_rights_defeasible_by_proportional_harm, holdable).
narrative_ontology:cs_axiom_grounding('cce0cf0d-2931-4209-b40c-46ba0199aef6', individual_rights_defeasible_by_proportional_harm, deontological).
narrative_ontology:cs_reference_frame('cce0cf0d-2931-4209-b40c-46ba0199aef6', liberal_constitutional_balance).
narrative_ontology:cs_drift_state('cce0cf0d-2931-4209-b40c-46ba0199aef6', contemporary_pandemic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cce0cf0d-2931-4209-b40c-46ba0199aef6', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, individuals_with_conscientious_objections).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, individuals_with_medical_contraindications).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, employers_and_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for assessing disease risk, vaccine safety, and implementing public health interventions, including mandates. They interpret and apply proportionality thresholds, balancing collective health with individual liberties. Their legitimacy depends on perceived adherence to these principles.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Individuals (e.g., immunocompromised, elderly, infants) who face severe health risks from infectious diseases and rely on high population immunity for protection. They benefit from mandates that reduce overall transmission, but are often trapped by their health status.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, generational, trapped, local).

% Individuals whose deeply held moral, ethical, or religious beliefs preclude vaccination. They face social, professional, or educational exclusion if mandates are enforced without robust, accessible exemptions, making their exit identity-locked.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, individuals_with_conscientious_objections, payer,
    powerless, biographical, identity_locked, local).

% Individuals who cannot safely receive vaccines due to pre-existing medical conditions. They rely on herd immunity for protection and face exclusion or health risks if mandates are not carefully balanced with their medical needs, making their exit trapped by their health.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, individuals_with_medical_contraindications, payer,
    powerless, biographical, trapped, local).

% Courts and tribunals that review the legality and constitutionality of vaccine mandates, often assessing whether they meet proportionality tests and respect individual rights. Their rulings shape the practical application and enforcement of the constraint.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, judicial_bodies, observer,
    institutional, generational, analytical, national).

% Organizations (e.g., hospitals, schools, businesses) that implement and enforce vaccine mandates based on public health guidance and legal frameworks. They bear the administrative and reputational costs of enforcement, but also benefit from a safer environment and reduced liability.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, employers_and_institutions, agenda_setter,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, employers_and_institutions, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__proportionality_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate individual liberties with collective public health needs by establishing clear, context-dependent criteria for vaccine mandates, ensuring they are only used when strictly necessary and justly applied.
% TRANSFER_FUNCTION: Transfers a degree of individual autonomy and choice to public health authorities in exchange for collective protection from infectious disease, conditional on proportionality. It also transfers the burden of proof for mandate justification to the state.
% ABSENT_VOICES: Those who advocate for a purely 'public_health_primary' approach (e.g., some epidemiologists, public health hardliners) would argue this reading is too permissive of individual exemptions, risking collective health. Those advocating for 'bodily_autonomy_primary' (e.g., some civil liberties groups, vaccine skeptics) would argue it is too permissive of state coercion, regardless of proportionality. Both are present in public discourse but are structurally excluded from the internal logic of this specific reading.
% DISAPPEARANCE_RATIONALE: If the proportionality principle vanished, vaccine mandates would either become categorical (as in 'public_health_primary') or impossible (as in 'bodily_autonomy_primary'), leading to a complete reorganization of public health policy, individual rights, and the legal landscape surrounding medical interventions.
% FOUNDING_PROBLEM: The historical tension between state power to protect public health and individual rights to bodily integrity, particularly in the context of compulsory medical interventions during epidemics.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, bioethicists, and public health legal experts (outside the direct beneficiaries of mandates) consistently attest that this tension remains a live and complex problem, requiring ongoing legal and ethical frameworks like proportionality to navigate. Judicial review of mandates frequently reaffirms the ongoing nature of this problem.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).
:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Rope because, when genuinely applied, it aims to coordinate public health protection with individual rights, benefiting both the collective and those who can safely comply, while providing off-ramps for those who cannot. However, its extractiveness (0.4) and suppression (0.3) are non-zero because even proportional mandates impose costs and limit choices. The resistance (0.5) reflects ongoing societal debate and legal challenges regarding the interpretation and application of proportionality. The theater ratio (0.1) is low, indicating that the stated justification (proportionality) largely aligns with its actual operation, though some performative elements may exist in justifying specific thresholds.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities, from their institutional seat, view this as a necessary coordination mechanism to protect the population, with individual costs being a regrettable but proportional trade-off. Individuals with objections or contraindications, from their powerless seat, experience the mandate as a coercive imposition, even with exemptions, due to the high stakes of non-compliance (e.g., job loss, exclusion from public spaces). The engine's per-seat classification should reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and vulnerable populations are beneficiaries (d near 0.0) as they gain collective protection and legitimacy for public health interventions. Individuals with conscientious objections or medical contraindications are targets (d near 1.0) as they bear the direct costs of compliance or exclusion. Judicial bodies are analytical observers (d near 0.5), evaluating the balance without directly benefiting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is designed to prevent mandatrophy by requiring continuous re-evaluation of proportionality. If disease severity or transmission risk decreases, or vaccine safety concerns emerge, the mandate's justification erodes, preventing it from becoming a Piton. If it were to persist without meeting proportionality thresholds, it would drift towards a Snare, extracting compliance without a valid public health justification. The 'contested' status of the founding problem reflects the ongoing debate about whether current health threats justify the level of intervention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_threshold_ambiguity,
    'What specific quantitative thresholds for disease severity, transmission risk, and vaccine safety constitute ''strict proportionality''?',
    'Consensus among independent epidemiological and bioethical bodies, or judicial precedent setting clear evidentiary standards.',
    'Clearer thresholds would reduce arbitrary application, potentially lowering perceived extraction and suppression for individuals. Ambiguity allows for broader interpretation, potentially increasing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_ambiguity, conceptual, 'Ambiguity in defining ''strict proportionality'' for vaccine mandates.').

omega_variable(
    reading_of_vaccine_mandate_balance,
    'Is this constraint a genuine application of proportionality, or a cover for either public health overreach or individualistic refusal?',
    'Empirical review of mandate outcomes against declared thresholds, and analysis of judicial review decisions on exemptions.',
    'If genuinely applied, it functions as a Rope. If it consistently favors public health without robust proportionality, it drifts towards Tangled Rope. If it is consistently undermined by broad exemptions, it drifts towards Piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_of_vaccine_mandate_balance, empirical, 'This constraint is the ''proportionality_reading'' of the ''vaccine_mandate_balance'' kernel. Sibling readings include ''public_health_primary'' (collective protection supersedes individual consent) and ''bodily_autonomy_primary'' (individual consent is inviolable). This reading differs by making mandate legitimacy context-dependent and conditional on disease parameters, rather than categorical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 20, 0.4).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vaccine_mandate_balance__proportionality_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vaccine_mandate_balance' kernel, alongside 'public_health_primary' and 'bodily_autonomy_primary'. Each reading represents a distinct structural claim about the conditions for legitimate vaccine mandates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
