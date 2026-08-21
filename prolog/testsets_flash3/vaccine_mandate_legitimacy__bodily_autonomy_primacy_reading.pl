% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Bodily Autonomy Primacy in Vaccine Mandate Legitimacy
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint represents a reading of vaccine mandate legitimacy that
 *   prioritizes absolute individual bodily autonomy, rendering state coercion
 *   for public health purposes categorically impermissible. It is framed as a
 *   fundamental, natural law, with minimal extraction from those it governs
 *   directly, but with significant costs borne by vulnerable populations who
 *   are exposed to increased health risks. The low extractiveness and
 *   suppression reflect the view that this principle is self-evident and
 *   requires no active enforcement or justification beyond its inherent
 *   truth.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.15).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.05).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, mountain).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Bodily Autonomy Primacy in Vaccine Mandate Legitimacy").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:emerges_naturally(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '00365dc0-1ed1-4f1c-96e5-04961563ef67').
narrative_ontology:cs_kernel_codification('00365dc0-1ed1-4f1c-96e5-04961563ef67', formalized).
narrative_ontology:cs_authority_grounding('00365dc0-1ed1-4f1c-96e5-04961563ef67', lineage).
narrative_ontology:cs_interpretation_layer_present('00365dc0-1ed1-4f1c-96e5-04961563ef67').
narrative_ontology:cs_reading_relation('00365dc0-1ed1-4f1c-96e5-04961563ef67', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('00365dc0-1ed1-4f1c-96e5-04961563ef67', vaccine_mandate_legitimacy__risk_stratification_reading, forecloses).
narrative_ontology:cs_axiom('00365dc0-1ed1-4f1c-96e5-04961563ef67', foundational, bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('00365dc0-1ed1-4f1c-96e5-04961563ef67', bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('00365dc0-1ed1-4f1c-96e5-04961563ef67', foundational, state_coercion_categorically_impermissible_in_medical_context).
narrative_ontology:cs_axiom_status(state_coercion_categorically_impermissible_in_medical_context, holdable).
narrative_ontology:cs_axiom_grounding('00365dc0-1ed1-4f1c-96e5-04961563ef67', state_coercion_categorically_impermissible_in_medical_context, deontological).
narrative_ontology:cs_reference_frame('00365dc0-1ed1-4f1c-96e5-04961563ef67', unfettered_individual_sovereignty).
narrative_ontology:cs_drift_state('00365dc0-1ed1-4f1c-96e5-04961563ef67', contemporary_pandemic_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('00365dc0-1ed1-4f1c-96e5-04961563ef67', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, individuals_opposed_to_mandates).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vulnerable_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their right to refuse medical intervention is upheld as absolute, free from state coercion. They benefit from the absence of mandates and the preservation of individual choice, even if it means bearing personal health risks.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, individuals_opposed_to_mandates, beneficiary,
    moderate, biographical, mobile, national).

% Actively champion the principle of absolute bodily autonomy and frame any state-imposed medical intervention as an impermissible infringement on fundamental rights. They benefit from the legal and social recognition of this principle.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, agenda_setter,
    organized, generational, analytical, national).

% Bear a disproportionately high risk of severe illness or death from vaccine-preventable diseases due to the absence of mandates. Their health and safety are directly impacted by the choices of others, with no effective means of self-protection in a high-transmission environment.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_individuals, payer,
    powerless, immediate, trapped, local).

% Includes the elderly, those with chronic conditions, and young children who cannot be vaccinated or for whom vaccines are less effective. They face increased exposure risk and potential for severe outcomes due to lower population immunity, with limited options to mitigate this risk.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vulnerable_populations, payer,
    powerless, immediate, constrained, local).

% Their ability to implement population-level health interventions, such as vaccine mandates, is severely curtailed. They are excluded from exercising what they perceive as a core duty to protect collective well-being, despite scientific consensus on vaccine efficacy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_authorities, excluded,
    institutional, generational, constrained, national).

% Witness the direct consequences of vaccine-preventable diseases on vulnerable patients and the strain on healthcare systems. They operate within a framework that prioritizes individual autonomy over collective health measures, often leading to moral distress.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, healthcare_providers, observer,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates individual liberty by establishing a clear boundary against state intervention in personal medical decisions, ensuring individuals are free from coercion regarding their bodies.
% TRANSFER_FUNCTION: Transfers the burden of managing public health risks from the state and the general population to vulnerable individuals, who bear the increased exposure risk and health consequences of lower population immunity.
% ABSENT_VOICES: Public health authorities and medical ethicists who prioritize collective well-being and the protection of vulnerable populations are structurally excluded from the decision-making process that establishes this principle as absolute. They would argue for a more balanced approach to autonomy and public good.
% DISAPPEARANCE_RATIONALE: If the principle of absolute bodily autonomy vanished overnight, the legal and ethical landscape around public health interventions would fundamentally shift. States would gain broader authority to implement mandates, and the balance of individual rights versus collective good would be re-evaluated, leading to significant changes in policy and social norms.
% FOUNDING_PROBLEM: The historical problem of state overreach and forced medical procedures, particularly in contexts of eugenics or unethical experimentation, which led to the establishment of strong individual rights against state coercion.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and human rights organizations corroborate the ongoing relevance of protecting individuals from state medical coercion, citing historical abuses and the potential for future overreach. This perspective is widely attested outside of liberty advocacy groups.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, ExtMetricName, E),
    domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain because it asserts a fundamental, unchangeable principle of individual rights. Its extractiveness is low (0.15) from the perspective of those whose autonomy is protected, as it primarily prevents state action rather than imposing costs. However, it creates a structural externality where vulnerable populations (immunocompromised, elderly) bear the costs of reduced collective immunity, making them victims. Suppression is minimal (0.05) because the principle is seen as self-enforcing; resistance is also low (0.05) as it is a foundational claim for many. Accessibility collapse is high (0.9) because, within this reading, alternatives to individual choice (like mandates) are conceptually foreclosed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of those whose autonomy is protected, this is a Mountain of fundamental rights. From the perspective of vulnerable populations, it is a structural condition that imposes severe, unchosen risks. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals opposed to mandates and liberty advocacy movements are beneficiaries, as their core principle is upheld. Immunocompromised and vulnerable populations are victims, as they bear the health risks of this absolute autonomy. Public health authorities are excluded, as their mandate to protect collective health is overridden by this principle.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_right,
    'Is absolute bodily autonomy a genuine natural law, or a constructed legal/ethical principle that benefits identifiable agents (liberty advocacy movements) while imposing costs on others?',
    'Philosophical and legal analysis of the grounding of rights, examining whether the principle is universally derivable or contingent on specific cultural/historical framings. Empirical analysis of the social consequences of its absolute application.',
    'If found to be a constructed principle, its classification would shift from Mountain to a more extractive type (e.g., Tangled Rope or Snare), reflecting the active maintenance and asymmetric distribution of costs and benefits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_right, conceptual, 'Ambiguity between a natural, unchangeable principle and a socially constructed right with beneficiaries.').

omega_variable(
    externality_distribution_ambiguity,
    'To what extent are the health risks borne by vulnerable populations a direct, unavoidable externality of absolute individual autonomy, versus a consequence of insufficient alternative protective measures?',
    'Epidemiological studies comparing health outcomes in jurisdictions with and without mandates, alongside analysis of non-coercive public health interventions (e.g., improved ventilation, targeted support for vulnerable groups).',
    'If risks are largely unavoidable externalities, the classification of this reading as a Mountain (with victims) remains stable. If alternative protective measures could significantly mitigate risks, the ''victim'' status of vulnerable populations might be re-evaluated, potentially shifting the constraint''s overall extractiveness profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_distribution_ambiguity, empirical, 'Clarifying the nature and avoidability of risks borne by vulnerable populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 10, 0.0).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 10, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 10, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
