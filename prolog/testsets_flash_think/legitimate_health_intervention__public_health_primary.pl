% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__public_health_primary, []).

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
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Public Health Primary Intervention Mandate
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the 'public_health_primary' reading of
 *   the 'legitimate_health_intervention' kernel. It posits that the
 *   legitimacy of public health interventions derives from their measurable
 *   reduction in population-level morbidity and mortality, framing individual
 *   refusal as an imposition of externality. This reading prioritizes
 *   collective health outcomes, justifying coercive measures to achieve them.
 *   The constraint is classified as a Tangled Rope due to its genuine
 *   coordination function (public health protection) coupled with significant
 *   asymmetric extraction and active enforcement against those who do not
 *   comply.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.75).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.8).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.75).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public Health Primary Intervention Mandate").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, '6d416f86-aa53-4e8f-955e-6af357bf1484').
narrative_ontology:cs_kernel_codification('6d416f86-aa53-4e8f-955e-6af357bf1484', formalized).
narrative_ontology:cs_authority_grounding('6d416f86-aa53-4e8f-955e-6af357bf1484', expertise).
narrative_ontology:cs_interpretation_layer_present('6d416f86-aa53-4e8f-955e-6af357bf1484').
narrative_ontology:cs_reading_relation('6d416f86-aa53-4e8f-955e-6af357bf1484', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('6d416f86-aa53-4e8f-955e-6af357bf1484', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('6d416f86-aa53-4e8f-955e-6af357bf1484', foundational, population_health_is_primary_legitimacy_source).
narrative_ontology:cs_axiom_status(population_health_is_primary_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('6d416f86-aa53-4e8f-955e-6af357bf1484', population_health_is_primary_legitimacy_source, deontological).
narrative_ontology:cs_axiom('6d416f86-aa53-4e8f-955e-6af357bf1484', secondary, individual_refusal_is_externality).
narrative_ontology:cs_axiom_status(individual_refusal_is_externality, holdable).
narrative_ontology:cs_axiom_grounding('6d416f86-aa53-4e8f-955e-6af357bf1484', individual_refusal_is_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('6d416f86-aa53-4e8f-955e-6af357bf1484', population_health_maximization).
narrative_ontology:cs_drift_state('6d416f86-aa53-4e8f-955e-6af357bf1484', contemporary_rights_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6d416f86-aa53-4e8f-955e-6af357bf1484', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, healthcare_systems).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, general_public).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, individuals_refusing_intervention).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for population health outcomes, they define and enforce interventions based on epidemiological data. They frame individual refusal as a threat to collective well-being.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Directly benefit from reduced disease transmission, as they are highly vulnerable to severe outcomes. Their health and safety depend on high population immunity.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, immediate, trapped, local).

% Benefits from reduced overall disease burden, stable healthcare systems, and the ability to participate in society with lower risk of infection. They may also bear indirect costs of enforcement.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, general_public, beneficiary,
    moderate, biographical, constrained, national).

% Bear the costs of non-compliance, including restrictions on employment, travel, and access to public spaces. Their refusal is framed as an imposition of externality on the collective.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, unvaccinated_individuals, payer,
    powerless, immediate, identity_locked, local).

% Benefit from reduced patient load during epidemics, allowing them to maintain normal operations and provide care for other conditions. They are key implementers of public health policy.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, healthcare_systems, beneficiary,
    institutional, biographical, constrained, national).

% Often mandated to enforce public health measures (e.g., vaccine requirements) to ensure workplace safety and continuity of operations. They benefit from a healthier workforce but bear administrative costs and potential employee resistance.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, employers, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__public_health_primary, employers, beneficiary).

% Argue for the primacy of individual rights and bodily autonomy, often challenging the scope and proportionality of public health mandates. Their perspective is often marginalized in policy discussions driven by this reading.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, civil_liberties_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To achieve and maintain population-level immunity and reduce the spread of infectious diseases, ensuring collective health and preventing healthcare system overload.
% TRANSFER_FUNCTION: Transfers the burden of disease risk from the general population and vulnerable individuals to those who refuse public health interventions, by imposing social, economic, and access costs on the latter.
% ABSENT_VOICES: Individuals and groups prioritizing absolute bodily autonomy, those with strong philosophical or religious objections to state-mandated medical interventions, and civil liberties organizations are often excluded from the core policy-making process, their concerns reframed as threats to public safety.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the legal and ethical basis for coercive public health measures would collapse. This would likely lead to a significant increase in preventable disease outbreaks, overwhelming healthcare systems, and a fundamental shift in the societal balance between individual liberty and collective responsibility.
% FOUNDING_PROBLEM: The historical and ongoing challenge of controlling infectious diseases that pose a collective threat, where individual actions (or inactions) have direct and measurable population-level consequences.
% FOUNDING_PROBLEM_CORROBORATION: International health organizations (e.g., WHO), national public health agencies (e.g., CDC), and medical professional bodies consistently corroborate the ongoing threat of infectious diseases and the efficacy of population-level interventions. Epidemiological studies provide empirical evidence for the externality of individual refusal.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__public_health_primary, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) reflects the substantial costs imposed on individuals who refuse interventions, such as loss of employment or access. Suppression (0.80) is high due to the active enforcement mechanisms (mandates, restrictions) required to ensure compliance and prevent alternatives. The theater ratio is low (0.10) because the interventions are genuinely aimed at achieving public health outcomes, with minimal performative maintenance. Resistance (0.75) is high, reflecting the ongoing societal debate and individual challenges to these policies. Accessibility collapse (0.70) is also high, as the ability to opt out without consequence is significantly curtailed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities and vulnerable populations, this constraint is a necessary Rope, ensuring collective well-being. From the perspective of those who refuse interventions, it is a Snare, coercively extracting compliance and individual liberty. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities, immunocompromised individuals, healthcare systems, and the general public are beneficiaries, gaining from reduced disease burden and stable health infrastructure. Unvaccinated individuals and those refusing interventions are the primary payers/victims, bearing the direct costs of non-compliance. Employers act as agenda-setters and beneficiaries by enforcing mandates for workplace safety. Civil liberties advocates are excluded, as their arguments for individual autonomy are often sidelined in this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling genuine public health coordination as pure extraction by acknowledging the coordination function. However, the high extractiveness and suppression indicate that the 'coordination' comes at a significant cost to a specific group, preventing it from being classified as a simple Rope. The 'live' status of the founding problem (ongoing infectious disease threats) suggests it is not a Piton, but the contestation around its status highlights the tension between its original mandate and its current operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_of_intervention,
    'Is the severity and scope of the public health intervention proportional to the actual threat posed by the disease and the efficacy of the intervention?',
    'Independent, transparent epidemiological modeling and cost-benefit analysis that includes social and economic costs to individuals, not just health outcomes.',
    'If interventions are found disproportionate, the measured extractiveness and suppression would be re-evaluated as excessive, potentially shifting the classification towards a Snare or a more extractive Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_of_intervention, empirical, 'Assesses whether the intervention''s burden is justified by its benefit.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal mandates, access restrictions) or internalized (social pressure, fear of ostracization)?',
    'Post-mandate-removal studies: if compliance or social pressure persists after legal enforcement is removed, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them even after external barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in public health compliance.').

omega_variable(
    kernel_framing_legitimacy,
    'Is the ''public_health_primary'' framing the only legitimate way to derive authority for health interventions, or are other framings (e.g., bodily autonomy) equally valid sources of legitimacy?',
    'Societal consensus shift or judicial rulings that explicitly re-prioritize individual rights over collective health in specific contexts, or vice-versa.',
    'If alternative framings gain legitimacy, the ''public_health_primary'' reading''s authority would erode, potentially reducing its effective extractiveness and shifting its classification towards a more contested or less coercive type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_legitimacy, conceptual, 'Contestation over the foundational source of legitimacy for public health interventions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(legi_tr_t5, legitimate_health_intervention__public_health_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(legi_tr_t10, legitimate_health_intervention__public_health_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(legi_tr_t15, legitimate_health_intervention__public_health_primary, theater_ratio, 15, 0.1).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__public_health_primary, theater_ratio, 20, 0.1).
narrative_ontology:measurement(legi_tr_t25, legitimate_health_intervention__public_health_primary, theater_ratio, 25, 0.1).
narrative_ontology:measurement(legi_tr_t30, legitimate_health_intervention__public_health_primary, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(legi_be_t5, legitimate_health_intervention__public_health_primary, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(legi_be_t10, legitimate_health_intervention__public_health_primary, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(legi_be_t15, legitimate_health_intervention__public_health_primary, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__public_health_primary, base_extractiveness, 20, 0.73).
narrative_ontology:measurement(legi_be_t25, legitimate_health_intervention__public_health_primary, base_extractiveness, 25, 0.74).
narrative_ontology:measurement(legi_be_t30, legitimate_health_intervention__public_health_primary, base_extractiveness, 30, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(legi_su_t5, legitimate_health_intervention__public_health_primary, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(legi_su_t10, legitimate_health_intervention__public_health_primary, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(legi_su_t15, legitimate_health_intervention__public_health_primary, suppression_requirement, 15, 0.8).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__public_health_primary, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(legi_su_t25, legitimate_health_intervention__public_health_primary, suppression_requirement, 25, 0.79).
narrative_ontology:measurement(legi_su_t30, legitimate_health_intervention__public_health_primary, suppression_requirement, 30, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, vaccine_mandates).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, healthcare_access_rules).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, public_health_emergency_powers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
