% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__public_health_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: Public Health Primary Coercion Legitimacy
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint describes the legal and ethical framework where the
 *   state's duty to prevent collective harm (e.g., through infectious disease
 *   control) is prioritized over individual bodily autonomy. It asserts the
 *   legitimacy of compelling medical interventions, such as mandatory
 *   vaccinations or quarantines, when public health is at significant risk.
 *   This reading frames such compulsion as a necessary, actively enforced
 *   coordination mechanism, albeit one that extracts heavily from individual
 *   liberty.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.78).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.85).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "Public Health Primary Coercion Legitimacy").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, '8788ebd2-707b-4a81-b275-728b8abef819').
narrative_ontology:cs_kernel_codification('8788ebd2-707b-4a81-b275-728b8abef819', formalized).
narrative_ontology:cs_authority_grounding('8788ebd2-707b-4a81-b275-728b8abef819', expertise).
narrative_ontology:cs_interpretation_layer_present('8788ebd2-707b-4a81-b275-728b8abef819').
narrative_ontology:cs_reading_relation('8788ebd2-707b-4a81-b275-728b8abef819', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('8788ebd2-707b-4a81-b275-728b8abef819', coercion_legitimacy_boundary__proportionality_reading, influences).
narrative_ontology:cs_axiom('8788ebd2-707b-4a81-b275-728b8abef819', foundational, collective_harm_prevention_is_primary_state_duty).
narrative_ontology:cs_axiom_status(collective_harm_prevention_is_primary_state_duty, holdable).
narrative_ontology:cs_axiom_grounding('8788ebd2-707b-4a81-b275-728b8abef819', collective_harm_prevention_is_primary_state_duty, deontological).
narrative_ontology:cs_axiom('8788ebd2-707b-4a81-b275-728b8abef819', foundational, individual_autonomy_is_defeasible_by_collective_risk).
narrative_ontology:cs_axiom_status(individual_autonomy_is_defeasible_by_collective_risk, holdable).
narrative_ontology:cs_axiom_grounding('8788ebd2-707b-4a81-b275-728b8abef819', individual_autonomy_is_defeasible_by_collective_risk, deontological).
narrative_ontology:cs_reference_frame('8788ebd2-707b-4a81-b275-728b8abef819', utilitarian_public_health_framework).
narrative_ontology:cs_drift_state('8788ebd2-707b-4a81-b275-728b8abef819', contemporary_pandemic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8788ebd2-707b-4a81-b275-728b8abef819', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, healthcare_system).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, individual_autonomy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting the collective health of the population, they assert the right to compel medical interventions when scientific consensus indicates a severe public health threat. They enforce mandates through legal and administrative means.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Cannot rely on their own immune systems for protection and depend on high population immunity (herd immunity) to avoid severe illness or death. They benefit directly from policies that compel vaccination or other protective measures.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, biographical, trapped, local).

% Are directly subjected to compelled medical interventions (e.g., mandatory vaccination, isolation) or face significant penalties (e.g., fines, restrictions on movement) for non-compliance. Their individual autonomy is overridden for collective benefit.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals, payer,
    powerless, immediate, constrained, local).

% Actively resist state compulsion, arguing for the primacy of individual bodily autonomy and informed consent. They bear the cost of their principles being overridden by state power, often through legal challenges and public protest.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, individual_autonomy_advocates, payer,
    organized, generational, constrained, national).

% Benefits from reduced burden during epidemics and pandemics due to widespread compliance with public health measures. It provides the infrastructure for interventions and is protected from being overwhelmed by severe disease outbreaks.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, healthcare_system, beneficiary,
    institutional, generational, constrained, national).

% Adjudicates the boundaries of state power and individual rights, often upholding public health mandates based on established legal precedents concerning the 'police power' of the state. It provides the enforcement mechanism for compelled interventions.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to prevent the spread of infectious diseases and protect vulnerable populations by establishing a clear hierarchy where collective harm prevention can override individual autonomy.
% TRANSFER_FUNCTION: Transfers the burden of risk and compliance from the collective (especially vulnerable groups) to individuals whose autonomy is compelled, ensuring a more resilient public health outcome.
% ABSENT_VOICES: Those who hold the 'bodily_autonomy_primary' reading are structurally excluded from the decision-making process when this constraint is active; their categorical objections are deemed secondary to collective welfare.
% DISAPPEARANCE_RATIONALE: If the state's ability to compel medical intervention for collective harm prevention vanished, public health crises would escalate, vulnerable populations would be at greater risk, and the healthcare system would face overwhelming strain, leading to a fundamental reorganization of public health governance and individual responsibility.
% FOUNDING_PROBLEM: The historical problem of widespread infectious disease outbreaks overwhelming communities and healthcare systems, necessitating collective action beyond individual choice.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological data, medical consensus on disease transmission, and historical accounts of public health crises (e.g., polio, smallpox) corroborate the ongoing need for collective harm prevention mechanisms, as attested by public health organizations and medical professionals globally.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__public_health_primary, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the significant cost to individual autonomy when the state compels medical intervention. Suppression (0.85) is high because the state actively enforces these mandates, limiting individual exit options. The theater ratio is low (0.1) as the interventions are direct, functional, and aimed at tangible public health outcomes, not mere performance. Accessibility collapse is high (0.75) because, once a mandate is in place, alternatives to compliance are severely limited. Resistance is also high (0.7) due to strong counter-claims from individual rights advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities and vulnerable individuals, this constraint is a vital Rope, ensuring collective safety. From the perspective of those compelled, it operates as a Snare, extracting fundamental rights. The engine's classification as Tangled Rope reflects this inherent tension: a genuine coordination function (public health) coupled with asymmetric extraction (from individual autonomy) requiring active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and the healthcare system are beneficiaries, as they gain the capacity to manage crises and protect populations. Immunocompromised individuals are direct beneficiaries, as their survival often depends on collective immunity. Unvaccinated individuals and individual autonomy advocates are targets, bearing the direct costs of compelled action or suppressed rights. The legal system acts as an agenda-setter, providing the framework for enforcement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''public_health_primary'' reading of the ''coercion_legitimacy_boundary'' kernel?',
    'Analysis of legal precedents, public health policy documents, and ethical frameworks to confirm the explicit prioritization of collective harm prevention over individual autonomy in specific contexts.',
    'If misidentified, the classification of this constraint and its relations to sibling readings would be inaccurate, leading to incorrect mapping of ethical and legal positions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading of the coercion legitimacy kernel.').

omega_variable(
    bodily_autonomy_primary_impact,
    'How would the classification change if the ''bodily_autonomy_primary'' reading were adopted?',
    'Re-evaluate base_properties (especially extractiveness and suppression) from the perspective where medical intervention without consent is categorically impermissible. This would likely result in significantly lower extractiveness and suppression, and a shift in victim/beneficiary sets.',
    'The constraint would likely classify as a Mountain (if individual autonomy is an irreducible limit) or a Rope (if it''s a coordination around consent), with ''unvaccinated_individuals'' moving from victims to beneficiaries of autonomy, and ''immunocompromised_individuals'' potentially entering a victim set due to lack of collective protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bodily_autonomy_primary_impact, conceptual, 'Impact of an alternative reading prioritizing individual autonomy.').

omega_variable(
    proportionality_reading_impact,
    'How would the classification change if the ''proportionality_reading'' were adopted?',
    'Re-evaluate base_properties based on the context-dependent scaling of coercion. This would introduce variability in extractiveness and suppression based on disease severity and transmission dynamics, potentially leading to a more nuanced classification (e.g., a Scaffold for temporary, high-risk situations, or a Rope for low-risk scenarios).',
    'The constraint''s extractiveness and suppression would become highly variable, potentially leading to different classifications depending on the specific public health threat. It would emphasize a dynamic rather than static assessment of coercion legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_reading_impact, empirical, 'Impact of an alternative reading emphasizing proportionality in coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t1900, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(coer_tr_t1930, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 1930, 0.12).
narrative_ontology:measurement(coer_tr_t1960, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(coer_tr_t1990, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(coer_tr_t2010, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(coer_tr_t2024, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(coer_be_t1900, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(coer_be_t1930, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 1930, 0.65).
narrative_ontology:measurement(coer_be_t1960, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 1960, 0.7).
narrative_ontology:measurement(coer_be_t1990, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 1990, 0.72).
narrative_ontology:measurement(coer_be_t2010, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(coer_be_t2024, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t1900, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(coer_su_t1930, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 1930, 0.75).
narrative_ontology:measurement(coer_su_t1960, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 1960, 0.8).
narrative_ontology:measurement(coer_su_t1990, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 1990, 0.82).
narrative_ontology:measurement(coer_su_t2010, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 2010, 0.83).
narrative_ontology:measurement(coer_su_t2024, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'coercion_legitimacy_boundary' kernel, each representing a distinct ethical and legal framework for state-compelled medical intervention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
