% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__proportionality_reading, []).

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
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Proportionality Principle for Public Health Coercion
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint defines the boundary of legitimate public health coercion
 *   based on the proportionality principle: the severity of interventions
 *   must match the severity and transmissibility of the disease. For example,
 *   measles (high R0, severe outcomes) justifies mandates, while seasonal flu
 *   (lower R0, generally milder outcomes) typically does not. This reading
 *   aims for a moderate level of extraction, as it acknowledges the necessity
 *   of some coercion for collective good but limits its scope. It is claimed
 *   as a Tangled Rope because it genuinely coordinates public health while
 *   extracting autonomy from individuals, requiring active enforcement to
 *   maintain this balance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.45).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.6).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Proportionality Principle for Public Health Coercion").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, '2916810a-bb14-4c1d-9dd5-f53163ea6c3e').
narrative_ontology:cs_kernel_codification('2916810a-bb14-4c1d-9dd5-f53163ea6c3e', formalized).
narrative_ontology:cs_authority_grounding('2916810a-bb14-4c1d-9dd5-f53163ea6c3e', lineage).
narrative_ontology:cs_interpretation_layer_present('2916810a-bb14-4c1d-9dd5-f53163ea6c3e').
narrative_ontology:cs_reading_relation('2916810a-bb14-4c1d-9dd5-f53163ea6c3e', coercion_legitimacy_boundary__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('2916810a-bb14-4c1d-9dd5-f53163ea6c3e', coercion_legitimacy_boundary__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('2916810a-bb14-4c1d-9dd5-f53163ea6c3e', foundational, coercion_must_be_proportionate_to_threat).
narrative_ontology:cs_axiom_status(coercion_must_be_proportionate_to_threat, holdable).
narrative_ontology:cs_axiom_grounding('2916810a-bb14-4c1d-9dd5-f53163ea6c3e', coercion_must_be_proportionate_to_threat, deontological).
narrative_ontology:cs_axiom('2916810a-bb14-4c1d-9dd5-f53163ea6c3e', secondary, individual_autonomy_is_defeasible_by_collective_harm).
narrative_ontology:cs_axiom_status(individual_autonomy_is_defeasible_by_collective_harm, holdable).
narrative_ontology:cs_axiom_grounding('2916810a-bb14-4c1d-9dd5-f53163ea6c3e', individual_autonomy_is_defeasible_by_collective_harm, deontological).
narrative_ontology:cs_reference_frame('2916810a-bb14-4c1d-9dd5-f53163ea6c3e', balancing_rights_and_duties).
narrative_ontology:cs_drift_state('2916810a-bb14-4c1d-9dd5-f53163ea6c3e', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2916810a-bb14-4c1d-9dd5-f53163ea6c3e', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, general_public).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, individuals_subject_to_mandates).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, religious_objectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting public health, they assess disease threats and recommend or implement coercive measures. They benefit from the ability to act decisively in crises but are constrained by legal and ethical challenges to their authority.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from reduced disease transmission and protection from severe illness. They generally support measures that protect collective health, but their support can wane if measures are perceived as disproportionate or overreaching.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, general_public, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct costs of coercive measures, such as mandatory vaccination or isolation. Their autonomy is curtailed, and they may face social or economic penalties for non-compliance. Their ability to resist is limited by legal enforcement.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, individuals_subject_to_mandates, payer,
    powerless, immediate, constrained, local).

% Face a conflict between religious beliefs and public health mandates. Their identity is often deeply tied to their objections, making exit (compliance) a profound personal cost. They often seek legal exemptions or challenge mandates in court.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, religious_objectors, payer,
    moderate, generational, identity_locked, national).

% Adjudicate challenges to public health mandates, balancing state powers against individual rights. Their rulings shape the boundaries of legitimate coercion and influence future policy. They operate within a framework of legal precedent and constitutional principles.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for public health interventions that balances collective safety with individual liberties, ensuring that coercive measures are applied judiciously and only when justified by the severity of the threat.
% TRANSFER_FUNCTION: Transfers a degree of individual autonomy to the state in exchange for collective health protection, with the extent of transfer determined by the proportionality of the disease threat.
% ABSENT_VOICES: Those who advocate for absolute bodily autonomy, regardless of public health risk, are often marginalized in policy debates when severe threats emerge. Their arguments are heard in courts but rarely shape the initial policy response.
% DISAPPEARANCE_RATIONALE: If the proportionality principle vanished, public health authorities would either over-mandate (leading to widespread resistance and rights violations) or under-mandate (leading to uncontrolled outbreaks), and the legal system would lack a coherent basis for adjudicating disputes. The balance between individual rights and collective good would collapse.
% FOUNDING_PROBLEM: To prevent arbitrary state overreach in public health crises while enabling effective responses to genuine threats, by linking the legitimacy of coercion to the severity and transmissibility of the disease.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, medical ethicists, and historical analyses of past epidemics (e.g., smallpox, polio) corroborate the ongoing need for a proportionality framework to manage public health powers responsibly. The public health authorities themselves, while sometimes pushing boundaries, generally acknowledge the principle's necessity.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(coercion_legitimacy_boundary__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__proportionality_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__proportionality_reading_tests).
:- end_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because it allows for significant curtailment of individual autonomy in severe cases, but not universally. Suppression is moderate-high (0.6) as it requires active legal and social enforcement to ensure compliance with mandates. Theater ratio is low (0.1) because the justification for coercion is generally tied to real epidemiological data and public health outcomes, with little performative maintenance. The victim set varies by pathogen: for measles, those mandated to vaccinate are victims; for flu, the constraint prevents mandates, so no victims are created by the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities view this as a necessary framework for effective disease control, balancing rights with responsibilities. Individuals subject to mandates, particularly those with strong objections, experience it as a direct infringement on their autonomy. Constitutional courts interpret and enforce this boundary, often mediating between these perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and the general public are beneficiaries, gaining collective protection. Individuals subject to mandates and religious objectors are payers, bearing the costs of curtailed autonomy. Constitutional courts act as observers, adjudicating the application of the principle.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling legitimate, albeit coercive, public health measures as pure extraction. By acknowledging both the coordination function (disease control) and the extraction (autonomy curtailment), it highlights the inherent tension that must be managed. The proportionality principle is intended to prevent mandatrophy by ensuring that the mandate's scope remains tied to its original, live problem (disease threat).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disease_severity_measurement_ambiguity,
    'How is ''disease severity'' objectively measured and agreed upon, especially for novel pathogens or those with highly variable individual outcomes?',
    'Standardized, transparent epidemiological metrics (e.g., R0, CFR, DALYs) adopted by international health organizations, with clear thresholds for triggering different levels of coercive intervention.',
    'If severity metrics are ambiguous or contested, the proportionality principle becomes subjective, potentially leading to arbitrary coercion or insufficient response. This would increase effective extractiveness for individuals and reduce the coordination function for public health.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disease_severity_measurement_ambiguity, empirical, 'Ambiguity in defining and measuring disease severity, which is central to the proportionality principle.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint truly a ''proportionality_reading'' of the ''coercion_legitimacy_boundary'' kernel, or does it lean more towards ''public_health_primary'' or ''bodily_autonomy_primary'' in practice?',
    'Analysis of judicial rulings and legislative debates over time: if rulings consistently emphasize balancing and context-dependency, it supports the proportionality reading. If they consistently prioritize one side, it suggests a different dominant reading.',
    'If this reading is misidentified, the analysis of public health policy will be flawed, potentially underestimating extraction (if it''s actually ''public_health_primary'') or overestimating it (if it''s ''bodily_autonomy_primary'').',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''coercion_legitimacy_boundary'' kernel. This omega documents the irreducible uncertainty of its precise alignment with the proportionality principle versus sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t1900, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(coer_tr_t1950, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(coer_tr_t2000, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(coer_tr_t2024, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(coer_be_t1900, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(coer_be_t1950, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(coer_be_t2000, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(coer_be_t2024, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t1900, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(coer_su_t1950, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(coer_su_t2000, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(coer_su_t2024, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__proportionality_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
