% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__public_health_primacy_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: Vaccine Mandate Legitimacy (Public Health Primacy Reading)
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint represents the 'public health primacy' reading of vaccine
 *   mandate legitimacy, where the state's duty to prevent collective harm
 *   justifies mandating vaccination, treating unvaccinated status as an
 *   externality. It is a Tangled Rope because it genuinely coordinates public
 *   health outcomes but does so through asymmetric extraction from
 *   unvaccinated individuals, requiring active enforcement. The claimed type
 *   reflects the framing from the perspective of public health authorities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.65).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.75).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "Vaccine Mandate Legitimacy (Public Health Primacy Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, '8f762b75-22d3-43fc-a300-cf9519cb81c4').
narrative_ontology:cs_kernel_codification('8f762b75-22d3-43fc-a300-cf9519cb81c4', formalized).
narrative_ontology:cs_authority_grounding('8f762b75-22d3-43fc-a300-cf9519cb81c4', lineage).
narrative_ontology:cs_interpretation_layer_present('8f762b75-22d3-43fc-a300-cf9519cb81c4').
narrative_ontology:cs_reading_relation('8f762b75-22d3-43fc-a300-cf9519cb81c4', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('8f762b75-22d3-43fc-a300-cf9519cb81c4', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('8f762b75-22d3-43fc-a300-cf9519cb81c4', foundational, collective_good_supersedes_individual_liberty_in_public_health_crises).
narrative_ontology:cs_axiom_status(collective_good_supersedes_individual_liberty_in_public_health_crises, holdable).
narrative_ontology:cs_axiom_grounding('8f762b75-22d3-43fc-a300-cf9519cb81c4', collective_good_supersedes_individual_liberty_in_public_health_crises, deontological).
narrative_ontology:cs_axiom('8f762b75-22d3-43fc-a300-cf9519cb81c4', foundational, unvaccinated_status_is_a_public_health_externality).
narrative_ontology:cs_axiom_status(unvaccinated_status_is_a_public_health_externality, holdable).
narrative_ontology:cs_axiom_grounding('8f762b75-22d3-43fc-a300-cf9519cb81c4', unvaccinated_status_is_a_public_health_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('8f762b75-22d3-43fc-a300-cf9519cb81c4', public_health_emergency_powers_doctrine).
narrative_ontology:cs_drift_state('8f762b75-22d3-43fc-a300-cf9519cb81c4', post_pandemic_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('8f762b75-22d3-43fc-a300-cf9519cb81c4', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_public).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting public health, they assert the right to mandate vaccination to prevent collective harm. They benefit from increased authority and reduced disease burden, but face political and legal challenges.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Bear the direct costs of mandates (job loss, exclusion from public spaces, social stigma). Their refusal is often rooted in deeply held beliefs or identity, making exit (vaccination) a high-cost option.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_individuals, payer,
    powerless, immediate, identity_locked, local).

% Benefit from reduced disease transmission and a sense of collective safety. They generally support mandates as a necessary measure for public good, but bear no direct costs from the mandate itself.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_public, beneficiary,
    organized, biographical, mobile, national).

% Often tasked with enforcing mandates, balancing compliance with employee retention and legal risks. They benefit from a healthier workforce but incur costs in implementation and potential legal challenges.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, employers, agenda_setter,
    powerful, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, employers, payer).

% Argue against mandates on grounds of individual rights and bodily autonomy, but their arguments are often overridden by the public health imperative in this reading. They are excluded from the primary decision-making process regarding mandate implementation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, civil_liberties_advocates, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to achieve herd immunity and reduce the spread of infectious diseases, ensuring public health and safety by minimizing individual externalities.
% TRANSFER_FUNCTION: Transfers the burden of disease risk from the collective (especially vulnerable populations) to unvaccinated individuals, who bear the costs of compliance or exclusion.
% ABSENT_VOICES: Those who prioritize absolute bodily autonomy or question the scientific consensus on vaccine efficacy/safety are systematically marginalized in this framework, their concerns deemed secondary to the collective good.
% DISAPPEARANCE_RATIONALE: If the legitimacy of mandates vanished, public health authorities would lose a critical tool for managing epidemics, leading to potential increases in disease transmission and a breakdown of collective health strategies. Society would have to find alternative, less coercive, and likely less effective, means of disease control.
% FOUNDING_PROBLEM: The challenge of controlling highly transmissible infectious diseases that pose a significant threat to public health and overwhelm healthcare systems, where individual choices have collective consequences.
% FOUNDING_PROBLEM_CORROBORATION: The scientific and medical community, public health organizations, and international bodies consistently corroborate the ongoing threat of infectious diseases and the efficacy of vaccination as a primary control measure. This corroboration comes from outside the direct beneficiaries of mandate authority.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because unvaccinated individuals bear significant costs (e.g., job loss, social exclusion) for the collective benefit. Suppression is also high (0.75) due to the active enforcement mechanisms (e.g., vaccine passports, employment requirements) and the social pressure to conform. Theater ratio is low (0.1) as the mandates are primarily functional in achieving public health goals, with minimal performative aspects. Accessibility collapse is moderate (0.4) as alternatives to vaccination (e.g., frequent testing, remote work) exist but are often constrained or less desirable. Resistance is high (0.8) reflecting significant public opposition and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities and the vaccinated public experience this as a necessary coordination mechanism for collective safety, while unvaccinated individuals experience it as a coercive and extractive measure that infringes on personal liberty. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and the vaccinated public are beneficiaries (d near 0.0-0.2) as they gain collective protection and authority. Unvaccinated individuals are targets (d near 0.8-1.0) as they bear the costs and face suppression. Employers are dual-positioned, acting as agenda-setters for enforcement while also bearing costs of implementation, placing their d closer to symmetric (0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a Snare (ignoring coordination). It acknowledges the genuine public health coordination function while highlighting the asymmetric extraction and active enforcement required. The 'live' status of the founding problem (controlling infectious disease) suggests it is not a Piton, but the high resistance and contested status of the founding problem indicate ongoing tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_harm_threshold,
    'What is the precise threshold of collective harm that justifies state mandate authority, and is it consistently applied?',
    'Epidemiological modeling combined with ethical frameworks for proportionality, subject to independent review and public deliberation.',
    'A clear, consistently applied threshold would strengthen the legitimacy of mandates; an ambiguous or inconsistently applied threshold would weaken it, potentially shifting the classification towards Snare if the ''harm'' justification is found to be a pretext for control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_harm_threshold, conceptual, 'Ambiguity in the definition and application of ''collective harm'' as a justification for mandates.').

omega_variable(
    externality_quantification,
    'How accurately can the ''externality'' of unvaccinated status be quantified, and does it justify the level of suppression imposed?',
    'Rigorous, peer-reviewed epidemiological and economic studies on disease transmission and healthcare burden, compared against the social and economic costs borne by unvaccinated individuals.',
    'If the externality is found to be minor relative to the suppression, the constraint''s extractiveness would be re-evaluated upward, potentially pushing it closer to a Snare. If the externality is substantial, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(externality_quantification, empirical, 'The empirical basis for treating unvaccinated status as a quantifiable externality.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., self-exclusion, continued social stigma) after the extractive mechanism (e.g., mandate enforcement) is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more resilient to external changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for unvaccinated individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 2020, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t2020, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 2020, 0.05).
narrative_ontology:measurement(vacc_tr_t2021, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 2021, 0.08).
narrative_ontology:measurement(vacc_tr_t2022, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 2022, 0.1).
narrative_ontology:measurement(vacc_tr_t2023, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(vacc_be_t2020, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 2020, 0.5).
narrative_ontology:measurement(vacc_be_t2021, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 2021, 0.6).
narrative_ontology:measurement(vacc_be_t2022, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 2022, 0.65).
narrative_ontology:measurement(vacc_be_t2023, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 2023, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t2020, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(vacc_su_t2021, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 2021, 0.7).
narrative_ontology:measurement(vacc_su_t2022, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 2022, 0.75).
narrative_ontology:measurement(vacc_su_t2023, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 2023, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
