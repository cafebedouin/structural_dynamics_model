% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__public_health_primary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Public Health Primary Vaccine Mandate
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'public health primary' reading of vaccine
 *   mandates, where collective protection is prioritized over individual
 *   consent when voluntary compliance fails to achieve herd immunity and
 *   vulnerable populations face lethal exposure risk. It acknowledges the
 *   coercive nature of mandates but frames it as a necessary cost for a
 *   greater good. The high extractiveness and suppression reflect the direct
 *   imposition on individual autonomy and the active enforcement required.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.7).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.8).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Public Health Primary Vaccine Mandate").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, '7bb772e6-e3c2-40b3-be13-28333219fd2d').
narrative_ontology:cs_kernel_codification('7bb772e6-e3c2-40b3-be13-28333219fd2d', formalized).
narrative_ontology:cs_authority_grounding('7bb772e6-e3c2-40b3-be13-28333219fd2d', expertise).
narrative_ontology:cs_interpretation_layer_present('7bb772e6-e3c2-40b3-be13-28333219fd2d').
narrative_ontology:cs_reading_relation('7bb772e6-e3c2-40b3-be13-28333219fd2d', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('7bb772e6-e3c2-40b3-be13-28333219fd2d', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('7bb772e6-e3c2-40b3-be13-28333219fd2d', foundational, collective_immunity_is_a_public_good).
narrative_ontology:cs_axiom_status(collective_immunity_is_a_public_good, holdable).
narrative_ontology:cs_axiom_grounding('7bb772e6-e3c2-40b3-be13-28333219fd2d', collective_immunity_is_a_public_good, empirically_contingent).
narrative_ontology:cs_axiom('7bb772e6-e3c2-40b3-be13-28333219fd2d', foundational, individual_consent_is_subordinate_to_collective_necessity).
narrative_ontology:cs_axiom_status(individual_consent_is_subordinate_to_collective_necessity, holdable).
narrative_ontology:cs_axiom_grounding('7bb772e6-e3c2-40b3-be13-28333219fd2d', individual_consent_is_subordinate_to_collective_necessity, deontological).
narrative_ontology:cs_reference_frame('7bb772e6-e3c2-40b3-be13-28333219fd2d', population_health_maximization).
narrative_ontology:cs_drift_state('7bb772e6-e3c2-40b3-be13-28333219fd2d', contemporary_public_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7bb772e6-e3c2-40b3-be13-28333219fd2d', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, vulnerable_populations).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, unvaccinated_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, healthcare_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting population health, they implement and enforce vaccine mandates, justifying them as necessary to achieve herd immunity and protect vulnerable groups. They bear the political cost of enforcement but gain public health outcomes.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Immunocompromised individuals, the elderly, and infants who cannot be vaccinated or for whom vaccines are less effective. They directly benefit from increased herd immunity, which reduces their lethal exposure risk. Without mandates, they are at severe risk.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Individuals who are compelled to vaccinate against their will or face social, economic, or legal penalties (e.g., job loss, exclusion from public spaces). Their bodily autonomy is subordinated to the collective good, leading to high personal costs.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, unvaccinated_individuals, payer,
    moderate, biographical, constrained, local).

% Benefit from reduced disease burden, preventing overwhelming surges in hospitalizations and preserving capacity for other medical needs. They are also tasked with administering vaccines and managing compliance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, healthcare_systems, beneficiary,
    institutional, biographical, constrained, national).

% Argue that vaccine mandates infringe on fundamental rights to bodily autonomy and informed consent, even in public health emergencies. They are often excluded from the primary decision-making process but engage in legal challenges and public discourse.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, civil_liberties_advocates, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to achieve herd immunity, protecting the entire population, especially those unable to protect themselves, from infectious disease outbreaks.
% TRANSFER_FUNCTION: Transfers the burden of individual health risk from vulnerable populations to unvaccinated individuals, who are compelled to accept vaccination or face penalties, thereby contributing to collective immunity.
% ABSENT_VOICES: Individuals and groups prioritizing absolute bodily autonomy are often marginalized in public health discourse during crises, their concerns dismissed as undermining collective safety. They would argue for less coercive measures and robust exemptions.
% DISAPPEARANCE_RATIONALE: If vaccine mandates vanished, voluntary compliance would likely drop, leading to lower herd immunity. This would expose vulnerable populations to increased lethal risk, overwhelm healthcare systems during outbreaks, and force a societal reorganization around endemic disease management.
% FOUNDING_PROBLEM: The problem of infectious disease outbreaks threatening population health, where voluntary measures are insufficient to achieve protective herd immunity, leaving vulnerable groups at high risk.
% FOUNDING_PROBLEM_CORROBORATION: Public health data on disease transmission, hospitalization rates, and vaccine efficacy, corroborated by epidemiological studies and medical consensus, attest to the ongoing threat and the effectiveness of mandates in mitigating it. Vulnerable populations' advocacy groups also corroborate the necessity.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__public_health_primary, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) is high due to the direct imposition on individual bodily autonomy and the penalties for non-compliance. Suppression (0.8) is also high, reflecting the active enforcement mechanisms (e.g., vaccine passports, employment mandates) required to ensure compliance. The claimed type is 'tangled_rope' because it genuinely solves a coordination problem (herd immunity) but does so through asymmetric extraction from unvaccinated individuals. Resistance is high (0.75) due to strong opposition from individuals and groups prioritizing bodily autonomy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities and vulnerable populations, this constraint is a necessary, albeit coercive, coordination mechanism. From the perspective of unvaccinated individuals, it is a snare that extracts their bodily autonomy. The engine's per-seat classification should reflect this divergence, with beneficiaries experiencing a 'rope' or 'scaffold' and payers experiencing a 'snare' or 'tangled_rope'.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and vulnerable populations are clear beneficiaries, as the constraint directly serves their interests (population health, reduced exposure risk). Unvaccinated individuals are the primary payers, bearing the direct costs of compelled vaccination or penalties. Healthcare systems also benefit from reduced strain. Civil liberties advocates are excluded, as their core concerns are subordinated by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_of_coercion,
    'Is the level of coercion (suppression) truly necessary to achieve the stated public health goals, or could less restrictive means achieve similar outcomes?',
    'Comparative analysis of jurisdictions with varying levels of mandate enforcement and their respective public health outcomes, controlling for other variables.',
    'If less restrictive means are effective, the constraint''s suppression could be reclassified as excessive, pushing it closer to a pure snare. If coercion is demonstrably necessary, the tangled_rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_coercion, empirical, 'Whether the degree of suppression is proportional to the public health necessity.').

omega_variable(
    long_term_trust_erosion,
    'Does the long-term enforcement of mandates, even if effective in the short term, erode public trust in health authorities, leading to future non-compliance with other public health measures?',
    'Longitudinal studies on public trust in health institutions in jurisdictions with and without mandates, correlated with compliance rates for subsequent public health initiatives.',
    'If trust erosion is significant, the long-term effectiveness of public health interventions could be compromised, suggesting a hidden cost that makes the constraint less beneficial than it appears, potentially shifting its classification towards a snare over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_trust_erosion, empirical, 'Impact of mandates on long-term public trust and future compliance.').

omega_variable(
    bodily_autonomy_vs_collective_good_framing,
    'Is the subordination of individual bodily autonomy to collective health a morally justifiable trade-off, or does it represent an unacceptable infringement on fundamental rights?',
    'This is a fundamental normative question that cannot be resolved empirically. Resolution depends on the adopted ethical framework (e.g., utilitarianism vs. deontology).',
    'If individual autonomy is held as inviolable, this reading would be reclassified as a snare from the perspective of the unvaccinated. If collective good is paramount, the tangled_rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bodily_autonomy_vs_collective_good_framing, preference, 'The core ethical tension between individual rights and collective welfare.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 2020, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(vacc_be_t2020, vaccine_mandate_balance__public_health_primary, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(vacc_be_t2021, vaccine_mandate_balance__public_health_primary, base_extractiveness, 2021, 0.65).
narrative_ontology:measurement(vacc_be_t2022, vaccine_mandate_balance__public_health_primary, base_extractiveness, 2022, 0.7).
narrative_ontology:measurement(vacc_be_t2023, vaccine_mandate_balance__public_health_primary, base_extractiveness, 2023, 0.68).
narrative_ontology:measurement(vacc_be_t2024, vaccine_mandate_balance__public_health_primary, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t2020, vaccine_mandate_balance__public_health_primary, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(vacc_su_t2021, vaccine_mandate_balance__public_health_primary, suppression_requirement, 2021, 0.8).
narrative_ontology:measurement(vacc_su_t2022, vaccine_mandate_balance__public_health_primary, suppression_requirement, 2022, 0.85).
narrative_ontology:measurement(vacc_su_t2023, vaccine_mandate_balance__public_health_primary, suppression_requirement, 2023, 0.82).
narrative_ontology:measurement(vacc_su_t2024, vaccine_mandate_balance__public_health_primary, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_balance__public_health_primary, 0.1).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vaccine_mandate_balance' kernel. This 'public_health_primary' reading prioritizes collective protection, leading to higher extractiveness and suppression for unvaccinated individuals compared to the 'bodily_autonomy_primary' or 'proportionality_reading' siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
