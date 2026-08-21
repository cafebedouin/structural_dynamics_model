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
 *   human_readable: Public Health Coercion Proportionality Principle
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality reading' of public health
 *   coercion, where the legitimacy of state mandates (e.g., vaccination,
 *   isolation) scales with the severity and transmissibility of the disease.
 *   Measles, with its high R0 and potential for severe complications, is seen
 *   as justifying mandates, while seasonal flu, with lower severity and R0,
 *   generally does not. This reading seeks a middle ground between absolute
 *   bodily autonomy and unlimited public health power, leading to
 *   case-by-case adjudication and moderate extraction from those subject to
 *   mandates.
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
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Public Health Coercion Proportionality Principle").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, '04e3d596-b0a6-493a-9a2a-fa2038fa55c4').
narrative_ontology:cs_kernel_codification('04e3d596-b0a6-493a-9a2a-fa2038fa55c4', formalized).
narrative_ontology:cs_authority_grounding('04e3d596-b0a6-493a-9a2a-fa2038fa55c4', lineage).
narrative_ontology:cs_interpretation_layer_present('04e3d596-b0a6-493a-9a2a-fa2038fa55c4').
narrative_ontology:cs_reading_relation('04e3d596-b0a6-493a-9a2a-fa2038fa55c4', coercion_legitimacy_boundary__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('04e3d596-b0a6-493a-9a2a-fa2038fa55c4', coercion_legitimacy_boundary__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('04e3d596-b0a6-493a-9a2a-fa2038fa55c4', foundational, coercion_must_be_proportional_to_threat).
narrative_ontology:cs_axiom_status(coercion_must_be_proportional_to_threat, holdable).
narrative_ontology:cs_axiom_grounding('04e3d596-b0a6-493a-9a2a-fa2038fa55c4', coercion_must_be_proportional_to_threat, deontological).
narrative_ontology:cs_axiom('04e3d596-b0a6-493a-9a2a-fa2038fa55c4', foundational, collective_good_can_outweigh_individual_autonomy_conditionally).
narrative_ontology:cs_axiom_status(collective_good_can_outweigh_individual_autonomy_conditionally, holdable).
narrative_ontology:cs_axiom_grounding('04e3d596-b0a6-493a-9a2a-fa2038fa55c4', collective_good_can_outweigh_individual_autonomy_conditionally, deontological).
narrative_ontology:cs_reference_frame('04e3d596-b0a6-493a-9a2a-fa2038fa55c4', ethical_public_health_governance).
narrative_ontology:cs_drift_state('04e3d596-b0a6-493a-9a2a-fa2038fa55c4', contemporary_pandemic_response, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('04e3d596-b0a6-493a-9a2a-fa2038fa55c4', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, individuals_subject_to_mandates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting public health, they advocate for and implement interventions, including mandates, when disease severity and transmission warrant. They balance individual rights against collective well-being, seeking to apply coercion proportionally.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Bear the direct cost of coercion (e.g., mandatory vaccination, isolation). Their autonomy is curtailed for the collective good, but only when the threat is deemed severe enough to justify it. Exit options are limited by legal enforcement and social pressure.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, individuals_subject_to_mandates, payer,
    powerless, immediate, constrained, local).

% Benefit from reduced disease transmission due to mandates, especially those who cannot be vaccinated or are immunocompromised. They advocate for public health measures that protect them, even if coercive for others.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, vulnerable_populations, beneficiary,
    organized, biographical, constrained, local).

% Review the ethical justifications for public health interventions, ensuring that coercion is applied justly and proportionally. They provide guidance and challenge policies that overstep ethical boundaries.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, medical_ethics_boards, observer,
    institutional, generational, analytical, national).

% Argue for the primacy of individual bodily autonomy and against state coercion in medical matters, regardless of proportionality. They are often in opposition to mandates, even those justified by severe public health threats.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, civil_liberties_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public health responses by establishing a framework for when collective action (including coercion) is justified to prevent widespread harm from infectious diseases, balancing individual rights with societal protection.
% TRANSFER_FUNCTION: Transfers a degree of individual bodily autonomy from individuals to the state (or collective) in exchange for reduced risk of severe disease transmission, with the extent of transfer determined by the severity and transmissibility of the pathogen.
% ABSENT_VOICES: Those who prioritize absolute bodily autonomy would object to any form of medical coercion, regardless of proportionality. Their arguments are often marginalized in public health discourse when severe threats are present.
% DISAPPEARANCE_RATIONALE: If this principle vanished, public health authorities would either over-mandate for minor threats (leading to public backlash) or under-mandate for severe threats (leading to uncontrolled epidemics), as the guiding framework for legitimate coercion would be gone. The balance between individual rights and collective safety would collapse.
% FOUNDING_PROBLEM: How to legitimately protect the public from infectious diseases that pose a collective threat, without unduly infringing on individual liberties, particularly when interventions require individual participation.
% FOUNDING_PROBLEM_CORROBORATION: Public health crises throughout history, from smallpox to COVID-19, consistently demonstrate the need for a framework to balance individual and collective rights. Medical ethicists and legal scholars outside of public health agencies corroborate the ongoing relevance of this problem.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate because coercion is not absolute; it is applied only when deemed necessary and proportional. Suppression (0.6) is present as mandates require active enforcement, but it's not total, as legal challenges and exemptions exist. Theater ratio is low (0.1) because the justification for coercion is generally genuine when applied, not performative. The metrics reflect a system that, by its own lights, attempts to be judicious in its application of coercive power.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities perceive this as a necessary and ethical framework for societal protection. Individuals subject to mandates, even when proportional, experience it as a loss of autonomy. The engine's classification will reflect this divergence, with the agenda-setter seat likely computing as a Rope or Scaffold, while the payer seat computes as a Tangled Rope or Snare, depending on the specific context of the mandate.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and vulnerable populations are beneficiaries, as the constraint enables collective protection. Individuals subject to mandates are victims, bearing the direct cost of curtailed autonomy. Civil liberties advocates are excluded, as their fundamental premise (absolute autonomy) is not fully accommodated by this proportionality framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_measurement_ambiguity,
    'How are ''severity'' and ''transmissibility'' objectively measured and weighted to determine the threshold for legitimate coercion, and who adjudicates these metrics?',
    'Establishment of independent, transparent, and publicly accepted epidemiological and ethical review boards with clear criteria for assessing disease threat and intervention proportionality.',
    'Lack of clear, agreed-upon metrics and adjudication processes can lead to arbitrary application of coercion, increasing extractiveness and suppression, potentially shifting the constraint towards a Snare if the ''proportionality'' becomes a cover for overreach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement_ambiguity, empirical, 'Ambiguity in defining and measuring ''proportionality'' in public health coercion.').

omega_variable(
    internalized_suppression_of_autonomy,
    'To what extent does the repeated application of ''proportional'' coercion lead to an internalized suppression of individual autonomy, even when the immediate threat is low?',
    'Longitudinal sociological and psychological studies on populations exposed to varying levels of public health mandates, assessing changes in perceived autonomy and willingness to comply with non-mandated health behaviors.',
    'If internalized suppression is significant, the effective suppression of the constraint is higher than structural measures suggest, as individuals may self-censor or over-comply even when coercion is not explicitly applied, making the constraint more extractive in practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_of_autonomy, empirical, 'Structural vs. internalized suppression mechanism for individual autonomy.').

omega_variable(
    framing_underdetermination_coercion_legitimacy,
    'Is the ''proportionality_reading'' the most defensible framing, or does an alternative framing (e.g., ''bodily_autonomy_primary'' or ''public_health_primary'') better capture the structural dynamics of coercion legitimacy?',
    'A comprehensive philosophical and legal analysis comparing the coherence, consistency, and practical implications of all three readings across a range of historical and hypothetical public health scenarios, seeking to identify which framing minimizes internal contradictions and maximizes ethical outcomes.',
    'If an alternative framing is adopted, the classification of this constraint would change dramatically. For example, adopting ''bodily_autonomy_primary'' would reclassify any coercion as a Snare, while ''public_health_primary'' might classify it as a Rope or Scaffold, depending on the specific context.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_coercion_legitimacy, conceptual, 'Alternative framings of coercion legitimacy and their impact on classification.').


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
