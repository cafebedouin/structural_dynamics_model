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
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Proportionality Principle in Public Health Coercion
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality_reading' of the
 *   'coercion_legitimacy_boundary' kernel, which posits that the legitimacy
 *   of public health coercion scales with the severity and transmission
 *   dynamics of a disease. For instance, mandates for highly contagious and
 *   severe diseases like measles are justified, while those for less severe,
 *   less transmissible diseases like seasonal flu are not. This reading
 *   attempts to balance individual autonomy with collective public health
 *   needs, leading to a case-by-case adjudication of coercive measures. The
 *   constraint is classified as a Tangled Rope because it serves a genuine
 *   coordination function (public health protection) but involves asymmetric
 *   extraction (from individuals whose autonomy is overridden) and requires
 *   active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.45).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.55).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Proportionality Principle in Public Health Coercion").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, 'e08d57d5-ee2f-40e9-9470-cfd2a71d04c1').
narrative_ontology:cs_kernel_codification('e08d57d5-ee2f-40e9-9470-cfd2a71d04c1', formalized).
narrative_ontology:cs_authority_grounding('e08d57d5-ee2f-40e9-9470-cfd2a71d04c1', lineage).
narrative_ontology:cs_interpretation_layer_present('e08d57d5-ee2f-40e9-9470-cfd2a71d04c1').
narrative_ontology:cs_reading_relation('e08d57d5-ee2f-40e9-9470-cfd2a71d04c1', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('e08d57d5-ee2f-40e9-9470-cfd2a71d04c1', coercion_legitimacy_boundary__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('e08d57d5-ee2f-40e9-9470-cfd2a71d04c1', foundational, state_coercion_requires_justification).
narrative_ontology:cs_axiom_status(state_coercion_requires_justification, holdable).
narrative_ontology:cs_axiom_grounding('e08d57d5-ee2f-40e9-9470-cfd2a71d04c1', state_coercion_requires_justification, deontological).
narrative_ontology:cs_axiom('e08d57d5-ee2f-40e9-9470-cfd2a71d04c1', foundational, coercion_must_be_least_restrictive).
narrative_ontology:cs_axiom_status(coercion_must_be_least_restrictive, holdable).
narrative_ontology:cs_axiom_grounding('e08d57d5-ee2f-40e9-9470-cfd2a71d04c1', coercion_must_be_least_restrictive, instrumental).
narrative_ontology:cs_reference_frame('e08d57d5-ee2f-40e9-9470-cfd2a71d04c1', harm_principle_adjudication).
narrative_ontology:cs_drift_state('e08d57d5-ee2f-40e9-9470-cfd2a71d04c1', contemporary_pandemic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e08d57d5-ee2f-40e9-9470-cfd2a71d04c1', '').
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

% Responsible for implementing and enforcing public health measures. They operate under legal and ethical frameworks that require them to justify coercive actions based on proportionality to disease threat.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Bear the direct costs of public health coercion (e.g., mandatory vaccination, quarantine, mask mandates) when a disease meets the severity and transmissibility thresholds defined by the proportionality principle. Their autonomy is temporarily overridden.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, individuals_subject_to_mandates, payer,
    powerless, immediate, trapped, local).

% Benefit from reduced disease transmission and severity due to public health mandates. They are often at higher risk of severe outcomes from infectious diseases and rely on collective action for protection.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, constrained, local).

% Monitor and challenge public health mandates, ensuring that the proportionality principle is rigorously applied and that individual rights are protected against overreach. They often represent individuals subject to mandates.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% Advise public health authorities and healthcare systems on the ethical implications of interventions, often applying the proportionality principle to assess the justification and scope of coercive measures.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, medical_ethics_boards, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__proportionality_reading, diffuse).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate collective action to mitigate public health threats by establishing a framework for legitimate state intervention, balancing individual liberties with the collective need for protection based on disease severity and transmission dynamics.
% TRANSFER_FUNCTION: Transfers a degree of individual autonomy (e.g., choice over medical procedures, freedom of movement) to the state for the benefit of collective public health, particularly for vulnerable populations, when disease severity and transmissibility warrant such measures.
% ABSENT_VOICES: Individuals disproportionately affected by mandates due to socioeconomic status, lack of access to resources, or historical medical mistrust, whose specific burdens may be overlooked in a generalized proportionality assessment. Also, those who categorically reject any state medical intervention.
% DISAPPEARANCE_RATIONALE: If the proportionality principle vanished, public health authorities would either implement overly coercive measures without justification (leading to rights violations) or be unable to implement necessary interventions (leading to uncontrolled epidemics). The balance between individual rights and public good would collapse, reorganizing legal and ethical frameworks.
% FOUNDING_PROBLEM: How to legitimately balance individual liberty with the collective need for public health protection in the face of infectious diseases, particularly after historical instances of state overreach and medical abuses.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, public health ethicists, and constitutional law experts widely acknowledge this as a persistent and actively debated tension in democratic societies, with ongoing case law and policy discussions reflecting its live status.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.45) and suppression (0.55) are moderate, reflecting the inherent tension and the fact that this reading allows for significant state power in certain circumstances, but also imposes limits. The values are not extreme because the principle itself aims for balance, preventing both categorical rejection of coercion and unlimited state power. Theater ratio is low (0.15) as the principle is actively debated and applied, not merely performed. Accessibility collapse is moderate (0.5) because alternatives to mandates are collapsed for high-threat diseases but remain for low-threat ones. Resistance is moderate (0.4) due to ongoing debates and challenges to specific applications of the principle.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities, this constraint is a necessary framework for effective governance and protection. From the perspective of individuals subject to mandates, it represents a potential (and sometimes actual) infringement on their autonomy. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing a more 'rope-like' outcome and victims a more 'snare-like' one, even within the same 'tangled_rope' structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and vulnerable populations are beneficiaries, as the constraint provides a legitimate framework for protecting collective health. Individuals subject to mandates are victims, as their autonomy is directly curtailed. Civil liberties advocates and medical ethics boards act as observers, influencing the application and interpretation of the principle. The directionality for individuals subject to mandates is high (near 1.0) when mandates are active, reflecting direct extraction of autonomy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_threshold_ambiguity,
    'What specific thresholds for disease severity and transmissibility legitimately trigger coercive public health measures under the proportionality principle?',
    'Consensus among epidemiologists, ethicists, and legal scholars on a standardized risk assessment framework, or landmark court rulings establishing clear precedents.',
    'Clearer thresholds would reduce contestation and potentially lower resistance, making the constraint''s application more consistent. Ambiguity allows for arbitrary application or political manipulation, increasing effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_ambiguity, conceptual, 'Ambiguity in defining the ''proportional'' threshold for coercion.').

omega_variable(
    bodily_autonomy_vs_proportionality,
    'How would the constraint''s structure change if the ''bodily_autonomy_primary'' reading were adopted, which holds that medical intervention without consent is categorically impermissible?',
    'Analysis of legal systems that enshrine absolute bodily autonomy, or a counterfactual scenario where all public health mandates are legally struck down.',
    'If ''bodily_autonomy_primary'' were adopted, the victim set (''individuals_subject_to_mandates'') would disappear, and the constraint would cease to exist as a mechanism for coercion. Public health authorities would lose a key tool, and the ''public_health_primary'' reading would be foreclosed. The extractiveness of this constraint would drop to 0, as no coercion would be legitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bodily_autonomy_vs_proportionality, conceptual, 'Structural impact of an absolute bodily autonomy principle.').

omega_variable(
    public_health_vs_proportionality,
    'How would the constraint''s structure change if the ''public_health_primary'' reading were adopted, which prioritizes collective harm-prevention above individual autonomy?',
    'Analysis of legal systems that grant broad powers to public health authorities, or a counterfactual scenario where proportionality tests are removed from public health law.',
    'If ''public_health_primary'' were adopted, the ''individuals_subject_to_mandates'' would face higher and more frequent extraction, as the proportionality limits would be weakened or removed. The extractiveness and suppression of the constraint would likely increase, and resistance might also rise as more individuals perceive it as a snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_health_vs_proportionality, conceptual, 'Structural impact of a public health primary principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t1990, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(coer_tr_t1996, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 1996, 0.11).
narrative_ontology:measurement(coer_tr_t2002, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 2002, 0.12).
narrative_ontology:measurement(coer_tr_t2008, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 2008, 0.13).
narrative_ontology:measurement(coer_tr_t2014, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 2014, 0.14).
narrative_ontology:measurement(coer_tr_t2020, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(coer_be_t1990, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(coer_be_t1996, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 1996, 0.41).
narrative_ontology:measurement(coer_be_t2002, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 2002, 0.42).
narrative_ontology:measurement(coer_be_t2008, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 2008, 0.43).
narrative_ontology:measurement(coer_be_t2014, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 2014, 0.44).
narrative_ontology:measurement(coer_be_t2020, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t1990, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(coer_su_t1996, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 1996, 0.51).
narrative_ontology:measurement(coer_su_t2002, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 2002, 0.52).
narrative_ontology:measurement(coer_su_t2008, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 2008, 0.53).
narrative_ontology:measurement(coer_su_t2014, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 2014, 0.54).
narrative_ontology:measurement(coer_su_t2020, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 2020, 0.55).


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
