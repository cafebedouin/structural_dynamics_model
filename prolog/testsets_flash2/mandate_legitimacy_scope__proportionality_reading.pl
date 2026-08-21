% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__proportionality_reading, []).

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
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Public Health Mandate Proportionality Principle
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality reading' of public health
 *   mandate legitimacy. It asserts that mandates are legitimate only when
 *   they are proportional to the threat (disease severity), effective and
 *   safe (vaccine efficacy/safety), and necessary (no less restrictive
 *   alternatives available). This reading acknowledges both public health
 *   goals and individual rights, seeking a balance. The extractiveness and
 *   suppression metrics are moderate, reflecting the inherent tension and the
 *   need for active enforcement balanced by legal challenges and public
 *   discourse.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.45).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.6).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Public Health Mandate Proportionality Principle").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, 'b628b0ec-f7e6-46a6-b213-bdf24f395d3b').
narrative_ontology:cs_kernel_codification('b628b0ec-f7e6-46a6-b213-bdf24f395d3b', formalized).
narrative_ontology:cs_authority_grounding('b628b0ec-f7e6-46a6-b213-bdf24f395d3b', lineage).
narrative_ontology:cs_interpretation_layer_present('b628b0ec-f7e6-46a6-b213-bdf24f395d3b').
narrative_ontology:cs_reading_relation('b628b0ec-f7e6-46a6-b213-bdf24f395d3b', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('b628b0ec-f7e6-46a6-b213-bdf24f395d3b', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('b628b0ec-f7e6-46a6-b213-bdf24f395d3b', foundational, mandate_legitimacy_conditional_on_proportionality).
narrative_ontology:cs_axiom_status(mandate_legitimacy_conditional_on_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('b628b0ec-f7e6-46a6-b213-bdf24f395d3b', mandate_legitimacy_conditional_on_proportionality, deontological).
narrative_ontology:cs_axiom('b628b0ec-f7e6-46a6-b213-bdf24f395d3b', secondary, least_restrictive_means_principle).
narrative_ontology:cs_axiom_status(least_restrictive_means_principle, holdable).
narrative_ontology:cs_axiom_grounding('b628b0ec-f7e6-46a6-b213-bdf24f395d3b', least_restrictive_means_principle, instrumental).
narrative_ontology:cs_reference_frame('b628b0ec-f7e6-46a6-b213-bdf24f395d3b', liberal_democratic_balancing_framework).
narrative_ontology:cs_drift_state('b628b0ec-f7e6-46a6-b213-bdf24f395d3b', contemporary_pandemic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b628b0ec-f7e6-46a6-b213-bdf24f395d3b', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, individuals_subject_to_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting public health, they issue mandates based on scientific evidence of disease severity, vaccine efficacy, and safety. They balance individual liberties against collective well-being, seeking the least restrictive means to achieve public health goals.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Must comply with mandates to access certain services, employment, or education. They bear the direct cost of vaccination (time, minor side effects) and the indirect cost of restricted autonomy. Their exit options are limited by the scope of the mandate.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, individuals_subject_to_mandate, payer,
    moderate, biographical, constrained, local).

% Benefit from reduced disease transmission due to mandates, as they are at higher risk of severe illness or death. They rely on collective action to protect their health, having limited individual means to avoid exposure.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, local).

% Argue that mandates infringe on fundamental bodily autonomy, regardless of public health benefits. They seek to uphold individual rights against state coercion and advocate for voluntary health measures.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, medical_autonomy_advocates, excluded,
    organized, generational, mobile, national).

% Adjudicate challenges to public health mandates, applying proportionality tests that weigh state interests against individual rights. Their rulings shape the legal boundaries of mandate legitimacy.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to mitigate infectious disease spread by ensuring a sufficient level of immunity in the population, protecting both individuals and the broader community, especially the vulnerable.
% TRANSFER_FUNCTION: Transfers a degree of individual medical autonomy from individuals to public health authorities in exchange for collective protection from disease. The 'cost' of this transfer (e.g., vaccine side effects, inconvenience) is borne by the individual, while the 'benefit' (reduced disease risk) is shared collectively.
% ABSENT_VOICES: Individuals who prioritize absolute bodily autonomy over collective health benefits are often marginalized in policy discussions, their concerns framed as anti-social or misinformed, rather than as a legitimate competing value. Their arguments for less restrictive alternatives are often dismissed.
% DISAPPEARANCE_RATIONALE: If the proportionality principle vanished, public health mandates would either become absolute (ignoring individual rights) or disappear entirely (leaving vulnerable populations unprotected). Either extreme would lead to a significant reorganization of public health policy, individual liberties, and societal disease burden.
% FOUNDING_PROBLEM: The challenge of balancing individual liberty with collective health needs during infectious disease outbreaks, particularly when interventions carry individual risks but offer population-level benefits.
% FOUNDING_PROBLEM_CORROBORATION: Public health ethicists, constitutional scholars, and medical associations outside of direct government agencies consistently affirm the ongoing challenge of balancing these competing values, especially in light of new pathogens and evolving scientific understanding.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__proportionality_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__proportionality_reading_tests).
:- end_tests(mandate_legitimacy_scope__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the imposition on individual autonomy, which is significant but mitigated by the proportionality test. Suppression (0.6) is necessary to ensure compliance, but it is not absolute, as legal and ethical challenges are part of the proportionality framework. The 'tangled_rope' classification reflects the genuine coordination function (disease control) coupled with asymmetric extraction (individual autonomy costs). The temporal measurements show fluctuations in extractiveness and suppression, reflecting periods of heightened disease threat (e.g., pandemics) followed by periods of lower threat, where the proportionality test might lead to less restrictive measures.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities perceive this as a necessary and just balancing act, while individuals subject to mandates may still experience it as an infringement, even if 'proportional.' The proportionality framework itself is the site of contestation, with different parties weighing the factors differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda-setters and beneficiaries (they achieve their mission, benefit from a healthier population). Vulnerable populations are clear beneficiaries (protected from severe harm). Individuals subject to mandates are payers (bear the direct costs and autonomy restrictions). Medical autonomy advocates are excluded, as their absolute stance is often outside the proportionality framework's balancing act. Constitutional courts act as observers, adjudicating the application of proportionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_measurement_ambiguity,
    'How are ''disease severity,'' ''vaccine efficacy/safety,'' and ''less restrictive alternatives'' objectively measured and weighted in practice?',
    'Development of standardized, transparent, and publicly debated metrics and weighting schemes for each factor, with independent oversight.',
    'Lack of clear metrics allows for subjective interpretation, potentially leading to mandates that are not truly proportional, increasing effective extraction. Clear metrics would reduce ambiguity and strengthen the legitimacy of mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement_ambiguity, empirical, 'Ambiguity in applying the proportionality test''s criteria.').

omega_variable(
    reading_conflict_location,
    'Where is the core disagreement between the proportionality reading and its siblings located structurally?',
    'Detailed comparative analysis of legal and ethical arguments from each reading, identifying the specific axioms or principles that are prioritized or rejected.',
    'Understanding the structural location of disagreement is crucial for effective policy debate. If the conflict is over foundational axioms, resolution is harder than if it''s over empirical weighting within a shared framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_conflict_location, conceptual, 'Structural location of conflict with sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(mand_be_t10, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(mand_be_t30, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(mand_be_t40, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(mand_be_t50, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(mand_su_t10, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(mand_su_t30, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(mand_su_t40, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(mand_su_t50, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'mandate_legitimacy_scope' kernel, focusing on proportionality. It is linked to 'public_health_primary' and 'bodily_autonomy_primary' as sibling readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
