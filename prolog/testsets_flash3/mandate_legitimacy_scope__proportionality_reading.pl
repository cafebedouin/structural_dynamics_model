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
    narrative_ontology:affects_constraint/2,
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
 *   proportional to disease severity, vaccine safety/efficacy, and the
 *   availability of less restrictive alternatives. This reading acknowledges
 *   both collective benefit and individual rights, seeking a balance. Its
 *   extractiveness and suppression fluctuate with perceived public health
 *   threats and societal tolerance for restrictions, as seen in the COVID-19
 *   pandemic era (2020-2024).
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
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, 'd5df7683-a4ae-454e-a358-4203683be9ec').
narrative_ontology:cs_kernel_codification('d5df7683-a4ae-454e-a358-4203683be9ec', formalized).
narrative_ontology:cs_authority_grounding('d5df7683-a4ae-454e-a358-4203683be9ec', lineage).
narrative_ontology:cs_interpretation_layer_present('d5df7683-a4ae-454e-a358-4203683be9ec').
narrative_ontology:cs_reading_relation('d5df7683-a4ae-454e-a358-4203683be9ec', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('d5df7683-a4ae-454e-a358-4203683be9ec', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('d5df7683-a4ae-454e-a358-4203683be9ec', foundational, mandates_must_be_least_restrictive).
narrative_ontology:cs_axiom_status(mandates_must_be_least_restrictive, holdable).
narrative_ontology:cs_axiom_grounding('d5df7683-a4ae-454e-a358-4203683be9ec', mandates_must_be_least_restrictive, deontological).
narrative_ontology:cs_axiom('d5df7683-a4ae-454e-a358-4203683be9ec', foundational, collective_good_justifies_limited_infringement).
narrative_ontology:cs_axiom_status(collective_good_justifies_limited_infringement, holdable).
narrative_ontology:cs_axiom_grounding('d5df7683-a4ae-454e-a358-4203683be9ec', collective_good_justifies_limited_infringement, instrumental).
narrative_ontology:cs_reference_frame('d5df7683-a4ae-454e-a358-4203683be9ec', constitutional_proportionality_doctrine).
narrative_ontology:cs_drift_state('d5df7683-a4ae-454e-a358-4203683be9ec', post_covid19_pandemic, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d5df7683-a4ae-454e-a358-4203683be9ec', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, individuals_subject_to_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting public health, they issue mandates based on scientific evidence of disease severity, vaccine efficacy, and public health risk. They balance individual liberties against collective well-being, seeking the least restrictive means.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Must comply with mandates to access certain services, employment, or education. They bear the direct cost of vaccination (time, minor side effects) and the indirect cost of restricted autonomy. Their exit options are limited by the scope of the mandate.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, individuals_subject_to_mandate, payer,
    moderate, biographical, constrained, local).

% Benefit from reduced disease transmission due to mandates, especially those who cannot be vaccinated or are immunocompromised. They rely on herd immunity for protection and have few alternatives if mandates are absent.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, local).

% Adjudicate challenges to public health mandates, applying proportionality tests that weigh state interest against individual rights. Their rulings shape the legal boundaries of mandate legitimacy.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Argue for the primacy of individual bodily autonomy and informed consent, often challenging the proportionality of mandates even for severe diseases. They are often excluded from the initial policy-making process but engage in litigation and public discourse.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, medical_autonomy_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to mitigate public health crises by ensuring sufficient vaccination rates, thereby protecting both vaccinated and unvaccinated vulnerable individuals from infectious diseases.
% TRANSFER_FUNCTION: Transfers a degree of individual medical autonomy from individuals to the state (via public health authorities) in exchange for collective protection from disease.
% ABSENT_VOICES: Those who prioritize absolute bodily autonomy are often marginalized in policy discussions, arguing that even proportional mandates infringe on fundamental rights. Their perspective is often heard only in legal challenges.
% DISAPPEARANCE_RATIONALE: If the proportionality principle for mandates vanished, public health authorities would either impose mandates indiscriminately (leading to widespread resistance) or abandon them entirely (leading to increased disease burden, especially for vulnerable populations). The balance between individual rights and collective health would be severely disrupted.
% FOUNDING_PROBLEM: The challenge of balancing individual liberty with collective health during infectious disease outbreaks, particularly when interventions carry risks or infringe on personal choice.
% FOUNDING_PROBLEM_CORROBORATION: Public health experts, ethicists, and legal scholars widely corroborate that this problem remains live, as new pathogens emerge and societal values regarding autonomy evolve. Constitutional courts continually revisit this balance in their rulings.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates collective health (beneficiaries: vulnerable populations, public health authorities) but also involves asymmetric extraction (victims: individuals subject to mandates) and requires active enforcement. Extractiveness is moderate (0.45) as it balances benefits and costs, but can rise during crises. Suppression (0.6) is necessary to ensure compliance, but is moderated by the proportionality principle itself. Theater ratio is low (0.1) as the justification for mandates is generally genuine, though it can increase if mandates are perceived as disproportionate.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities and vulnerable populations would experience this as a necessary Rope, ensuring collective safety. Individuals subject to mandates, however, would experience it as a Tangled Rope, feeling the burden of compliance and the restriction of their autonomy, even if they acknowledge the collective good. Constitutional courts act as an observer, attempting to reconcile these perspectives through legal tests.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda-setters and beneficiaries, as they implement policies for collective good. Vulnerable populations are clear beneficiaries, relying on the constraint for protection. Individuals subject to mandates are payers, bearing the direct costs and autonomy restrictions. Medical autonomy advocates are excluded, as their primary concern (absolute autonomy) is not fully accommodated by this balancing act.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is designed to prevent mandatrophy by requiring ongoing justification based on proportionality. If a mandate outlives its necessity (e.g., for a mild disease with safe alternatives), this reading would deem it illegitimate, preventing it from becoming a Piton or Snare. The 'contested' status of the founding problem reflects the ongoing societal debate about this balance, which is central to preventing mandate creep.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_measurement_ambiguity,
    'How are ''disease severity,'' ''vaccine safety/efficacy,'' and ''less restrictive alternatives'' objectively measured and weighted in practice?',
    'Development of standardized, transparent, and publicly debated metrics and weighting schemes for proportionality assessments, ideally with independent oversight.',
    'Lack of clear metrics allows for subjective interpretation, potentially leading to mandates that are perceived as disproportionate, increasing resistance and effective extraction. Clear metrics would enhance legitimacy and reduce perceived extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement_ambiguity, empirical, 'Ambiguity in applying the proportionality principle.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''proportionality reading'' or does it lean too heavily towards either ''public_health_primary'' or ''bodily_autonomy_primary'' in practice?',
    'Analysis of judicial rulings and public health policy documents over time, specifically examining how the three criteria (severity, safety/efficacy, alternatives) are weighted and applied in specific cases.',
    'If it consistently defaults to one extreme, it should be reclassified as a variant of that sibling reading, indicating that the ''proportionality'' claim is rhetorical cover for a different underlying principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishing this reading from its siblings in practice.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, access restrictions) or internalized (social pressure, fear of ostracization)?',
    'Post-mandate compliance trajectory: if compliance persists after legal enforcement is removed, reclassify as partially internalized. Surveys on reasons for compliance.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — individuals carry the suppression with them after formal enforcement ends, making exit harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for mandate compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t1900, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(mand_tr_t1950, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(mand_tr_t2000, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(mand_tr_t2020, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(mand_tr_t2024, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(mand_be_t1900, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(mand_be_t1950, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(mand_be_t2000, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(mand_be_t2020, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(mand_be_t2024, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t1900, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(mand_su_t1950, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(mand_su_t2000, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(mand_su_t2020, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(mand_su_t2024, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'mandate_legitimacy_scope' kernel. This 'proportionality_reading' attempts to balance collective health and individual autonomy, influencing and coexisting with the 'public_health_primary' and 'bodily_autonomy_primary' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
