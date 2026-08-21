% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__proportionality_reading, []).

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
 *   constraint_id: public_health_mandate_authority__proportionality_reading
 *   human_readable: Public Health Mandate Authority (Proportionality Reading)
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality reading' of public health
 *   mandate authority, where the legitimacy of state-imposed health measures
 *   is contingent on a sliding scale of factors: severity of threat,
 *   availability of alternatives, magnitude of coercion, and duration of
 *   imposition. It aims to balance collective public health protection with
 *   individual liberties. The metrics reflect a dynamic constraint, with
 *   extractiveness and suppression rising during periods of high threat
 *   (e.g., a severe pandemic peak) and receding as the threat diminishes or
 *   less restrictive alternatives become viable. The claimed type is Tangled
 *   Rope, as it genuinely seeks to coordinate public health outcomes but
 *   inherently involves asymmetric extraction from individuals, requiring
 *   active enforcement and judicial oversight to maintain its balance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.65).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.75).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Public Health Mandate Authority (Proportionality Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, '0e095b3b-b972-4aa8-9f11-18c23522a602').
narrative_ontology:cs_kernel_codification('0e095b3b-b972-4aa8-9f11-18c23522a602', formalized).
narrative_ontology:cs_authority_grounding('0e095b3b-b972-4aa8-9f11-18c23522a602', lineage).
narrative_ontology:cs_interpretation_layer_present('0e095b3b-b972-4aa8-9f11-18c23522a602').
narrative_ontology:cs_reading_relation('0e095b3b-b972-4aa8-9f11-18c23522a602', public_health_mandate_authority__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('0e095b3b-b972-4aa8-9f11-18c23522a602', public_health_mandate_authority__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('0e095b3b-b972-4aa8-9f11-18c23522a602', foundational, state_power_is_limited_by_individual_rights).
narrative_ontology:cs_axiom_status(state_power_is_limited_by_individual_rights, holdable).
narrative_ontology:cs_axiom_grounding('0e095b3b-b972-4aa8-9f11-18c23522a602', state_power_is_limited_by_individual_rights, deontological).
narrative_ontology:cs_axiom('0e095b3b-b972-4aa8-9f11-18c23522a602', foundational, public_health_measures_must_be_least_restrictive).
narrative_ontology:cs_axiom_status(public_health_measures_must_be_least_restrictive, holdable).
narrative_ontology:cs_axiom_grounding('0e095b3b-b972-4aa8-9f11-18c23522a602', public_health_measures_must_be_least_restrictive, instrumental).
narrative_ontology:cs_reference_frame('0e095b3b-b972-4aa8-9f11-18c23522a602', constitutional_proportionality_doctrine).
narrative_ontology:cs_drift_state('0e095b3b-b972-4aa8-9f11-18c23522a602', contemporary_pandemic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0e095b3b-b972-4aa8-9f11-18c23522a602', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, individuals_subject_to_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting public health, they issue mandates (e.g., vaccination, masking, quarantine) and justify them based on scientific evidence and the severity of the threat. They operate within legal frameworks that require proportionality.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Immunocompromised, elderly, or those with underlying conditions who rely on collective public health measures to reduce their risk of severe illness or death. They benefit from mandates that reduce pathogen transmission.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, local).

% Individuals whose liberties (e.g., bodily autonomy, freedom of movement, access to public spaces) are restricted by public health mandates. They bear the direct costs of compliance or the consequences of non-compliance. Their position varies depending on the perceived proportionality of the mandate.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, individuals_subject_to_mandate, payer,
    moderate, immediate, constrained, local).

% Adjudicate challenges to public health mandates, applying proportionality tests to determine if the state's actions are a legitimate exercise of power, narrowly tailored, and least restrictive. Their rulings shape the interpretation and enforcement of the constraint.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, courts, observer,
    institutional, generational, analytical, national).

% Organizations and individuals who champion individual rights and often challenge public health mandates on grounds of overreach or disproportionality. While they participate in legal and public discourse, their categorical objections are often framed as outside the 'balancing' approach of proportionality.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, civil_liberties_advocates, excluded,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To balance the state's interest in protecting public health with individual constitutional rights, ensuring that public health interventions are proportionate to the threat, minimally coercive, and temporary.
% TRANSFER_FUNCTION: Transfers a variable degree of individual liberty and autonomy to the collective for the sake of public health, while simultaneously imposing a burden on the state to justify these transfers through a proportionality test.
% ABSENT_VOICES: Those who hold a categorical 'bodily autonomy primary' position are often excluded from the core legal and ethical framing of proportionality, which inherently accepts some state power to coerce for public health. They would argue that no collective benefit can justify non-consensual medical intervention.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished, public health mandates would either become unchecked (leading to potential authoritarian overreach) or impossible (leading to unchecked disease spread), fundamentally altering the relationship between state power and individual liberty in health crises.
% FOUNDING_PROBLEM: How to legitimately exercise state power to protect public health in a liberal democracy that values individual rights, avoiding both tyranny and anarchy during health crises.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional law scholars, bioethicists, and historical legal precedents (e.g., Jacobson v. Massachusetts) consistently corroborate the ongoing challenge and necessity of this balancing act, particularly during novel public health emergencies.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(public_health_mandate_authority__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__proportionality_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) and suppression (0.75) are set at a mid-to-high range, reflecting the inherent tension and burden on individuals even when mandates are deemed proportionate. The values are representative of a period where mandates are active but under scrutiny. The temporal measurements show a surge in extractiveness and suppression during a hypothetical peak of a public health crisis (time points 5-10), followed by a slight reduction as the crisis abates, illustrating the 'sliding scale' nature. Theater ratio is low (0.20) because the constraint's function is generally direct and consequential, not performative. Accessibility collapse (0.60) is moderate, as some alternatives to mandates (e.g., remote work, testing) exist but are often constrained, and resistance (0.70) is high due to ongoing public and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities, this constraint is a necessary Rope, enabling collective action for the common good. From the perspective of individuals subject to mandates, it can feel like a Snare, particularly if they perceive the measures as disproportionate or overly coercive. The courts, as observers, attempt to hold the line of proportionality, mediating these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and vulnerable populations are beneficiaries, as the constraint enables collective protection. Individuals subject to mandates are payers, bearing the direct costs of compliance. Courts act as observers, while civil liberties advocates are often excluded from the core framing of the debate, as their position often challenges the premise of state coercion itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is inherently designed to prevent mandatrophy by requiring ongoing proportionality assessments. If the threat diminishes or less restrictive alternatives become available, the mandate's legitimacy (and thus its enforcement and extractiveness) should decrease. Failure to adjust mandates to changing conditions would indicate a drift towards a Snare, where the original coordination function has atrophied but extraction persists due to inertia or rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_proportionality_reading,
    'Is this constraint a genuine balancing act of proportionality, or is it a cover for prioritizing public health over individual rights (public_health_primary) or vice versa (bodily_autonomy_primary)?',
    'Judicial review outcomes and public acceptance of mandates over time, particularly in cases where the threat level is ambiguous or alternatives are readily available.',
    'If it consistently prioritizes public health regardless of proportionality, it drifts towards the ''public_health_primary'' reading (lower extraction, more Rope-like). If it consistently fails to justify any coercion, it collapses towards the ''bodily_autonomy_primary'' reading (higher extraction, more Snare-like for the state).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_proportionality_reading, conceptual, 'This constraint is one reading of the ''public_health_mandate_authority'' kernel, specifically the ''proportionality_reading''.').

omega_variable(
    conflict_with_bodily_autonomy_primary,
    'How would the classification of public health mandates change if the ''bodily_autonomy_primary'' reading were adopted, which categorically rejects non-consensual medical intervention?',
    'A shift in constitutional jurisprudence or widespread public rejection of state health powers. This would render most mandates illegitimate.',
    'Under a ''bodily_autonomy_primary'' reading, any mandate would be reclassified as a Snare, with significantly higher extractiveness and suppression, and the state itself would become a victim of its own overreach, as its authority would be delegitimized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conflict_with_bodily_autonomy_primary, conceptual, 'Structural conflict with the ''bodily_autonomy_primary'' reading.').

omega_variable(
    conflict_with_public_health_primary,
    'How would the classification of public health mandates change if the ''public_health_primary'' reading were adopted, which prioritizes collective health protection above individual liberties?',
    'A shift in constitutional jurisprudence or widespread public acceptance of expansive state health powers. This would reduce the burden of justification on the state.',
    'Under a ''public_health_primary'' reading, mandates would be reclassified as a Rope or Tangled Rope with lower perceived extractiveness, as individual burdens would be seen as necessary contributions to the common good, and the ''individuals_subject_to_mandate'' might shift from victim to beneficiary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conflict_with_public_health_primary, conceptual, 'Structural conflict with the ''public_health_primary'' reading.').

omega_variable(
    threat_severity_contingency,
    'To what extent does the actual extractiveness of the mandate depend on the objective severity and transmissibility of the public health threat?',
    'Epidemiological data and public health risk assessments. A low-severity threat with high extractiveness would indicate disproportionality.',
    'If the threat is objectively low, but mandates remain highly extractive, the constraint''s classification would shift towards Snare, as the coordination function would be minimal relative to the extraction. If the threat is severe, higher extractiveness might be deemed proportionate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threat_severity_contingency, empirical, 'Extractiveness is contingent on the severity of the public health threat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(publ_be_t5, public_health_mandate_authority__proportionality_reading, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(publ_be_t10, public_health_mandate_authority__proportionality_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(publ_be_t15, public_health_mandate_authority__proportionality_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement(publ_be_t20, public_health_mandate_authority__proportionality_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(publ_su_t5, public_health_mandate_authority__proportionality_reading, suppression_requirement, 5, 0.8).
narrative_ontology:measurement(publ_su_t10, public_health_mandate_authority__proportionality_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(publ_su_t15, public_health_mandate_authority__proportionality_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(publ_su_t20, public_health_mandate_authority__proportionality_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
