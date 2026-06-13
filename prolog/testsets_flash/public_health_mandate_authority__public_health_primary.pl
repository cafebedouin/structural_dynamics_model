% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__public_health_primary, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: public_health_mandate_authority__public_health_primary
 *   human_readable: Public Health Mandate (Public Health Primary Reading)
 *   domain: public_health/law/bioethics
 *
 * SUMMARY:
 *   This constraint represents the 'public health primary' reading of public
 *   health mandate authority, where the obligation to protect the vulnerable
 *   commons (immunocompromised, healthcare infrastructure) via collective
 *   action is paramount. In this reading, the immunocompromised enter the
 *   victim set when mandates fail, and unvaccinated individuals are largely
 *   excluded from the victim set, framed instead as free-riders imposing
 *   externalities. The constraint is classified as a Tangled Rope due to its
 *   genuine coordination function (protecting public health) coupled with
 *   significant asymmetric extraction from those who resist mandates,
 *   enforced through social and legal coercion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.7).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.8).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.7).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public Health Mandate (Public Health Primary Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public_health/law/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, 'd9be3325-f924-47b1-8a17-65d454d239e1').
narrative_ontology:cs_kernel_codification('d9be3325-f924-47b1-8a17-65d454d239e1', formalized).
narrative_ontology:cs_authority_grounding('d9be3325-f924-47b1-8a17-65d454d239e1', lineage).
narrative_ontology:cs_interpretation_layer_present('d9be3325-f924-47b1-8a17-65d454d239e1').
narrative_ontology:cs_reading_relation('d9be3325-f924-47b1-8a17-65d454d239e1', public_health_mandate_authority__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('d9be3325-f924-47b1-8a17-65d454d239e1', public_health_mandate_authority__proportionality_reading, influences).
narrative_ontology:cs_axiom('d9be3325-f924-47b1-8a17-65d454d239e1', foundational, collective_health_supremacy).
narrative_ontology:cs_axiom_status(collective_health_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('d9be3325-f924-47b1-8a17-65d454d239e1', collective_health_supremacy, deontological).
narrative_ontology:cs_axiom('d9be3325-f924-47b1-8a17-65d454d239e1', foundational, vulnerable_commons_protection_obligation).
narrative_ontology:cs_axiom_status(vulnerable_commons_protection_obligation, holdable).
narrative_ontology:cs_axiom_grounding('d9be3325-f924-47b1-8a17-65d454d239e1', vulnerable_commons_protection_obligation, deontological).
narrative_ontology:cs_reference_frame('d9be3325-f924-47b1-8a17-65d454d239e1', public_health_emergency_powers_doctrine).
narrative_ontology:cs_drift_state('d9be3325-f924-47b1-8a17-65d454d239e1', contemporary_rights_discourse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d9be3325-f924-47b1-8a17-65d454d239e1', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, healthcare_system).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, general_public).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, businesses_with_mandate_exemptions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for issuing and enforcing public health mandates, such as vaccination or mask requirements, to protect the collective health. They frame these actions as necessary obligations to safeguard vulnerable populations and healthcare capacity.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Directly benefit from collective adherence to public health mandates, as their health and lives are disproportionately at risk from infectious diseases. Without mandates, their ability to participate in public life is severely curtailed.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, immediate, trapped, local).

% Benefits from mandates by preventing overwhelming surges in patient numbers, preserving resources, and protecting healthcare workers. Mandates reduce the strain on infrastructure and personnel.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, healthcare_system, beneficiary,
    institutional, biographical, constrained, national).

% Benefits from reduced disease transmission, leading to fewer illnesses, hospitalizations, and deaths. They experience a safer environment for daily activities and reduced economic disruption.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, general_public, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct costs of mandates, such as forced vaccination, mask-wearing, or exclusion from certain public spaces or employment. They perceive these as infringements on personal liberty and bodily autonomy.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals, payer,
    moderate, immediate, constrained, local).

% Face operational challenges and potential financial losses due to implementing and enforcing mandates, or from losing customers who refuse to comply. They may also bear costs from legal challenges.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, businesses_with_mandate_exemptions, payer,
    moderate, immediate, constrained, local).

% Analyze the legality and constitutional implications of public health mandates, often representing parties on both sides of the issue. Their work shapes judicial interpretations of public health authority versus individual rights.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, constitutional_lawyers, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to mitigate the spread of infectious diseases, ensuring a baseline level of public health protection that individual actions alone cannot achieve, especially for vulnerable populations.
% TRANSFER_FUNCTION: Transfers a portion of individual liberty and autonomy (e.g., choice over medical interventions, freedom of movement) from mandate-resistant individuals to the collective good of public health and safety for all, particularly the vulnerable.
% ABSENT_VOICES: The voices of those who prioritize individual bodily autonomy above all collective health considerations are often marginalized in this reading, framed as free-riders imposing externalities rather than legitimate rights-holders. They are present in public discourse but excluded from the core justification of the mandate.
% DISAPPEARANCE_RATIONALE: If public health mandates and their enforcement vanished overnight, the world would rearrange significantly. Vulnerable populations would face immediate, heightened risks, healthcare systems would be more prone to collapse during outbreaks, and society would lose a key mechanism for collective defense against communicable diseases. The social contract around collective health would be fundamentally altered.
% FOUNDING_PROBLEM: The problem of infectious disease spread, where individual actions (or inactions) have collective consequences, leading to widespread illness, death, and overwhelming of public resources, particularly impacting those unable to protect themselves.
% FOUNDING_PROBLEM_CORROBORATION: Medical professionals, epidemiologists, and public health historians consistently corroborate that the founding problem of infectious disease spread and its collective impact remains live. Data on past pandemics and ongoing endemic diseases, along with the vulnerability of specific populations, supports this view from outside the immediate beneficiaries of mandates.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(public_health_mandate_authority__public_health_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because mandates impose significant costs on mandate-resistant individuals, including potential loss of employment or access to services. Suppression is also high (0.8) as the constraint relies on active enforcement and social pressure to ensure compliance, with limited exit options for those who dissent. The theater ratio is low (0.1) because the mandate's function is largely direct and not performative; its purpose is to achieve a tangible public health outcome. Accessibility collapse is moderate (0.6) as alternatives to mandates (e.g., individual precautions) exist but are often insufficient to achieve the collective protection sought. Resistance is high (0.75) reflecting the significant public and legal opposition mandates often face.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities and beneficiaries (immunocompromised, healthcare system), the mandate is a necessary, legitimate coordination mechanism. From the perspective of mandate-resistant individuals, it is a coercive, extractive imposition on their autonomy. The engine's per-seat classification will reflect this divergence based on the declared roles, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda-setters and beneficiaries, as they implement and benefit from the system's stability. Immunocompromised individuals, the healthcare system, and the general public are clear beneficiaries. Mandate-resistant individuals and businesses with mandate exemptions are payers, bearing the direct costs and restrictions. Unvaccinated individuals, while not explicitly listed as victims in this reading, are implicitly the target of the mandate's coercive force, framed as agents whose actions impose costs on others.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling coordination as pure extraction by emphasizing the genuine collective action problem mandates address (protecting the vulnerable commons). However, the high extractiveness and suppression indicate that the coordination comes at a significant cost to a specific group, making it a Tangled Rope rather than a pure Rope. The mandate's function is live, but its implementation generates substantial friction and resistance, indicating a tension between its coordination goal and its extractive means.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_vs_individual_rights_priority,
    'Is the prioritization of collective public health over individual bodily autonomy a necessary structural feature of effective public health mandates, or a policy choice that could be rebalanced?',
    'Comparative legal analysis of different constitutional frameworks and their outcomes during public health crises, or philosophical debate on the limits of state power in health emergencies.',
    'If rebalanced, the extractiveness on mandate-resistant individuals could decrease, potentially shifting the constraint towards a Rope or even a Scaffold if temporary. If necessary, the Tangled Rope classification is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_vs_individual_rights_priority, conceptual, 'Ambiguity in the fundamental trade-off between collective good and individual rights in public health.').

omega_variable(
    mandate_efficacy_vs_resistance,
    'At what point does the resistance generated by high extractiveness and suppression undermine the overall efficacy of the public health mandate, making it counterproductive?',
    'Empirical studies correlating mandate stringency and enforcement with public compliance rates, social cohesion, and long-term public health outcomes, accounting for resistance.',
    'If resistance significantly reduces efficacy, the mandate''s coordination function is compromised, potentially increasing its theater ratio and pushing it towards a Snare or Piton if its primary effect becomes social division rather than health protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_efficacy_vs_resistance, empirical, 'The point at which mandate resistance negates public health benefits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(publ_tr_t5, public_health_mandate_authority__public_health_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(publ_tr_t10, public_health_mandate_authority__public_health_primary, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(publ_be_t5, public_health_mandate_authority__public_health_primary, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(publ_be_t10, public_health_mandate_authority__public_health_primary, base_extractiveness, 10, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(publ_su_t5, public_health_mandate_authority__public_health_primary, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(publ_su_t10, public_health_mandate_authority__public_health_primary, suppression_requirement, 10, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
