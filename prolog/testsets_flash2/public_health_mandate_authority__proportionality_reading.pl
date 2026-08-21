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
    narrative_ontology:measurement_basis/2,
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
 *   mandate authority, where the legitimacy and scope of mandates are
 *   determined by a sliding scale of factors: severity of threat,
 *   availability of alternatives, magnitude of coercion, and duration of
 *   imposition. This reading aims to balance collective health with
 *   individual liberties, leading to a dynamic level of extractiveness and
 *   suppression that fluctuates with the perceived threat level. It is a
 *   Tangled Rope because it genuinely coordinates collective action
 *   (protecting public health) but does so through asymmetric extraction
 *   (imposing costs on certain individuals/groups) that requires active
 *   enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.65).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.7).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Public Health Mandate Authority (Proportionality Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, '4c372349-2418-434c-9c91-7be174965b7d').
narrative_ontology:cs_kernel_codification('4c372349-2418-434c-9c91-7be174965b7d', formalized).
narrative_ontology:cs_authority_grounding('4c372349-2418-434c-9c91-7be174965b7d', lineage).
narrative_ontology:cs_interpretation_layer_present('4c372349-2418-434c-9c91-7be174965b7d').
narrative_ontology:cs_reading_relation('4c372349-2418-434c-9c91-7be174965b7d', public_health_mandate_authority__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('4c372349-2418-434c-9c91-7be174965b7d', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('4c372349-2418-434c-9c91-7be174965b7d', foundational, mandates_must_be_least_restrictive).
narrative_ontology:cs_axiom_status(mandates_must_be_least_restrictive, holdable).
narrative_ontology:cs_axiom_grounding('4c372349-2418-434c-9c91-7be174965b7d', mandates_must_be_least_restrictive, deontological).
narrative_ontology:cs_axiom('4c372349-2418-434c-9c91-7be174965b7d', foundational, threat_severity_justifies_coercion_scale).
narrative_ontology:cs_axiom_status(threat_severity_justifies_coercion_scale, holdable).
narrative_ontology:cs_axiom_grounding('4c372349-2418-434c-9c91-7be174965b7d', threat_severity_justifies_coercion_scale, empirically_contingent).
narrative_ontology:cs_reference_frame('4c372349-2418-434c-9c91-7be174965b7d', liberal_democratic_proportionality_framework).
narrative_ontology:cs_drift_state('4c372349-2418-434c-9c91-7be174965b7d', contemporary_pandemic_response, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4c372349-2418-434c-9c91-7be174965b7d', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, immunocompromised_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, unvaccinated_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, businesses_facing_mandates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting population health, they issue mandates based on their assessment of threat severity and available alternatives. They benefit from the ability to implement broad public health interventions but face legal and political challenges.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Highly vulnerable to severe disease, they benefit directly from mandates that reduce pathogen transmission. Their ability to exit high-risk environments is severely limited, making them dependent on collective action for safety.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_individuals, beneficiary,
    powerless, immediate, trapped, local).

% Bear the direct costs of mandates (e.g., inability to access certain venues, employment restrictions). Their exit options are limited to compliance, legal challenge, or social isolation, depending on the mandate's scope and severity.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, unvaccinated_individuals, payer,
    moderate, biographical, constrained, local).

% Incur costs related to enforcing mandates (e.g., checking vaccination status, managing non-compliant employees) and potential loss of customers. Their options are compliance, legal challenge, or risking fines/closure.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, businesses_facing_mandates, payer,
    organized, immediate, constrained, local).

% Analyze mandates for proportionality, balancing public health goals against individual rights. They challenge mandates that they deem overly coercive or lacking sufficient justification based on the sliding scale criteria.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to mitigate public health threats by imposing measures proportional to the risk, aiming to protect both individual liberties and population-level health outcomes.
% TRANSFER_FUNCTION: Transfers certain individual liberties (e.g., freedom of movement, choice of medical intervention) from individuals to the collective, in exchange for reduced public health risk and protection of vulnerable populations. The magnitude of this transfer is scaled by the threat.
% ABSENT_VOICES: Those who are disproportionately affected by mandates due to socioeconomic status, lack of access to healthcare, or cultural barriers, whose specific burdens may not be fully accounted for in a generalized proportionality assessment.
% DISAPPEARANCE_RATIONALE: If the authority to issue public health mandates based on proportionality vanished, public health responses to epidemics would become fragmented and ineffective, leading to higher morbidity and mortality, especially among vulnerable groups. Society would struggle to coordinate collective action against health threats.
% FOUNDING_PROBLEM: The need to balance individual rights with collective well-being during public health crises, preventing both unchecked state power and uncontrolled spread of disease.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, bioethicists, and public health experts widely corroborate the ongoing challenge of balancing individual liberties and public health, especially in the context of novel pathogens and evolving scientific understanding. This corroboration comes from outside the direct beneficiaries of mandates.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.65) and suppression (0.70) are substantial because mandates, even when proportional, impose real costs and restrict freedoms, requiring active enforcement. However, these values are not maximal, reflecting the 'sliding scale' nature where mandates are theoretically adjusted to minimize unnecessary burden. The 'resistance' is high (0.75) because the proportionality of mandates is frequently contested by affected parties and civil liberties advocates. The 'accessibility_collapse' is moderate (0.40) as alternatives (e.g., voluntary measures, less restrictive interventions) are considered, but often deemed insufficient by authorities. The temporal measurements show fluctuations, reflecting how the constraint's severity (and thus extractiveness/suppression) changes with the perceived public health threat over time.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities view this as a necessary and carefully calibrated tool for societal protection, while those subject to mandates often perceive it as an overreach, even if theoretically proportional. The engine's classification will reflect the objective extractiveness and suppression, which may diverge from the 'claimed rope' framing by authorities.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and immunocompromised individuals are beneficiaries, as the constraint enables collective protection. Unvaccinated individuals and businesses facing mandates are payers, bearing the direct costs and restrictions. Civil liberties advocates act as observers, scrutinizing the proportionality. The dynamic nature of the constraint means that the victim set can expand or contract depending on the threat level and the specific mandate imposed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is designed to prevent mandatrophy by explicitly linking its legitimacy to ongoing proportionality. If the founding problem (balancing rights and health) were to become 'dead' or the proportionality assessment became theatrical, the constraint would shift towards a Snare or Piton. The high resistance and active debate around proportionality serve as a check against its mandate atrophying into pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_measurement_ambiguity,
    'How is ''proportionality'' objectively measured and agreed upon across diverse stakeholders, given differing risk tolerances and values?',
    'Development of standardized, transparent, and independently verifiable metrics for threat severity, alternative efficacy, and coercive impact, coupled with deliberative processes for stakeholder input.',
    'If proportionality metrics remain subjective, the constraint''s application will be perceived as arbitrary, increasing resistance and potentially shifting its effective classification towards a Snare due to perceived unfairness. If objective metrics are established, it could strengthen its Rope-like coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement_ambiguity, conceptual, 'Ambiguity in the objective measurement and societal agreement on ''proportionality'' criteria.').

omega_variable(
    threat_level_escalation_bias,
    'Is there an inherent bias in public health authorities to over-estimate threat severity or under-estimate the efficacy of less coercive alternatives, leading to an ''escalation ratchet'' in mandates?',
    'Independent, ex-post audits of public health decision-making during crises, comparing initial threat assessments and chosen interventions against actual outcomes and the performance of less restrictive alternatives.',
    'If such a bias is confirmed, the constraint''s effective extractiveness and suppression would be systematically higher than justified by objective proportionality, pushing it closer to a Snare. If no bias is found, the current classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threat_level_escalation_bias, empirical, 'Potential for bias in public health authorities'' assessment of threat and alternatives.').

omega_variable(
    structural_vs_internalized_suppression,
    'Is the measured suppression primarily structural (legal penalties, access restrictions) or internalized (social pressure, fear of ostracization) for individuals subject to mandates?',
    'Post-mandate psychological and sociological studies: if compliance persists or self-restriction continues after legal mandates are lifted, it indicates a significant internalized component.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them even after formal enforcement lessens. This would amplify the perceived extractiveness for affected individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Structural vs. internalized suppression mechanism for public health mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__proportionality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(publ_tr_t5, public_health_mandate_authority__proportionality_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement_basis(publ_tr_t5, observed).
narrative_ontology:measurement(publ_tr_t10, public_health_mandate_authority__proportionality_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(publ_tr_t10, observed).
narrative_ontology:measurement(publ_tr_t15, public_health_mandate_authority__proportionality_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement_basis(publ_tr_t15, observed).
narrative_ontology:measurement(publ_tr_t20, public_health_mandate_authority__proportionality_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(publ_be_t5, public_health_mandate_authority__proportionality_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(publ_be_t5, observed).
narrative_ontology:measurement(publ_be_t10, public_health_mandate_authority__proportionality_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement_basis(publ_be_t10, observed).
narrative_ontology:measurement(publ_be_t15, public_health_mandate_authority__proportionality_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(publ_be_t15, observed).
narrative_ontology:measurement(publ_be_t20, public_health_mandate_authority__proportionality_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(publ_su_t5, public_health_mandate_authority__proportionality_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement_basis(publ_su_t5, observed).
narrative_ontology:measurement(publ_su_t10, public_health_mandate_authority__proportionality_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement_basis(publ_su_t10, observed).
narrative_ontology:measurement(publ_su_t15, public_health_mandate_authority__proportionality_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement_basis(publ_su_t15, observed).
narrative_ontology:measurement(publ_su_t20, public_health_mandate_authority__proportionality_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'public_health_mandate_authority' kernel, focusing on proportionality. It is structurally distinct from the 'public_health_primary' and 'bodily_autonomy_primary' readings, which emphasize different foundational principles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
