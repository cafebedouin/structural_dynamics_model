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
 *   mandate authority, where the legitimacy of mandates depends on a sliding
 *   scale of factors: severity of threat, availability of alternatives,
 *   magnitude of coercion, and duration of imposition. It aims to balance
 *   collective public health goals with individual rights. The constraint is
 *   dynamic, with its extractiveness and suppressive force varying based on
 *   the perceived threat level and the specific context of its application.
 *   The metrics reflect a scenario of moderate-to-high public health threat
 *   where mandates are actively enforced.
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
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, '3dad2f14-f924-4418-9b9b-c1a43100235f').
narrative_ontology:cs_kernel_codification('3dad2f14-f924-4418-9b9b-c1a43100235f', formalized).
narrative_ontology:cs_authority_grounding('3dad2f14-f924-4418-9b9b-c1a43100235f', lineage).
narrative_ontology:cs_interpretation_layer_present('3dad2f14-f924-4418-9b9b-c1a43100235f').
narrative_ontology:cs_reading_relation('3dad2f14-f924-4418-9b9b-c1a43100235f', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('3dad2f14-f924-4418-9b9b-c1a43100235f', public_health_mandate_authority__public_health_primary, influences).
narrative_ontology:cs_axiom('3dad2f14-f924-4418-9b9b-c1a43100235f', foundational, proportionality_of_intervention).
narrative_ontology:cs_axiom_status(proportionality_of_intervention, holdable).
narrative_ontology:cs_axiom_grounding('3dad2f14-f924-4418-9b9b-c1a43100235f', proportionality_of_intervention, deontological).
narrative_ontology:cs_reference_frame('3dad2f14-f924-4418-9b9b-c1a43100235f', balanced_public_interest).
narrative_ontology:cs_drift_state('3dad2f14-f924-4418-9b9b-c1a43100235f', contemporary_public_health_crises, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3dad2f14-f924-4418-9b9b-c1a43100235f', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, mandate_targets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for assessing public health threats, designing interventions, and enforcing mandates. They interpret and apply the proportionality principle to justify their actions, aiming to balance collective good with individual rights.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Directly benefit from public health mandates that reduce disease transmission, as they are at higher risk of severe illness or death. Their health status often limits their ability to avoid exposure, making collective protection crucial.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Individuals or groups subject to public health mandates (e.g., masking, vaccination, quarantine). They bear the direct costs of compliance, including restrictions on liberty, economic impact, or medical procedures. Their ability to exit is limited by legal enforcement and social pressure.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, mandate_targets, payer,
    moderate, biographical, constrained, local).

% Monitor public health mandates for potential overreach and advocate for individual rights. They scrutinize the application of proportionality, often challenging mandates in court or through public discourse.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, civil_liberties_advocates, observer,
    organized, biographical, analytical, national).

% Adjudicate challenges to public health mandates, interpreting constitutional rights and statutory powers through the lens of proportionality. Their rulings shape the boundaries of legitimate public health authority.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Advocate for a categorical right to bodily integrity, viewing any non-consensual medical intervention or restriction as a violation. Their arguments often fall outside the balancing framework of proportionality, leading to their exclusion from direct policy-making within this reading.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, bodily_autonomy_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To balance the collective imperative of public health protection with the individual rights and liberties of citizens, ensuring that public health interventions are proportionate to the threat and are the least restrictive means available.
% TRANSFER_FUNCTION: Transfers some degree of individual liberty and autonomy from mandate targets to the collective for the benefit of public health, particularly for vulnerable populations. It also transfers the burden of enforcement and justification to public health authorities and the courts.
% ABSENT_VOICES: Those who advocate for an absolute or primary right to bodily autonomy, as their categorical stance is often not fully integrated into the balancing framework of proportionality, which inherently seeks compromise rather than absolute rights.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished, public health mandates would either become overly coercive (if public health primary dominates) or largely ineffective (if bodily autonomy primary dominates), leading to a breakdown in public trust, increased legal challenges, and potentially worse public health outcomes, forcing a re-evaluation of public health powers and ethics.
% FOUNDING_PROBLEM: How to effectively protect public health during crises, especially through coercive measures, without unduly infringing on fundamental individual rights and freedoms, and to ensure such measures are ethically and legally justifiable.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, bioethicists, and public health practitioners widely acknowledge the ongoing challenge of balancing collective health with individual liberties. Recent global pandemics have intensified debates and court cases, continually testing and refining the application of proportionality, demonstrating its live and contested status.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The `extractiveness` (0.65) is substantial because mandates inherently impose costs and restrictions on individuals, even when deemed proportionate. `suppression` (0.75) is high due to the active enforcement required to ensure compliance and the limited alternatives for mandate targets. `theater_ratio` (0.20) is relatively low, indicating that the constraint is largely functional in its stated purpose, though some performative aspects may exist in its justification. `accessibility_collapse` (0.60) reflects that while alternatives are constrained, they are not entirely eliminated (e.g., remote work, masking options). `resistance` (0.70) is high, as mandates often face significant opposition from those whose liberties are curtailed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities and vulnerable populations, the proportionality reading is a necessary and just framework for collective protection. From the perspective of mandate targets, it can still feel highly extractive and suppressive, even if legally justified. The engine's per-seat classification will capture this divergence, showing a more beneficial classification for the former and a more extractive one for the latter.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and vulnerable populations are the primary beneficiaries, gaining collective protection and legitimacy for interventions. Mandate targets are the payers, bearing the direct costs of compliance. Civil liberties advocates and courts act as observers and adjudicators, ensuring the proportionality principle is upheld. Bodily autonomy advocates are excluded, as their categorical stance is not fully accommodated by a balancing framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading is designed to prevent public health mandates from becoming pure extraction or persisting beyond their necessity. By requiring ongoing assessment of threat severity, alternatives, coercion, and duration, it provides a mechanism to adjust or sunset mandates when they are no longer proportionate, thus mitigating mandatrophy. However, the interpretation of 'proportionality' itself can be contested, potentially allowing mandates to persist if the assessment criteria are biased.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_proportionality_reading,
    'Is this constraint accurately representing the ''proportionality_reading'' of the ''public_health_mandate_authority'' kernel, or is it conflating elements of other readings?',
    'Detailed textual analysis of legal precedents and policy documents, comparing the explicit balancing criteria against the core tenets of the sibling readings.',
    'If conflated, the classification may inaccurately reflect the structural properties of this specific reading, potentially leading to mischaracterization of its coordination and extraction functions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_proportionality_reading, conceptual, 'Verifies the distinct identity of the proportionality reading within the kernel.').

omega_variable(
    dynamic_extractiveness_assessment,
    'How does the actual extractiveness of this constraint dynamically shift with varying levels of public health threat and available alternatives?',
    'Empirical case studies across different public health crises (e.g., mild seasonal flu vs. severe pandemic) and jurisdictions with varying alternative options, measuring the actual costs imposed on individuals.',
    'If extractiveness is consistently high even for low threats, the ''proportionality'' claim may be theatrical, pushing the constraint towards a Snare. If it genuinely scales, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dynamic_extractiveness_assessment, empirical, 'Assesses the real-world variability of extraction based on context.').

omega_variable(
    definition_of_proportionality_ambiguity,
    'Is the definition and application of ''proportionality'' sufficiently clear and consistently applied, or is it subject to interpretive bias that allows for greater extraction?',
    'Analysis of judicial review outcomes and public health policy guidelines for consistency, and expert consensus on the operationalization of proportionality criteria.',
    'If ''proportionality'' is ambiguously defined or inconsistently applied, it could serve as a cover for increased extraction, pushing the constraint closer to a Snare or a more extractive Tangled Rope. Clearer definitions would reinforce its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_proportionality_ambiguity, conceptual, 'Examines the clarity and consistency of the proportionality principle.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal penalties, lack of alternatives) or internalized (social pressure, perceived moral obligation)?',
    'Post-mandate compliance trajectory: if compliance persists after legal enforcement is removed, reclassify as partially internalized. Surveys on public attitudes towards mandates and social norms.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them. This would amplify the effective extraction for mandate targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for mandate compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__proportionality_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__proportionality_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__proportionality_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__proportionality_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__proportionality_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__proportionality_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__proportionality_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__proportionality_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__proportionality_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__proportionality_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__proportionality_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__proportionality_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__proportionality_reading, suppression_requirement, 18, 0.78).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__proportionality_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__proportionality_reading, suppression_requirement, 30, 0.75).


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
