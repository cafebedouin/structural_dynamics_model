% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__proportionality_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimate_health_intervention__proportionality_reading
 *   human_readable: Proportionality Principle for Public Health Interventions
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality reading' of the
 *   legitimate_health_intervention kernel. It asserts that public health
 *   interventions must be proportional to the threat level, balancing
 *   population harm reduction with individual autonomy. The severity of
 *   interventions and the degree of individual extraction (ε) are conditional
 *   on disease characteristics like transmissibility and case-fatality rate.
 *   This reading introduces a conditional structure to public health policy,
 *   aiming to prevent both under- and over-intervention.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.7).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.8).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Proportionality Principle for Public Health Interventions").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, '1f249c0c-80ca-4d02-b443-494cb4df097e').
narrative_ontology:cs_kernel_codification('1f249c0c-80ca-4d02-b443-494cb4df097e', formalized).
narrative_ontology:cs_authority_grounding('1f249c0c-80ca-4d02-b443-494cb4df097e', lineage).
narrative_ontology:cs_interpretation_layer_present('1f249c0c-80ca-4d02-b443-494cb4df097e').
narrative_ontology:cs_reading_relation('1f249c0c-80ca-4d02-b443-494cb4df097e', legitimate_health_intervention__public_health_primary, influences).
narrative_ontology:cs_reading_relation('1f249c0c-80ca-4d02-b443-494cb4df097e', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('1f249c0c-80ca-4d02-b443-494cb4df097e', foundational, proportionality_principle_is_foundational).
narrative_ontology:cs_axiom_status(proportionality_principle_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('1f249c0c-80ca-4d02-b443-494cb4df097e', proportionality_principle_is_foundational, deontological).
narrative_ontology:cs_axiom('1f249c0c-80ca-4d02-b443-494cb4df097e', secondary, conditional_autonomy_respect).
narrative_ontology:cs_axiom_status(conditional_autonomy_respect, holdable).
narrative_ontology:cs_axiom_grounding('1f249c0c-80ca-4d02-b443-494cb4df097e', conditional_autonomy_respect, conventional).
narrative_ontology:cs_reference_frame('1f249c0c-80ca-4d02-b443-494cb4df097e', liberal_democratic_balancing_framework).
narrative_ontology:cs_drift_state('1f249c0c-80ca-4d02-b443-494cb4df097e', contemporary_pandemic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1f249c0c-80ca-4d02-b443-494cb4df097e', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, general_public).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, individuals_subject_to_interventions).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, autonomy_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, medical_professionals).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, medical_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting population health, they implement and enforce interventions. They must balance public good with individual rights, often under political and scientific pressure. Their legitimacy depends on demonstrating proportionality.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Bear the direct costs of interventions (e.g., isolation, mandatory vaccination, business closures). Their autonomy is curtailed, and their ability to resist is limited by legal mandates and social pressure. The severity of the intervention directly impacts their lives.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, individuals_subject_to_interventions, payer,
    powerless, immediate, constrained, local).

% Benefits from reduced disease transmission and protection of healthcare systems. They generally support interventions they perceive as necessary and proportional, but can become resistant if the costs outweigh the perceived benefits or if the proportionality is questioned.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, general_public, beneficiary,
    moderate, biographical, mobile, national).

% Actively champion individual rights and bodily integrity, bearing the 'cost' of reduced autonomy when interventions are implemented. They scrutinize interventions for overreach and disproportionate impact, often challenging them legally and politically.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, autonomy_advocates, payer,
    organized, biographical, mobile, national).

% Adjudicate legal challenges to public health interventions, assessing whether they meet constitutional standards of proportionality and necessity. Their rulings can significantly alter the scope and enforcement of such constraints.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from a healthier population and reduced strain on healthcare systems. They also bear the costs of implementing interventions, facing ethical dilemmas, burnout, and public scrutiny regarding the proportionality of measures.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, medical_professionals, beneficiary,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, medical_professionals, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to mitigate public health threats by establishing a framework for state intervention that balances population-level protection with respect for individual liberties, adapting to specific disease characteristics.
% TRANSFER_FUNCTION: Transfers a degree of individual autonomy and economic freedom from individuals to public health authorities, in exchange for collective health security and reduced disease burden for the general public.
% ABSENT_VOICES: Those most marginalized and disproportionately affected by interventions (e.g., low-income workers, undocumented immigrants) often have their voices absent from the policy-making process, despite bearing significant costs. They would argue for more equitable and less coercive measures.
% DISAPPEARANCE_RATIONALE: Without a proportionality principle, public health interventions would either become excessively coercive (prioritizing public health absolutely) or entirely ineffective (prioritizing individual autonomy absolutely), leading to a breakdown in public trust and a chaotic response to health crises. The legal and ethical frameworks governing state power would need fundamental re-evaluation.
% FOUNDING_PROBLEM: To establish a legitimate basis for state intervention in individual lives during public health crises, ensuring that such power is exercised responsibly and does not unduly infringe on fundamental rights, while still effectively protecting the population.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, medical ethicists, and constitutional law experts consistently affirm the ongoing necessity of the proportionality principle to navigate the inherent tension between individual liberty and collective well-being in public health. This is corroborated by ongoing debates and legal challenges during every major health crisis.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimate_health_intervention__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__proportionality_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates collective health action (beneficiaries: general_public, public_health_authorities) but does so through asymmetric extraction from individuals (victims: individuals_subject_to_interventions, autonomy_advocates) via actively enforced measures. Extractiveness (0.70) and suppression (0.80) are high, reflecting the significant costs imposed on individuals and the coercive power required to enforce interventions, especially during severe crises. The theater ratio is low (0.20) because the interventions are generally functional, though the justification for specific measures can be debated. The temporal measurements show an increase in extractiveness and suppression during the 2020-2025 period, reflecting the heightened state response to a global pandemic, followed by a slight decrease as the immediate crisis subsides and re-evaluation occurs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities, the proportionality principle is a necessary framework for legitimate governance, allowing them to act decisively while respecting rights. From the perspective of individuals subject to interventions, the same principle can feel like a justification for state overreach, especially when the perceived threat does not align with the severity of the intervention. Autonomy advocates view it as a constant battleground to protect fundamental freedoms.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda-setters and beneficiaries, gaining the capacity to manage crises. The general public are beneficiaries of collective health security. Individuals subject to interventions and autonomy advocates are payers, bearing the direct costs of curtailed liberty and economic activity. Constitutional courts act as both observers and potential agenda-setters, ensuring the proportionality principle is upheld. Medical professionals are dual-positioned, benefiting from public health but also bearing the ethical and practical burdens of implementing interventions.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality principle is designed to prevent mandatrophy by requiring continuous re-evaluation of interventions against evolving threat levels. If interventions persist beyond their proportional justification, the principle itself is violated, signaling a potential drift towards pure extraction. The contestation around its application during crises is precisely how its mandate is tested and, ideally, renewed or adjusted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    weighting_autonomy_vs_population_harm,
    'How should individual autonomy and population harm be quantitatively weighted against each other when determining the proportionality of an intervention?',
    'Development of a widely accepted ethical calculus or a robust legal framework that provides clear, consistent guidance for weighting these values across different contexts and disease characteristics.',
    'Resolution would reduce contestation over specific interventions, potentially lowering perceived extractiveness and resistance by providing a clearer, more predictable application of the principle. Lack of resolution perpetuates high resistance and perceived extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(weighting_autonomy_vs_population_harm, conceptual, 'Ambiguity in the ethical weighting of competing values within the proportionality principle.').

omega_variable(
    empirical_threat_level_assessment,
    'What is the true, empirically verifiable threat level of a given disease, and how reliably can it be assessed in real-time to inform proportional interventions?',
    'Improved epidemiological modeling, real-time data collection, and transparent, independent scientific consensus mechanisms that are insulated from political pressure.',
    'More accurate threat assessment would strengthen the legitimacy of interventions, reducing perceived suppression and resistance. Inaccurate or contested assessment fuels claims of disproportionality and overreach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_threat_level_assessment, empirical, 'Uncertainty in the empirical basis for determining threat levels.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of individual autonomy structural (legal mandates, fines) or internalized (social pressure, fear of ostracism)?',
    'Post-intervention behavioral analysis: if compliance persists after legal mandates are lifted, reclassify as partially internalized. Longitudinal studies on public trust and social norms.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest, making exit more difficult even after formal mandates are removed. This would increase the effective extractiveness for individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for individual compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1950, legitimate_health_intervention__proportionality_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(legi_tr_t1970, legitimate_health_intervention__proportionality_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(legi_tr_t1990, legitimate_health_intervention__proportionality_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(legi_tr_t2010, legitimate_health_intervention__proportionality_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(legi_tr_t2020, legitimate_health_intervention__proportionality_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(legi_tr_t2025, legitimate_health_intervention__proportionality_reading, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(legi_be_t1950, legitimate_health_intervention__proportionality_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(legi_be_t1970, legitimate_health_intervention__proportionality_reading, base_extractiveness, 1970, 0.58).
narrative_ontology:measurement(legi_be_t1990, legitimate_health_intervention__proportionality_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(legi_be_t2010, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(legi_be_t2020, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2020, 0.75).
narrative_ontology:measurement(legi_be_t2025, legitimate_health_intervention__proportionality_reading, base_extractiveness, 2025, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1950, legitimate_health_intervention__proportionality_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(legi_su_t1970, legitimate_health_intervention__proportionality_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(legi_su_t1990, legitimate_health_intervention__proportionality_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(legi_su_t2010, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(legi_su_t2020, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2020, 0.85).
narrative_ontology:measurement(legi_su_t2025, legitimate_health_intervention__proportionality_reading, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
