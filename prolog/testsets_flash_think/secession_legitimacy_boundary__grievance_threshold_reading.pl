% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__grievance_threshold_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__grievance_threshold_reading
 *   human_readable: Federal Authority as Snare (Grievance Threshold Reading)
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'grievance_threshold_reading' of
 *   the 'secession_legitimacy_boundary' kernel. It posits that federal
 *   authority, while nominally a coordinating mechanism, becomes an
 *   illegitimate snare when its actions cross a threshold of structural
 *   injustice against a constituent region. Secession, in this reading, is
 *   not merely a political act but a legitimate response to a broken compact.
 *   The metrics reflect the state of federal authority *after* this threshold
 *   of injustice has been crossed, from the perspective of the aggrieved
 *   party.
 *
 * KEY AGENTS:
 *   - federal_government: Primary beneficiary/agenda_setter (institutional/arbitrage)
 *   - aggrieved_region_state: Primary target/victim (organized/trapped)
 *   - non_seceding_states: Secondary beneficiary (organized/mobile)
 *   - international_observers: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.85).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.9).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, snare).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Federal Authority as Snare (Grievance Threshold Reading)").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__grievance_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, '3274f116-a3d1-468e-b930-8cb1de8c8f6e').
narrative_ontology:cs_kernel_codification('3274f116-a3d1-468e-b930-8cb1de8c8f6e', formalized).
narrative_ontology:cs_authority_grounding('3274f116-a3d1-468e-b930-8cb1de8c8f6e', extraction).
narrative_ontology:cs_interpretation_layer_present('3274f116-a3d1-468e-b930-8cb1de8c8f6e').
narrative_ontology:cs_reading_relation('3274f116-a3d1-468e-b930-8cb1de8c8f6e', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('3274f116-a3d1-468e-b930-8cb1de8c8f6e', secession_legitimacy_boundary__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('3274f116-a3d1-468e-b930-8cb1de8c8f6e', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('3274f116-a3d1-468e-b930-8cb1de8c8f6e', foundational, federal_compact_is_conditional).
narrative_ontology:cs_axiom_status(federal_compact_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('3274f116-a3d1-468e-b930-8cb1de8c8f6e', federal_compact_is_conditional, empirically_contingent).
narrative_ontology:cs_axiom('3274f116-a3d1-468e-b930-8cb1de8c8f6e', foundational, right_to_exit_unjust_union).
narrative_ontology:cs_axiom_status(right_to_exit_unjust_union, holdable).
narrative_ontology:cs_axiom_grounding('3274f116-a3d1-468e-b930-8cb1de8c8f6e', right_to_exit_unjust_union, deontological).
narrative_ontology:cs_reference_frame('3274f116-a3d1-468e-b930-8cb1de8c8f6e', conditional_federal_compact).
narrative_ontology:cs_drift_state('3274f116-a3d1-468e-b930-8cb1de8c8f6e', contemporary_era_of_resource_politics, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3274f116-a3d1-468e-b930-8cb1de8c8f6e', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, non_seceding_states).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_region_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the union's integrity, collects taxes, and enforces federal law across all states. From its perspective, secession is illegal and an existential threat to the nation-state. It benefits from the continued flow of resources and political power from all regions.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_government, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Experiences federal policies as structurally unjust, leading to economic exploitation, cultural suppression, or political marginalization. Bears the costs of these policies and is denied the right to self-determination through secession. Exit is legally and militarily suppressed.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_region_state, payer,
    organized, generational, trapped, regional).

% Benefit from the stability and resource flows of the existing federal union. They may not perceive the same level of injustice as the aggrieved region and support federal authority, fearing economic disruption or loss of shared identity if secession occurs.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, non_seceding_states, beneficiary,
    organized, generational, mobile, national).

% Monitor the conflict, assess claims of structural injustice, and evaluate the legitimacy of both federal authority and secessionist movements under international law and human rights principles. Their analysis can influence global opinion and diplomatic pressure.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, international_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The federal union was established to coordinate collective defense, economic integration, and shared governance across diverse regions, providing a framework for peaceful coexistence and mutual benefit.
% TRANSFER_FUNCTION: Moves resources (taxes, natural resources, labor) and political power from the aggrieved region to the federal center and other states, while denying the aggrieved region self-determination.
% ABSENT_VOICES: Historical and contemporary voices from the aggrieved region advocating for self-determination and documenting structural injustices are often marginalized or suppressed in national discourse, their grievances reframed as disloyalty or economic irrationality.
% DISAPPEARANCE_RATIONALE: If federal authority over the aggrieved region vanished overnight, the region would immediately declare independence, reorient its economy, and establish new international relations. The federal state would lose territory, resources, and legitimacy, triggering a profound political and economic reorganization.
% FOUNDING_PROBLEM: The federal union was founded to overcome inter-state conflicts, ensure collective security, and foster economic prosperity through a unified market and shared governance.
% FOUNDING_PROBLEM_CORROBORATION: The federal government and many non-seceding states assert the founding problems (security, economic stability) are still live. The aggrieved region, supported by some international legal scholars and human rights organizations, argues that the founding compact has been violated by structural injustice, rendering the original problem 'dead' for them and replaced by a new problem of federal overreach.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(secession_legitimacy_boundary__grievance_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the federal system is perceived as systematically siphoning resources and opportunities from the aggrieved region without commensurate benefit, constituting structural injustice. Suppression is very high (0.90) due to the federal government's legal and coercive power to prevent secession, effectively trapping the aggrieved region. Theater ratio is low (0.20) because the federal government's enforcement of unity is genuinely functional in maintaining its power, even if the justification for that power is contested. Accessibility collapse is high (0.80) as legal and practical avenues for exit are severely limited. Resistance is high (0.75) reflecting the active and sustained efforts by the aggrieved region to challenge federal authority and pursue independence.
 *
 * PERSPECTIVAL GAP:
 *   The federal government and non-seceding states perceive the union as a legitimate, beneficial coordination mechanism (a Rope or Tangled Rope), justifying its enforcement. The aggrieved region, however, experiences the same structure as a Snare, where the coordination narrative is cover for extraction and suppression. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is a clear beneficiary, collecting resources and maintaining power. Non-seceding states also benefit from the stability and shared infrastructure of the union. The aggrieved region is the primary target, bearing the costs of perceived injustice and denied self-determination. International observers are analytical, assessing the situation without direct benefit or cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling federal authority as a benign Rope when it has, in fact, become an extractive Snare due to structural injustice. It highlights that the original mandate for coordination has atrophied or been corrupted, leading to a situation where the constraint persists through coercion rather than mutual benefit. The 'contested' status of the founding problem further supports this analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_injustice_threshold_definition,
    'What objective criteria define the ''threshold of structural injustice'' that legitimizes secession, and who adjudicates whether it has been crossed?',
    'Development of internationally recognized legal standards for structural injustice in federal systems, coupled with independent, multi-stakeholder commissions to assess specific cases.',
    'If the threshold is clearly defined and objectively met, the classification of federal authority as a Snare becomes universally accepted, strengthening the legitimacy of secession. If ambiguous, the federal government''s claim of legitimate authority retains more traction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_injustice_threshold_definition, conceptual, 'Ambiguity in defining and adjudicating structural injustice.').

omega_variable(
    empirical_verifiability_of_extraction_claims,
    'Are the claims of economic exploitation and resource siphoning by the aggrieved region empirically verifiable through independent economic analysis?',
    'Comprehensive, transparent economic audits conducted by neutral international bodies, comparing resource flows, investment, and development outcomes between the aggrieved region and the federal center/other states.',
    'Strong empirical evidence of extraction would solidify the Snare classification and bolster the aggrieved region''s case for secession. Lack of clear evidence would weaken the ''structural injustice'' claim and support the federal government''s coordination narrative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_verifiability_of_extraction_claims, empirical, 'Verifiability of economic extraction claims.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''grievance_threshold_reading'' of the ''secession_legitimacy_boundary'' kernel. What would change if a sibling reading were adopted?',
    'Analysis of legal and political outcomes in jurisdictions where alternative readings (e.g., ''constitutional_impossibility_reading'') are dominant.',
    'Adopting the ''constitutional_impossibility_reading'' would reclassify federal authority as a Mountain or Rope, regardless of perceived injustice, shifting the focus from grievance to legal text. Adopting ''popular_sovereignty_reading'' would make legitimacy dependent solely on a regional referendum, bypassing the injustice threshold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t1950, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(sece_tr_t1965, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(sece_tr_t1980, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(sece_tr_t1995, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(sece_tr_t2010, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(sece_tr_t2024, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(sece_be_t1950, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(sece_be_t1965, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(sece_be_t1980, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(sece_be_t1995, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 1995, 0.78).
narrative_ontology:measurement(sece_be_t2010, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(sece_be_t2024, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t1950, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(sece_su_t1965, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 1965, 0.6).
narrative_ontology:measurement(sece_su_t1980, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(sece_su_t1995, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 1995, 0.83).
narrative_ontology:measurement(sece_su_t2010, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement(sece_su_t2024, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__grievance_threshold_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
