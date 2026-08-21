% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__commemorative_husk_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Stone Commitment (Commemorative Husk Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   This constraint story represents the 'commemorative husk' reading of the
 *   Aneyoshi tsunami stone. In this reading, the stone, erected after the
 *   1896 tsunami to warn future generations against building below a certain
 *   elevation, has lost its operational force as a land-use constraint. By
 *   2011, it functions primarily as a memorial artifact, with land-use
 *   decisions made independently of its directive. The survival of the
 *   village in 2011 is attributed to luck or other factors, not the stone's
 *   active behavioral constraint. This reading posits high extractiveness
 *   (from the original intent) and high theater, as the stone is maintained
 *   symbolically but not functionally.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, 0.85).
domain_priors:suppression_score(aneyoshi_stone_commitment__commemorative_husk_reading, 0.1).
domain_priors:theater_ratio(aneyoshi_stone_commitment__commemorative_husk_reading, 0.9).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0.9).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Stone Commitment (Commemorative Husk Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, 'ba26dc41-3f5d-452d-8df7-36a59976d7c6').
narrative_ontology:cs_kernel_codification('ba26dc41-3f5d-452d-8df7-36a59976d7c6', fixed_text).
narrative_ontology:cs_authority_grounding('ba26dc41-3f5d-452d-8df7-36a59976d7c6', lineage).
narrative_ontology:cs_interpretation_layer_present('ba26dc41-3f5d-452d-8df7-36a59976d7c6').
narrative_ontology:cs_reading_relation('ba26dc41-3f5d-452d-8df7-36a59976d7c6', aneyoshi_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('ba26dc41-3f5d-452d-8df7-36a59976d7c6', foundational, historical_warning_is_symbolic).
narrative_ontology:cs_axiom_status(historical_warning_is_symbolic, holdable).
narrative_ontology:cs_axiom_grounding('ba26dc41-3f5d-452d-8df7-36a59976d7c6', historical_warning_is_symbolic, conventional).
narrative_ontology:cs_axiom('ba26dc41-3f5d-452d-8df7-36a59976d7c6', secondary, modern_planning_supersedes_ancestral_directives).
narrative_ontology:cs_axiom_status(modern_planning_supersedes_ancestral_directives, holdable).
narrative_ontology:cs_axiom_grounding('ba26dc41-3f5d-452d-8df7-36a59976d7c6', modern_planning_supersedes_ancestral_directives, conventional).
narrative_ontology:cs_reference_frame('ba26dc41-3f5d-452d-8df7-36a59976d7c6', stone_as_historical_artifact).
narrative_ontology:cs_drift_state('ba26dc41-3f5d-452d-8df7-36a59976d7c6', contemporary_land_use_practices, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ba26dc41-3f5d-452d-8df7-36a59976d7c6', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, local_residents).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_planners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in areas below the stone's warning, making land-use decisions based on contemporary economic and social factors, not the stone's directive. They bear the diffuse, unacknowledged risk of ignoring the stone's original intent, but also benefit from the economic viability of lower-lying land. The stone is a historical curiosity, not a behavioral constraint.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, local_residents, payer,
    moderate, biographical, mobile, local).

% Are responsible for land-use zoning and disaster preparedness. They acknowledge the stone's historical significance but prioritize modern engineering, economic development, and current safety regulations, which often permit construction below the stone's implicit warning line. The stone is a symbolic artifact, not a binding planning document.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_planners, agenda_setter,
    institutional, generational, constrained, local).

% Study the history of disaster preparedness and community memory. They interpret the stone as a powerful historical artifact whose original behavioral mandate has atrophied, becoming a 'commemorative husk' that no longer actively constrains land use but serves as a reminder of past events.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_historians, observer,
    analytical, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone originally coordinated community land-use decisions to avoid tsunami risk by marking a safe elevation. In this reading, it no longer serves a live coordination function.
% TRANSFER_FUNCTION: In this reading, the stone does not actively transfer anything. Its original function was to transfer safety by constraining land use, but that function has atrophied.
% ABSENT_VOICES: The original villagers who erected the stone, and future generations who might suffer from ignored warnings, are absent. They would argue for the stone's original behavioral mandate to be reinstated as a live constraint on land use.
% DISAPPEARANCE_RATIONALE: If the stone disappeared overnight, land-use decisions would continue to be made based on contemporary factors, as they already are. Its removal would not alter current behavior, only remove a historical artifact.
% FOUNDING_PROBLEM: To prevent future generations from building in areas vulnerable to tsunamis, following a devastating event in 1896.
% FOUNDING_PROBLEM_CORROBORATION: Disaster historians and contemporary land-use patterns corroborate that the original problem of building in unsafe areas persists, but the stone's directive is no longer the active solution. Municipal planners acknowledge the historical context but prioritize modern regulations and economic development over the stone's specific warning.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the original intent of the stone (to prevent building in unsafe areas) is largely ignored, leading to a 'cost' of unacknowledged risk. Suppression is low (0.1) because there is no active enforcement of the stone's directive; people are free to build where they choose. Theater ratio is very high (0.9) as the stone is preserved and revered as a historical artifact, but its primary function has atrophied, making its maintenance largely performative. Accessibility collapse is low (0.15) because alternatives to following the stone's directive (building lower) are readily available and chosen. Resistance is low (0.05) because there is no active constraint to resist.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the original intent, the constraint has failed, leading to high extraction of safety. From the contemporary local perspective, the stone is a historical curiosity that does not impose costs or benefits on current land use, making its extractiveness effectively zero for them. The engine's classification will reflect the analytical observer's view of the decayed commitment.
 *
 * DIRECTIONALITY LOGIC:
 *   Local residents and municipal planners are 'victims' in the sense that they bear the unacknowledged risk of ignoring the stone's original warning, but they also benefit from the economic freedom to develop land as they see fit. Disaster historians are observers, analyzing the stone's shift from a live constraint to a symbolic one.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_impact_of_stone_in_2011,
    'To what extent did the physical presence and historical memory of the Aneyoshi stone, even if not a formal rule, influence land-use decisions or evacuation behavior in 2011?',
    'Detailed ethnographic studies, survivor interviews, and spatial analysis of building patterns relative to the stone''s location and evacuation routes in 2011.',
    'If a subtle, non-formal influence is detected, the ''commemorative husk'' reading''s extractiveness might be slightly lower, and its theater ratio might be reduced, suggesting a residual, implicit coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actual_impact_of_stone_in_2011, empirical, 'Assessing residual behavioral influence of the stone beyond formal rules.').

omega_variable(
    framing_of_survival_in_2011,
    'Is the survival of the Aneyoshi village in 2011 attributable to the stone''s original warning (behavioral competence) or to other factors (luck, modern infrastructure, different evacuation routes)?',
    'Comparative analysis with other tsunami-affected villages lacking such stones, and detailed reconstruction of evacuation dynamics and building resilience in Aneyoshi.',
    'If survival is strongly linked to the stone''s warning, the ''behavioral_competence_reading'' gains empirical support, challenging the ''commemorative_husk'' reading''s high extractiveness and theater. If not, the ''commemorative_husk'' reading is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_survival_in_2011, empirical, 'Determining the causal role of the stone in the 2011 tsunami survival.').

omega_variable(
    kernel_reading_divergence,
    'Is the Aneyoshi stone commitment a live land-use rule (behavioral_competence_reading) or a decayed memorial artifact (commemorative_husk_reading)?',
    'Empirical evidence of land-use decisions and community adherence to the stone''s directive over time, combined with analysis of official planning documents and disaster preparedness guidelines.',
    'If the behavioral_competence_reading is validated, the constraint would be reclassified as a Rope or Tangled Rope with lower extractiveness and theater. If the commemorative_husk_reading is validated, it remains a Piton with high extractiveness and theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'The core conceptual divergence between the two readings of the Aneyoshi stone kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1933, 0.1).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1970, 0.6).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1990, 0.8).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2011, 0.9).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1933, 0.2).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(aney_be_t1970, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2011, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 1933, 0.3).
narrative_ontology:measurement(aney_su_t1950, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement(aney_su_t1970, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(aney_su_t1990, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 2011, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
