% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__behavioral_competence_reading, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: aneyoshi_land_use_prohibition__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes the Aneyoshi tsunami stone's land-use
 *   prohibition from the 'behavioral competence' reading. In this reading,
 *   the stone functions as a direct, operationally enforced rule, guiding
 *   community behavior to avoid tsunami risk. The prohibition was actively
 *   observed and enforced through social practice for 78 years, culminating
 *   in the 2011 tsunami where the village suffered no casualties due to
 *   adherence. This reading emphasizes the constraint's direct behavioral
 *   impact and its grounding in the physical reality of tsunami hazards.
 *
 * KEY AGENTS:
 *   - aneyoshi_residents: Payer (moderate/constrained) — bear costs of adherence, benefit from safety
 *   - tsunami_physics: Observer (universal/analytical) — the natural law component
 *   - local_authorities: Agenda Setter (organizational/constrained) — reinforce the norm
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, '7cfdec16-0cef-4b82-a19f-65208b446638').
narrative_ontology:cs_kernel_codification('7cfdec16-0cef-4b82-a19f-65208b446638', fixed_text).
narrative_ontology:cs_authority_grounding('7cfdec16-0cef-4b82-a19f-65208b446638', practice).
narrative_ontology:cs_reading_relation('7cfdec16-0cef-4b82-a19f-65208b446638', aneyoshi_land_use_prohibition__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('7cfdec16-0cef-4b82-a19f-65208b446638', foundational, tsunami_risk_is_imminent_and_behaviorally_mitigable).
narrative_ontology:cs_axiom_status(tsunami_risk_is_imminent_and_behaviorally_mitigable, holdable).
narrative_ontology:cs_axiom_grounding('7cfdec16-0cef-4b82-a19f-65208b446638', tsunami_risk_is_imminent_and_behaviorally_mitigable, empirically_contingent).
narrative_ontology:cs_axiom('7cfdec16-0cef-4b82-a19f-65208b446638', foundational, community_survival_requires_strict_adherence).
narrative_ontology:cs_axiom_status(community_survival_requires_strict_adherence, holdable).
narrative_ontology:cs_axiom_grounding('7cfdec16-0cef-4b82-a19f-65208b446638', community_survival_requires_strict_adherence, deontological).
narrative_ontology:cs_reference_frame('7cfdec16-0cef-4b82-a19f-65208b446638', post_1933_tsunami_behavioral_adaptation).
narrative_ontology:cs_drift_state('7cfdec16-0cef-4b82-a19f-65208b446638', id_2011_tohoku_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7cfdec16-0cef-4b82-a19f-65208b446638', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the 'cost' of adherence (building higher) is directly offset by the benefit of survival; it's a necessary adaptation to a physical reality, not an arbitrary imposition. Suppression is low (0.1) as enforcement is primarily social and self-reinforcing, not coercive. Theater ratio is negligible (0.02) because the constraint's function is direct and effective. Accessibility collapse is high (0.9) because the physical reality of tsunamis makes building below the line a near-certain path to disaster, effectively collapsing safe alternatives. Resistance is very low (0.01) due to the clear and present danger of tsunamis and the historical memory of past disasters.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Aneyoshi residents, the stone is a life-saving instruction, a 'mountain' of physical reality. From an external, purely symbolic reading (the 'commemorative_husk_reading'), its behavioral force might be underestimated. This reading emphasizes the direct, operational impact.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi residents are primarily payers (bear the cost of adherence) but are also the ultimate beneficiaries (survival). Tsunami physics is an 'observer' as it is the immutable force the constraint responds to. Local authorities act as agenda-setters by reinforcing the norm, but do not extract from it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as a 'piton' or 'snare' by emphasizing its continued operational relevance and direct life-saving function. The founding problem (tsunami risk) is demonstrably 'live', and the constraint's disappearance would lead to 'world_rearranges', indicating its active, non-atrophied status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint primarily a behavioral rule grounded in physical reality, or a historical memorial with decayed behavioral force?',
    'Empirical observation of land-use patterns and community adherence over time, particularly during and after disaster events. The 2011 tsunami provides strong evidence for the behavioral competence reading.',
    'If resolved towards the ''commemorative_husk_reading'', the constraint would be reclassified as a Piton, with higher theater and lower effective suppression, as its behavioral force would be seen as atrophied.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'Distinguishing the behavioral competence reading from the commemorative husk reading of the Aneyoshi land-use prohibition.').

omega_variable(
    natural_law_vs_social_construct,
    'To what extent is the prohibition a ''mountain'' (natural law of tsunami physics) versus a ''rope'' (social construct of community agreement)?',
    'Analysis of community decision-making processes and the degree of active enforcement versus internalized norm. If adherence is purely voluntary and not tied to physical risk, it leans towards social construct.',
    'If resolved as primarily a social construct, the ''emerges_naturally'' flag would be reconsidered, potentially reclassifying it as a Rope, acknowledging the coordination function without the ''natural law'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity between the physical necessity and social agreement aspects of the land-use rule.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1933, 0.02).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1950, 0.02).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1970, 0.02).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1990, 0.02).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 2011, 0.02).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1933, 0.05).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(aney_be_t1970, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1970, 0.05).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 2011, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1933, 0.1).
narrative_ontology:measurement(aney_su_t1950, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(aney_su_t1970, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(aney_su_t1990, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 2011, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__behavioral_competence_reading, attachment_coordination).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'behavioral competence' reading of the Aneyoshi land-use prohibition, emphasizing its active, life-saving function. It is linked to the 'commemorative husk' reading, which views the stone as a historical symbol with diminished behavioral force. The two readings represent different interpretations of the same physical artifact and its social impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
