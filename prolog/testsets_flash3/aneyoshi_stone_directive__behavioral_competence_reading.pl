% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__behavioral_competence_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: aneyoshi_stone_directive__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Directive (Behavioral Competence Reading)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'behavioral competence' reading of
 *   the Aneyoshi stone directive. In this reading, the stone markers are not
 *   merely commemorative but represent a binding, empirically validated
 *   land-use constraint. Their placement at safe elevations, based on
 *   ancestral knowledge of tsunami inundation, functions as a 'mountain' — an
 *   unchangeable physical and historical limit on human settlement. The
 *   constraint's persistence is due to its proven efficacy in saving lives,
 *   not active enforcement or extraction. This reading emphasizes the
 *   directive's role in coordinating safe human behavior with natural
 *   hazards.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Stone Directive (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, 'ae8d8383-c26f-4b05-94f6-15d804e54a3b').
narrative_ontology:cs_kernel_codification('ae8d8383-c26f-4b05-94f6-15d804e54a3b', fixed_text).
narrative_ontology:cs_authority_grounding('ae8d8383-c26f-4b05-94f6-15d804e54a3b', lineage).
narrative_ontology:cs_interpretation_layer_present('ae8d8383-c26f-4b05-94f6-15d804e54a3b').
narrative_ontology:cs_reading_relation('ae8d8383-c26f-4b05-94f6-15d804e54a3b', aneyoshi_stone_directive__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('ae8d8383-c26f-4b05-94f6-15d804e54a3b', foundational, ancestral_wisdom_is_empirical_truth).
narrative_ontology:cs_axiom_status(ancestral_wisdom_is_empirical_truth, holdable).
narrative_ontology:cs_axiom_grounding('ae8d8383-c26f-4b05-94f6-15d804e54a3b', ancestral_wisdom_is_empirical_truth, empirically_contingent).
narrative_ontology:cs_axiom('ae8d8383-c26f-4b05-94f6-15d804e54a3b', foundational, tsunami_risk_is_immutable).
narrative_ontology:cs_axiom_status(tsunami_risk_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('ae8d8383-c26f-4b05-94f6-15d804e54a3b', tsunami_risk_is_immutable, empirically_contingent).
narrative_ontology:cs_reference_frame('ae8d8383-c26f-4b05-94f6-15d804e54a3b', ancestral_tsunami_adaptation).
narrative_ontology:cs_drift_state('ae8d8383-c26f-4b05-94f6-15d804e54a3b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ae8d8383-c26f-4b05-94f6-15d804e54a3b', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__behavioral_competence_reading, coastal_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents of the Aneyoshi village, whose ancestors placed the stone markers. They benefit from the directive's guidance on safe elevation, but are also bound by the cultural and historical weight of the stones, making relocation difficult even if the directive were ignored.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, coastal_residents, beneficiary,
    powerless, generational, identity_locked, local).

% Responsible for land-use planning and disaster preparedness. They interpret the stone directive as a valid, empirically-grounded constraint on development, integrating it into zoning laws and evacuation plans. Their authority is reinforced by the directive's proven efficacy.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, local_government, agenda_setter,
    institutional, biographical, constrained, local).

% Study the historical efficacy of indigenous disaster mitigation strategies. They view the Aneyoshi stones as a successful, long-term adaptation to recurrent natural hazards, providing empirical validation for the directive's behavioral competence.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, disaster_scientists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates safe settlement patterns in a tsunami-prone region by providing clear, historically validated elevation guidelines, preventing development in hazardous zones.
% TRANSFER_FUNCTION: Transfers knowledge and safety from past generations to future ones, effectively transferring risk away from coastal residents by constraining their settlement choices.
% ABSENT_VOICES: Developers or residents who might wish to build closer to the coast for economic or aesthetic reasons are implicitly excluded by the directive's authority, but their voices are largely muted by the directive's proven efficacy.
% DISAPPEARANCE_RATIONALE: If the directive vanished, the institutional memory of safe zones would erode, leading to gradual resettlement in hazardous areas, increasing vulnerability to future tsunamis. The long-term safety of the community would be compromised.
% FOUNDING_PROBLEM: Recurrent, devastating tsunamis repeatedly wiped out coastal settlements, leading to immense loss of life and property.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of tsunami events and their impacts, geological evidence of past inundation levels, and the continued threat of tsunamis (attested by disaster scientists and local government) corroborate the founding problem's live status. The stones themselves are physical corroboration of the historical imperative.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because the directive primarily guides behavior towards safety, not away from resources. Suppression is low (0.1) as compliance is largely voluntary, driven by historical evidence and cultural respect, rather than coercion. Theater ratio is minimal (0.05) because the stones' function is direct and unambiguous. Accessibility collapse is high (0.9) as the directive effectively closes off unsafe coastal areas for settlement, and resistance is low (0.02) due to the clear and catastrophic consequences of non-compliance.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the objective, life-saving function of the stones. A 'commemorative husk' reading (a sibling constraint) would view the stones as having lost their behavioral force, becoming mere memorials, and thus would have a different extractiveness and suppression profile. This story focuses solely on the behavioral competence aspect.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal residents are beneficiaries of the safety provided by the directive, even though it constrains their choices. Local government benefits from a clear, historically validated framework for land-use planning. There are no identifiable victims in this reading, as the constraint is seen as a necessary adaptation to natural hazards, benefiting all who comply.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    directive_status_ambiguity,
    'Is the Aneyoshi stone directive primarily a behavioral constraint or a commemorative artifact?',
    'Longitudinal ethnographic study of local land-use decisions and community narratives, particularly during extended inter-tsunami periods. If new construction consistently respects the stone''s elevation, it''s behavioral; if not, it''s commemorative.',
    'If primarily commemorative, the constraint''s extractiveness and suppression would be near zero, and its classification would shift from Mountain to Piton (inertial artifact).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(directive_status_ambiguity, empirical, 'Ambiguity between active behavioral guidance and passive memorialization.').

omega_variable(
    natural_vs_cultural_mountain,
    'Is the ''mountain'' nature of this constraint derived from the physical geography of tsunami risk, or from the cultural authority of ancestral wisdom?',
    'Comparative analysis with other tsunami-prone regions lacking such directives. If similar safe settlement patterns emerge without cultural markers, the physical geography is dominant. If not, the cultural directive is the primary ''mountain''.',
    'If purely physical, the constraint is a pure Mountain. If primarily cultural, it''s a ''false summit'' Mountain, where a cultural construct is presented as natural law, potentially masking subtle forms of social control or identity-locked behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_cultural_mountain, conceptual, 'Distinguishing between a natural physical constraint and a culturally constructed one presented as natural.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(aney_tr_t15, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(aney_tr_t30, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(aney_tr_t45, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 45, 0.05).
narrative_ontology:measurement(aney_tr_t60, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 78, 0.05).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(aney_be_t15, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 15, 0.05).
narrative_ontology:measurement(aney_be_t30, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 30, 0.05).
narrative_ontology:measurement(aney_be_t45, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 45, 0.05).
narrative_ontology:measurement(aney_be_t60, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 78, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(aney_su_t15, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 15, 0.1).
narrative_ontology:measurement(aney_su_t30, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 30, 0.1).
narrative_ontology:measurement(aney_su_t45, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 45, 0.1).
narrative_ontology:measurement(aney_su_t60, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 60, 0.1).
narrative_ontology:measurement(aney_su_t78, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 78, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__behavioral_competence_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'aneyoshi_stone_directive' kernel. The 'commemorative_husk_reading' is a sibling constraint that views the stones as having lost their behavioral force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
