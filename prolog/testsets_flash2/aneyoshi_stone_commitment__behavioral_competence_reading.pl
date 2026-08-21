% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__behavioral_competence_reading, []).

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
 *   constraint_id: aneyoshi_stone_commitment__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Commitment: Behavioral Competence Reading
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'behavioral competence' reading of the
 *   Aneyoshi tsunami stone, where the stone functions as a live, effective
 *   land-use rule that directly constrained building location decisions for
 *   78 years, leading to the community's survival in the 2011 tsunami. The
 *   low extractiveness and suppression reflect its status as a widely
 *   accepted, highly effective, and minimally coercive coordination
 *   mechanism. This reading emphasizes the stone's operational force and the
 *   causal link between compliance and disaster resilience.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Stone Commitment: Behavioral Competence Reading").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:emerges_naturally(aneyoshi_stone_commitment__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, 'c8eaad9c-69a2-4485-b071-e683a86b7749').
narrative_ontology:cs_kernel_codification('c8eaad9c-69a2-4485-b071-e683a86b7749', fixed_text).
narrative_ontology:cs_authority_grounding('c8eaad9c-69a2-4485-b071-e683a86b7749', lineage).
narrative_ontology:cs_interpretation_layer_present('c8eaad9c-69a2-4485-b071-e683a86b7749').
narrative_ontology:cs_reading_relation('c8eaad9c-69a2-4485-b071-e683a86b7749', aneyoshi_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('c8eaad9c-69a2-4485-b071-e683a86b7749', foundational, ancestral_wisdom_is_operational_guidance).
narrative_ontology:cs_axiom_status(ancestral_wisdom_is_operational_guidance, holdable).
narrative_ontology:cs_axiom_grounding('c8eaad9c-69a2-4485-b071-e683a86b7749', ancestral_wisdom_is_operational_guidance, conventional).
narrative_ontology:cs_axiom('c8eaad9c-69a2-4485-b071-e683a86b7749', foundational, survival_is_contingent_on_adherence).
narrative_ontology:cs_axiom_status(survival_is_contingent_on_adherence, holdable).
narrative_ontology:cs_axiom_grounding('c8eaad9c-69a2-4485-b071-e683a86b7749', survival_is_contingent_on_adherence, empirically_contingent).
narrative_ontology:cs_reference_frame('c8eaad9c-69a2-4485-b071-e683a86b7749', ancestral_directive_as_active_rule).
narrative_ontology:cs_drift_state('c8eaad9c-69a2-4485-b071-e683a86b7749', contemporary_post_2011_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c8eaad9c-69a2-4485-b071-e683a86b7749', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents of Aneyoshi, whose ancestors erected the stone, benefit directly from its directive to build above a certain elevation. Their lives and property are protected from tsunamis, as demonstrated by the 2011 event. Their 'trapped' exit option refers to their geographic location, not the stone itself.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents, beneficiary,
    powerless, generational, trapped, local).

% The local government implicitly upholds the stone's directive through land-use planning and building codes that align with its spirit, even if not explicitly citing the stone. They are constrained by the historical precedent and the clear evidence of its efficacy.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, local_government, agenda_setter,
    institutional, biographical, constrained, local).

% Researchers and disaster preparedness experts who study the Aneyoshi stone as a case study in long-term disaster memory and effective indigenous risk mitigation. They analyze its causal link to survival outcomes.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, analytical_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates land-use decisions across generations to ensure settlement above historical tsunami inundation lines, preventing loss of life and property.
% TRANSFER_FUNCTION: Transfers knowledge and a behavioral imperative from past generations to future ones, effectively transferring safety and resilience to the community.
% ABSENT_VOICES: Past generations who experienced the tsunamis and erected the stone are the 'absent voices' whose experience is encoded in the directive. Their warnings are present through the stone, but they cannot directly participate in contemporary debates.
% DISAPPEARANCE_RATIONALE: If the commitment vanished, future generations might build in lower, more convenient areas, leading to catastrophic loss in subsequent tsunami events. The physical landscape would remain, but the human settlement pattern and its safety would fundamentally change.
% FOUNDING_PROBLEM: Repeated catastrophic loss of life and property from tsunamis due to settlement in low-lying coastal areas.
% FOUNDING_PROBLEM_CORROBORATION: The 2011 Tohoku earthquake and tsunami, where Aneyoshi's residents survived by adhering to the stone's directive, provides empirical corroboration from outside the immediate community, validating the problem's ongoing relevance and the solution's efficacy.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(aneyoshi_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_commitment__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because the 'cost' of building higher is negligible compared to the benefit of survival. Suppression is low (0.1) because adherence is primarily driven by collective memory and self-preservation, not active enforcement. Theater ratio is low (0.05) as the stone's function is direct and effective, not performative. Accessibility collapse is high (0.9) because the alternative (building lower) is understood to be catastrophic. Resistance is very low (0.02) due to the clear and repeatedly demonstrated efficacy of the directive.
 *
 * PERSPECTIVAL GAP:
 *   This reading posits the stone as a functional, low-extraction constraint. A competing 'commemorative husk' reading would see it as a high-theater, low-impact artifact, leading to a different classification. The engine's classification will highlight which reading's metrics are more consistent with the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi residents are direct beneficiaries (d near 0.0) as the stone protects their lives. The local government, while an agenda-setter, also benefits from a resilient community and implicitly upholds the directive. There are no identifiable victims in this reading, as the 'cost' of compliance is minimal and universally beneficial.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_symbolic_function,
    'Is the Aneyoshi stone primarily a live behavioral constraint on land use, or a commemorative artifact with decayed operational force?',
    'Detailed ethnographic study of contemporary land-use decision-making processes, including interviews with residents and local planners, to determine the explicit and implicit influence of the stone''s directive on building locations post-2011.',
    'If primarily behavioral, this ''mountain'' classification holds. If primarily commemorative, the constraint would reclassify as a ''piton'' or ''tangled_rope'' with higher theater and potentially higher extraction (if new, less safe, building patterns emerge).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_vs_symbolic_function, empirical, 'Distinguishing the stone''s active behavioral influence from its symbolic role.').

omega_variable(
    natural_law_vs_social_norm,
    'Is the ''emerges_naturally'' claim for this mountain truly about natural law (the physics of tsunamis), or is it a deeply internalized social norm that functions like natural law?',
    'Comparative analysis with other tsunami-prone communities lacking such a directive: if similar behavioral patterns emerge without explicit instruction, it leans towards natural law; if not, it''s a highly effective social construct.',
    'If a social norm, the ''mountain'' classification is a ''false summit'' (reclassifying to ''tangled_rope'' by default), as it benefits identifiable agents (residents) through a constructed, albeit highly effective, mechanism. If truly natural law, the mountain holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_norm, conceptual, 'Ambiguity between a natural physical limit and a deeply internalized, highly effective social norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1933, 0.05).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2011, 0.05).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1933, 0.05).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(aney_be_t1970, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1970, 0.05).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 2011, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1933, 0.1).
narrative_ontology:measurement(aney_su_t1950, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(aney_su_t1970, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(aney_su_t1990, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 2011, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'aneyoshi_stone_commitment' kernel. It focuses on the stone's active role in shaping land-use behavior, distinct from a reading that emphasizes its symbolic or commemorative function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
