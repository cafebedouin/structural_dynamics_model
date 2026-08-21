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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: aneyoshi_land_use_prohibition__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'behavioral competence' reading of the
 *   Aneyoshi tsunami stone, where the stone functions as a live,
 *   operationally enforced land-use rule. The prohibition against building
 *   below a certain elevation, marked by the stone, was actively observed and
 *   transmitted across 78 years (1933-2011), directly contributing to the
 *   survival of the Aneyoshi community during the 2011 Tohoku tsunami. This
 *   reading emphasizes the stone's role in shaping actual behavior and
 *   land-use patterns, rather than merely serving as a historical artifact.
 *   The constraint is a Mountain because it reflects an irreducible physical
 *   limit (tsunami inundation) enforced through a robust, low-cost social
 *   practice, with negligible extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, '2abde5d0-be5b-4e14-9ad9-5f9d8e1d9444').
narrative_ontology:cs_kernel_codification('2abde5d0-be5b-4e14-9ad9-5f9d8e1d9444', fixed_text).
narrative_ontology:cs_authority_grounding('2abde5d0-be5b-4e14-9ad9-5f9d8e1d9444', practice).
narrative_ontology:cs_interpretation_layer_present('2abde5d0-be5b-4e14-9ad9-5f9d8e1d9444').
narrative_ontology:cs_reading_relation('2abde5d0-be5b-4e14-9ad9-5f9d8e1d9444', aneyoshi_land_use_prohibition__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('2abde5d0-be5b-4e14-9ad9-5f9d8e1d9444', foundational, tsunami_risk_is_perpetual_and_local).
narrative_ontology:cs_axiom_status(tsunami_risk_is_perpetual_and_local, holdable).
narrative_ontology:cs_axiom_grounding('2abde5d0-be5b-4e14-9ad9-5f9d8e1d9444', tsunami_risk_is_perpetual_and_local, empirically_contingent).
narrative_ontology:cs_axiom('2abde5d0-be5b-4e14-9ad9-5f9d8e1d9444', foundational, intergenerational_knowledge_transfer_is_effective).
narrative_ontology:cs_axiom_status(intergenerational_knowledge_transfer_is_effective, holdable).
narrative_ontology:cs_axiom_grounding('2abde5d0-be5b-4e14-9ad9-5f9d8e1d9444', intergenerational_knowledge_transfer_is_effective, empirically_contingent).
narrative_ontology:cs_reference_frame('2abde5d0-be5b-4e14-9ad9-5f9d8e1d9444', ancestral_wisdom_as_live_rule).
narrative_ontology:cs_drift_state('2abde5d0-be5b-4e14-9ad9-5f9d8e1d9444', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2abde5d0-be5b-4e14-9ad9-5f9d8e1d9444', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, future_generations).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, tsunami_hazard_awareness).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, intergenerational_risk_transmission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents of Aneyoshi village who live above the tsunami inundation line, benefiting from the safety provided by the land-use prohibition. Their ancestors placed the stone, and they continue to observe its warning, passing the knowledge to their children. Their exit from the constraint would be to build below the line, risking their lives.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_residents, beneficiary,
    moderate, generational, constrained, local).

% Future residents who inherit the land-use rule and the knowledge of tsunami risk. They benefit from the safety established by prior generations' adherence to the stone's warning. Their 'exit' is to ignore the warning, which would put them in direct harm's way.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, future_generations, beneficiary,
    powerless, generational, trapped, local).

% Academics and practitioners who study the effectiveness of traditional disaster warnings and intergenerational knowledge transfer. They observe the Aneyoshi stone as a successful example of behavioral competence in risk mitigation.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, disaster_risk_reduction_experts, observer,
    analytical, generational, analytical, global).

% The local municipal authority responsible for land-use planning and disaster preparedness. While not actively enforcing the stone's prohibition with legal force, they implicitly support it by not permitting construction in the prohibited zone, recognizing its historical and practical efficacy.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, local_government, agenda_setter,
    institutional, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates land-use behavior among residents to avoid tsunami inundation zones, ensuring collective safety and intergenerational survival in a high-risk coastal environment.
% TRANSFER_FUNCTION: Transfers knowledge of extreme natural hazard and safe land-use practices across generations, from ancestors to descendants, ensuring the survival of the community.
% ABSENT_VOICES: Developers or individuals seeking to maximize land value by building in the prohibited zone would object, arguing for economic development over traditional warnings. Their voices are absent due to strong community consensus and the clear historical record of tsunami impacts.
% DISAPPEARANCE_RATIONALE: If the behavioral competence associated with the stone vanished, new construction would eventually occur in the prohibited zone, leading to loss of life and property in future tsunami events. The community's long-term survival strategy would collapse.
% FOUNDING_PROBLEM: Repeated devastating tsunamis wiped out coastal settlements, leading ancestors to seek a permanent, clear warning to prevent future generations from rebuilding in dangerous areas.
% FOUNDING_PROBLEM_CORROBORATION: The problem of tsunami risk remains live, as evidenced by the 2011 Tohoku earthquake and tsunami, which validated the stone's warning. Disaster risk reduction experts and historical records corroborate the founding problem and its ongoing relevance, independent of local residents' claims.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very low (0.05) because the constraint primarily serves a life-saving coordination function with minimal overhead; no party profits from its operation beyond collective safety. Suppression is low (0.1) as adherence is driven by shared understanding of risk and intergenerational trust, not coercion. Theater ratio is also very low (0.05) because the stone's function is direct and practical, not performative. Accessibility collapse is high (0.9) because building below the line is understood to be a direct path to disaster, making alternatives effectively non-existent. Resistance is negligible (0.02) due to the clear and repeatedly validated efficacy of the warning.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the stone's active behavioral role. A sibling reading (commemorative_husk_reading) would view the stone as primarily a historical memorial, with its behavioral force having atrophied. The key difference lies in whether the prohibition is still 'live' as a rule or merely a 'husk' of past wisdom. This constraint's metrics reflect the 'live rule' perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi residents and future generations are direct beneficiaries, as the constraint ensures their safety. The local government implicitly supports the constraint by aligning land-use policy, acting as a secondary agenda-setter. Disaster risk reduction experts are observers, analyzing its efficacy. No identifiable victims exist, as the constraint prevents harm rather than imposing costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Mountain because its mandate (avoiding tsunami risk) is perpetually live and directly validated by natural events. There is no mandatrophy; the function has not atrophied. The classification prevents mislabeling a vital, low-cost coordination mechanism as an extractive or inertial structure, which would be the case if it were merely a 'commemorative husk' without behavioral force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_commemorative_function,
    'Is the Aneyoshi stone primarily a live land-use rule shaping behavior, or a historical memorial whose behavioral force has atrophied?',
    'Post-tsunami land-use patterns and community interviews: if new construction avoids the prohibited zone and residents explicitly cite the stone''s warning, the behavioral function is live. If construction occurs below the line and the stone is only mentioned as a historical artifact, the function is commemorative.',
    'If primarily behavioral, the constraint is a Mountain (as authored). If primarily commemorative, it would be reclassified as a Piton, reflecting an inertial, theatrical function with negligible real-world impact on land use.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_vs_commemorative_function, empirical, 'Distinguishing the stone''s active behavioral role from its symbolic, commemorative role.').

omega_variable(
    natural_law_vs_social_construct,
    'Is the land-use prohibition a ''natural law'' (derived from tsunami physics) or a ''social construct'' (a community-enforced norm)?',
    'Analysis of community adherence mechanisms: if adherence is primarily driven by direct understanding of physical risk and intergenerational knowledge transfer, it leans towards natural law. If it requires active social enforcement or legal backing, it leans towards social construct.',
    'If purely natural law, the ''beneficiaries'' are simply those who align with reality. If a social construct, the presence of beneficiaries (Aneyoshi residents) would trigger a False Summit Mountain reclassification, as a constructed rule benefits identifiable agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity between a physical constraint and a socially constructed norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1933, 0.05).
narrative_ontology:measurement(aney_tr_t1952, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1952, 0.05).
narrative_ontology:measurement(aney_tr_t1971, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1971, 0.05).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 2011, 0.05).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1933, 0.05).
narrative_ontology:measurement(aney_be_t1952, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1952, 0.05).
narrative_ontology:measurement(aney_be_t1971, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1971, 0.05).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 2011, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1933, 0.1).
narrative_ontology:measurement(aney_su_t1952, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1952, 0.1).
narrative_ontology:measurement(aney_su_t1971, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1971, 0.1).
narrative_ontology:measurement(aney_su_t1990, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 2011, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__behavioral_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Aneyoshi tsunami stone kernel. This 'behavioral competence' reading emphasizes the stone as a live, behavior-shaping land-use rule, distinct from the 'commemorative husk' reading which views it as a symbolic memorial with atrophied behavioral force. Both are linked as part of the 'aneyoshi_land_use_prohibition' constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
