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
 *   constraint_id: aneyoshi_stone_commitment__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Commitment (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   This constraint story represents the 'behavioral competence' reading of
 *   the Aneyoshi tsunami stone commitment. In this reading, the stone
 *   functions as a live, operational land-use rule that has successfully
 *   guided building location decisions for generations, directly contributing
 *   to the community's survival in the 2011 tsunami. The constraint is
 *   classified as a Mountain due to its enduring, almost natural-law-like
 *   influence on behavior, with negligible extraction and high accessibility
 *   collapse for alternatives (building below the line).
 *
 * KEY AGENTS:
 *   - Aneyoshi residents: Primary beneficiaries (moderate power/constrained exit) — adhere to the directive and are protected.
 *   - Local government: Agenda-setter (institutional power/constrained exit) — implicitly upholds the directive through land-use policy.
 *   - Disaster preparedness experts: Analytical observers (analytical power/analytical exit) — validate the stone's efficacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Stone Commitment (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:emerges_naturally(aneyoshi_stone_commitment__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, 'ff75b091-fd4e-4361-8a3e-35a4f33b0ebc').
narrative_ontology:cs_kernel_codification('ff75b091-fd4e-4361-8a3e-35a4f33b0ebc', fixed_text).
narrative_ontology:cs_authority_grounding('ff75b091-fd4e-4361-8a3e-35a4f33b0ebc', lineage).
narrative_ontology:cs_interpretation_layer_present('ff75b091-fd4e-4361-8a3e-35a4f33b0ebc').
narrative_ontology:cs_reading_relation('ff75b091-fd4e-4361-8a3e-35a4f33b0ebc', aneyoshi_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('ff75b091-fd4e-4361-8a3e-35a4f33b0ebc', foundational, intergenerational_tsunami_safety_is_paramount).
narrative_ontology:cs_axiom_status(intergenerational_tsunami_safety_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('ff75b091-fd4e-4361-8a3e-35a4f33b0ebc', intergenerational_tsunami_safety_is_paramount, deontological).
narrative_ontology:cs_axiom('ff75b091-fd4e-4361-8a3e-35a4f33b0ebc', foundational, historical_warnings_are_behaviorally_binding).
narrative_ontology:cs_axiom_status(historical_warnings_are_behaviorally_binding, holdable).
narrative_ontology:cs_axiom_grounding('ff75b091-fd4e-4361-8a3e-35a4f33b0ebc', historical_warnings_are_behaviorally_binding, conventional).
narrative_ontology:cs_reference_frame('ff75b091-fd4e-4361-8a3e-35a4f33b0ebc', post_1933_tsunami_rebuilding_directive).
narrative_ontology:cs_drift_state('ff75b091-fd4e-4361-8a3e-35a4f33b0ebc', contemporary_2011_tohoku_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ff75b091-fd4e-4361-8a3e-35a4f33b0ebc', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, intergenerational_wisdom).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, disaster_preparedness_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents of Aneyoshi who, for generations, have adhered to the stone's directive to build above a certain elevation. They directly benefit from the protection against tsunamis, as evidenced by their survival in the 2011 disaster. Their adherence is a cultural norm, reinforced by historical memory.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents, beneficiary,
    moderate, generational, constrained, local).

% The local government implicitly upholds the stone's directive through land-use planning and building codes that respect the traditional high-ground settlement patterns. While not actively 'enforcing' the stone, their policies align with its guidance, reinforcing its operational force.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, local_government, agenda_setter,
    institutional, generational, constrained, local).

% Academics and practitioners who study disaster resilience and intergenerational knowledge transfer. They observe the Aneyoshi case as a successful example of long-term behavioral compliance with a traditional warning system, validating the stone's effectiveness.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, disaster_preparedness_experts, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates land-use decisions across generations to ensure community safety from tsunamis by establishing a clear, high-ground building line.
% TRANSFER_FUNCTION: Transfers safety and resilience across generations by constraining individual building choices, effectively transferring potential risk from future residents to the present generation's adherence.
% ABSENT_VOICES: Past generations who died in previous tsunamis are the 'absent voices' whose experience is encoded in the stone's warning. Their absence is the very reason the stone exists.
% DISAPPEARANCE_RATIONALE: If the stone's commitment vanished, future generations might build closer to the coast, increasing their vulnerability to tsunamis and potentially leading to catastrophic loss of life, as seen in other communities that did not adhere to similar warnings.
% FOUNDING_PROBLEM: Repeated catastrophic loss of life from tsunamis, leading to the need for a permanent, intergenerational warning system to prevent future generations from rebuilding in dangerous low-lying areas.
% FOUNDING_PROBLEM_CORROBORATION: The 2011 Tohoku earthquake and tsunami, where Aneyoshi's adherence to the stone's warning resulted in zero casualties, provides strong empirical corroboration from outside the immediate community, validating the founding problem's continued relevance and the stone's efficacy.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness is very low (0.05) because the 'cost' of adherence (building higher up) is overwhelmingly offset by the benefit of survival. Suppression is low (0.1) as adherence is primarily cultural and self-enforced, rather than requiring active coercion. The theater ratio is negligible (0.02) as the stone's function is direct and effective, not performative. Accessibility collapse is high (0.9) because the catastrophic consequences of ignoring the directive make alternatives (building lower) effectively unthinkable. Resistance is minimal (0.01) due to the clear, empirically validated benefit of compliance. The claimed type is Mountain because, in this reading, the stone's directive acts as an irreducible limit on safe settlement, akin to a natural law of the landscape.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the stone's active, behavioral influence. A contrasting 'commemorative husk' reading would see the stone as a mere memorial, with its behavioral force having atrophied. The engine's classification would diverge significantly between these readings, with the 'husk' reading likely computing as a Piton or even a non-constraint, while this reading computes as a Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi residents are clear beneficiaries, as the constraint directly ensures their safety. The local government, by aligning its policies, also benefits from a resilient community. There are no direct 'victims' in this reading, as the constraint serves a collective good with minimal individual cost. The directionality for residents is near 0.0 (full beneficiary) due to the life-saving outcome.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_commemorative_function,
    'Is the Aneyoshi stone primarily an active behavioral constraint on land use, or has it largely decayed into a commemorative artifact?',
    'Longitudinal ethnographic study of land-use decisions and community narratives in Aneyoshi and comparable communities over multiple generations, particularly after major disaster events.',
    'If resolved as a behavioral constraint (this reading), the classification as Mountain holds. If resolved as a commemorative husk, the constraint would reclassify as a Piton (atrophied function, maintained theatrically) or even a non-constraint, with significantly higher theater_ratio and lower accessibility_collapse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_vs_commemorative_function, conceptual, 'Ambiguity between the stone''s active behavioral role and its symbolic, commemorative role.').

omega_variable(
    natural_law_vs_cultural_norm,
    'To what extent does the stone''s directive function as an ''emerges_naturally'' physical limit (a natural law of tsunami safety), versus a culturally constructed and maintained norm?',
    'Comparative analysis with other coastal communities: if similar warnings are ignored with catastrophic results, it highlights the cultural maintenance aspect. If all communities universally avoid low-lying areas after a tsunami, it points to a more ''natural law'' understanding.',
    'If more strongly a cultural norm, the ''emerges_naturally'' flag might be reconsidered, potentially shifting the classification from a pure Mountain to a highly effective Rope, acknowledging the human coordination component more explicitly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_cultural_norm, conceptual, 'Ambiguity in the ''naturalness'' of the constraint''s emergence and persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1933, 0.02).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1950, 0.02).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1970, 0.02).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1990, 0.02).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2011, 0.02).

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
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, disaster_resilience_policy).

% DUAL FORMULATION NOTE:
% This constraint is one of two primary readings of the 'aneyoshi_stone_commitment' kernel. This 'behavioral_competence_reading' emphasizes the stone's active role in shaping land-use behavior, while the 'commemorative_husk_reading' (a sibling constraint) focuses on its symbolic, non-operational function. Both are linked as part of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
