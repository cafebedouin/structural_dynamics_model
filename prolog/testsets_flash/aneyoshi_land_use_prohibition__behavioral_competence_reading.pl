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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   This constraint story instantiates the 'behavioral competence' reading of
 *   the Aneyoshi tsunami stone. In this reading, the stone is a live,
 *   operationally enforced land-use rule that successfully guided settlement
 *   patterns for 78 years (from the 1933 tsunami to the 2011 tsunami),
 *   preventing loss of life. The prohibition is understood as a direct,
 *   effective response to a physical constraint (tsunami physics),
 *   internalized by the community through generations of practice. It is a
 *   'mountain' because its force derives from natural law, not human
 *   extraction, and its persistence is due to its efficacy, not coercion. The
 *   low extractiveness reflects that the 'cost' of building higher is a
 *   necessary adaptation, not a rent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'c2e0e20f-a0a2-4cdf-b5f4-0eea86b87dce').
narrative_ontology:cs_kernel_codification('c2e0e20f-a0a2-4cdf-b5f4-0eea86b87dce', fixed_text).
narrative_ontology:cs_authority_grounding('c2e0e20f-a0a2-4cdf-b5f4-0eea86b87dce', practice).
narrative_ontology:cs_interpretation_layer_present('c2e0e20f-a0a2-4cdf-b5f4-0eea86b87dce').
narrative_ontology:cs_reading_relation('c2e0e20f-a0a2-4cdf-b5f4-0eea86b87dce', aneyoshi_land_use_prohibition__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('c2e0e20f-a0a2-4cdf-b5f4-0eea86b87dce', foundational, tsunami_stone_as_active_behavioral_guide).
narrative_ontology:cs_axiom_status(tsunami_stone_as_active_behavioral_guide, holdable).
narrative_ontology:cs_axiom_grounding('c2e0e20f-a0a2-4cdf-b5f4-0eea86b87dce', tsunami_stone_as_active_behavioral_guide, empirically_contingent).
narrative_ontology:cs_axiom('c2e0e20f-a0a2-4cdf-b5f4-0eea86b87dce', foundational, community_adherence_as_effective_survival_strategy).
narrative_ontology:cs_axiom_status(community_adherence_as_effective_survival_strategy, holdable).
narrative_ontology:cs_axiom_grounding('c2e0e20f-a0a2-4cdf-b5f4-0eea86b87dce', community_adherence_as_effective_survival_strategy, empirically_contingent).
narrative_ontology:cs_reference_frame('c2e0e20f-a0a2-4cdf-b5f4-0eea86b87dce', community_survival_through_adherence).
narrative_ontology:cs_drift_state('c2e0e20f-a0a2-4cdf-b5f4-0eea86b87dce', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c2e0e20f-a0a2-4cdf-b5f4-0eea86b87dce', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents of Aneyoshi who, for generations, have adhered to the stone's warning, building their homes above the designated tsunami inundation line. Their adherence is a matter of survival and cultural identity, passed down through families. They benefit from the physical protection the rule provides.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_residents, beneficiary,
    powerless, generational, identity_locked, local).

% The underlying physical laws governing tsunami generation and propagation, which dictate the actual inundation zone. The stone's prohibition aligns with these physical realities, making the constraint a 'mountain' from this perspective.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, tsunami_physics, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(aneyoshi_land_use_prohibition__behavioral_competence_reading, tsunami_physics).

% Local government bodies responsible for land-use planning and disaster preparedness. While they do not actively 'enforce' the stone in a coercive sense, they incorporate its principles into zoning and evacuation plans, reinforcing the traditional prohibition through modern governance.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, local_authorities, agenda_setter,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates community settlement patterns to avoid repeated catastrophic loss of life and property from tsunamis, ensuring long-term survival in a high-risk coastal environment.
% TRANSFER_FUNCTION: Transfers the cost of building higher up the slope (e.g., longer commutes, less convenient access to fishing grounds) to residents, in exchange for safety from tsunamis. It also transfers knowledge and behavioral norms across generations.
% ABSENT_VOICES: Past generations who perished in tsunamis are the 'absent voices' whose experience is embodied in the stone's warning. Their absence is the very reason the stone exists. Modern developers seeking to maximize coastal property value might also object, but their voice is largely muted by the community's lived experience.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished, future generations might gradually drift back into the inundation zone, leading to catastrophic loss in subsequent tsunami events. The long-term survival strategy of the community would collapse.
% FOUNDING_PROBLEM: Repeated catastrophic loss of life and community infrastructure due to tsunamis, leading to an existential threat for the coastal settlement.
% FOUNDING_PROBLEM_CORROBORATION: The historical record of tsunami events and their devastating impact, coupled with the community's unbroken multi-generational adherence to the stone's warning, corroborates the problem's ongoing relevance. Disaster scientists and anthropologists studying the region also attest to the stone's efficacy.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness is very low (0.05) because the 'cost' imposed (building above the inundation line) is a direct, necessary adaptation to a natural hazard, not a transfer of wealth. The community is a net beneficiary of this 'cost'. Suppression is low (0.1) because adherence is driven by collective memory and survival, not active coercion; alternatives (building lower) are collapsed by the physical reality of tsunamis. Theater ratio is zero because the stone's function is entirely real and effective. The claimed type is 'mountain' because the constraint's persistence and efficacy are rooted in the unchangeable physics of tsunamis, which the community has successfully integrated into its behavior.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the stone's functional efficacy and the community's successful adaptation. A different reading (e.g., 'commemorative husk') might focus on the stone's symbolic value after its behavioral force has decayed, leading to a different classification. The engine computes the classification from the structural data; this reading asserts the structural data of a functional, natural-law-aligned constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi residents are beneficiaries: they bear the cost of building higher, but this cost is dwarfed by the benefit of survival. Tsunami physics is an 'observer' (non-agent) that dictates the terms of the constraint. Local authorities reinforce the constraint but do not extract from it. There are no identifiable victims in this reading, as the constraint serves a genuine collective survival function.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_symbolic_function,
    'Is the Aneyoshi tsunami stone primarily a live, behavior-shaping land-use rule, or has its function decayed to a commemorative symbol without direct behavioral force?',
    'Post-2011 tsunami settlement patterns: if new construction continues to adhere to the stone''s warning, it supports the behavioral competence reading. If new construction ignores it, it supports the commemorative husk reading.',
    'If the behavioral competence reading is correct, the constraint is a Mountain (or Rope) due to its alignment with natural law and effective coordination. If the commemorative husk reading is correct, it would be a Piton, as its original function has atrophied.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_vs_symbolic_function, empirical, 'Ambiguity between the stone''s active behavioral function and its symbolic, historical role.').

omega_variable(
    natural_law_vs_social_construct,
    'To what extent is the Aneyoshi land-use prohibition a ''natural law'' (dictated by tsunami physics) versus a ''social construct'' (a community''s chosen response)?',
    'Comparative analysis with other tsunami-prone communities: if similar prohibitions arise independently in diverse cultures, it supports the ''natural law'' aspect. If adherence varies widely despite similar physical risks, it highlights the ''social construct'' aspect.',
    'A stronger ''natural law'' component reinforces the Mountain classification. A stronger ''social construct'' component, especially if it benefits specific groups, could shift it towards a Rope or even Tangled Rope, depending on the beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'The balance between physical necessity and social choice in the constraint''s origin and persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1933, 0.0).
narrative_ontology:measurement(aney_tr_t1952, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1952, 0.0).
narrative_ontology:measurement(aney_tr_t1971, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1971, 0.0).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1990, 0.0).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 2011, 0.0).

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
% This constraint is one of two readings of the 'aneyoshi_land_use_prohibition' kernel. This 'behavioral competence' reading emphasizes the stone's active, functional role in guiding settlement patterns, while the 'commemorative husk' reading focuses on its symbolic, historical significance after its direct behavioral force has atrophied.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
