% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__commemorative_husk_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: aneyoshi_stone_directive__commemorative_husk_reading
 *   human_readable: Aneyoshi Stone Directive as Commemorative Husk
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   The Aneyoshi stone, erected after the 1896 tsunami and respected during
 *   the 1933 Showa-Sanriku tsunami, lost its behavioral force during the
 *   78-year inter-catastrophe period (1933-2011). The generation that
 *   experienced 1933 died out; their descendants treated the stone as a
 *   landmark, not a directive. By the 1990s, development pressure made the
 *   no-build zone economically painful, but the stone's cultural weight —
 *   amplified by heritage designation and disaster tourism — prevented formal
 *   revocation. The 2011 tsunami validated the stone's elevation empirically
 *   (water stopped at the line) but this retrospective validation arrived
 *   after the directive had already become a commemorative husk. This reading
 *   treats the constraint as a snare: high extraction (suppressed development
 *   value) without the coordination function (behavioral compliance) that
 *   would justify it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, 0.75).
domain_priors:suppression_score(aneyoshi_stone_directive__commemorative_husk_reading, 0.65).
domain_priors:theater_ratio(aneyoshi_stone_directive__commemorative_husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__commemorative_husk_reading, snare).
narrative_ontology:human_readable(aneyoshi_stone_directive__commemorative_husk_reading, "Aneyoshi Stone Directive as Commemorative Husk").
narrative_ontology:topic_domain(aneyoshi_stone_directive__commemorative_husk_reading, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__commemorative_husk_reading, '0b16df80-62a3-4e4f-accd-87cd830e5e5b').
narrative_ontology:cs_kernel_codification('0b16df80-62a3-4e4f-accd-87cd830e5e5b', fixed_text).
narrative_ontology:cs_authority_grounding('0b16df80-62a3-4e4f-accd-87cd830e5e5b', lineage).
narrative_ontology:cs_interpretation_layer_present('0b16df80-62a3-4e4f-accd-87cd830e5e5b').
narrative_ontology:cs_reading_relation('0b16df80-62a3-4e4f-accd-87cd830e5e5b', aneyoshi_stone_directive__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('0b16df80-62a3-4e4f-accd-87cd830e5e5b', foundational, directive_behavioral_force_lapsed).
narrative_ontology:cs_axiom_status(directive_behavioral_force_lapsed, holdable).
narrative_ontology:cs_axiom_grounding('0b16df80-62a3-4e4f-accd-87cd830e5e5b', directive_behavioral_force_lapsed, empirically_contingent).
narrative_ontology:cs_axiom('0b16df80-62a3-4e4f-accd-87cd830e5e5b', secondary, memorial_function_supersedes_safety_function).
narrative_ontology:cs_axiom_status(memorial_function_supersedes_safety_function, holdable).
narrative_ontology:cs_axiom_grounding('0b16df80-62a3-4e4f-accd-87cd830e5e5b', memorial_function_supersedes_safety_function, conventional).
narrative_ontology:cs_reference_frame('0b16df80-62a3-4e4f-accd-87cd830e5e5b', post_1933_tsunami_compliance_era).
narrative_ontology:cs_drift_state('0b16df80-62a3-4e4f-accd-87cd830e5e5b', pre_2011_inter_catastrophe_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0b16df80-62a3-4e4f-accd-87cd830e5e5b', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, cultural_heritage_advocates).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, tourism_interests).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, local_residents_seeking_development).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, tourism_interests).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__commemorative_husk_reading, cultural_memory_preserves_disaster_wisdom).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__commemorative_husk_reading, memorial_artifacts_have_intrinsic_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Real estate developers, construction firms, and tourism operators who would build below the stone's marked elevation. They face cultural and regulatory friction from the stone's presence, which suppresses economically rational coastal development. Their exit options are constrained by land ownership patterns and the stone's cultural weight — they cannot easily develop elsewhere without losing the specific coastal location value.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests, payer,
    powerful, biographical, constrained, local).

% Aneyoshi residents who want to rebuild or expand homes and businesses in the coastal zone. They bear the opportunity cost of the stone's cultural prohibition. Some feel the directive is outdated given modern seawalls and early warning systems. Exit means leaving the community, which is constrained by kinship, livelihood, and identity ties to the village.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, local_residents_seeking_development, payer,
    moderate, biographical, constrained, local).

% Preservation societies, UNESCO-linked heritage networks, and academic folklorists who maintain the stone as a cultural asset. They gain professional recognition, funding, and narrative authority from the stone's status as a 'living disaster memory.' Their exit is mobile — they can redirect attention to other heritage sites — but they invest in this specific constraint's symbolic capital.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, cultural_heritage_advocates, beneficiary,
    organized, generational, mobile, national).

% Local and regional tourism operators who market the stone as a disaster tourism destination. They benefit from visitor traffic and narrative packaging. They also pay indirectly when the stone's prohibition limits hotel or facility development. Their exit is mobile — tourism can be rebranded around other attractions — but the stone is a unique draw.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, tourism_interests, beneficiary,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__commemorative_husk_reading, tourism_interests, payer).

% The town planning office that administratively maintains the stone's zone designation. They neither fully enforce the building prohibition nor formally revoke it. They can arbitrage between national disaster mitigation funding (which rewards 'community-based memory') and local development pressure. Their exit is arbitrage-grade — they can reclassify the zone with a stroke of a pen, but political cost prevents it.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, municipal_planning_authority, agenda_setter,
    institutional, generational, arbitrage, local).

% The generation born between the 1933 and 2011 tsunamis who grew up with the stone as background scenery, not binding directive. They were never socialized into the behavioral compliance their grandparents practiced. They would object to development restrictions they experience as archaic, but they hold no decision-making seat in planning processes. Their exclusion is structural — the constraint's legitimacy derives from ancestral authority they never consented to.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, inter_catastrophe_generation, excluded,
    powerless, biographical, trapped, local).

% Seismologists, tsunami modelers, and disaster researchers who study the stone as a data point in community resilience. They see the full structural picture: the directive's empirical validity (the 2011 tsunami stopped at the stone line), its behavioral lapse, and its current symbolic function. They neither collect nor pay — their seat is analytical.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, tsunami_risk_scientists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: solved the collective action problem of tsunami evacuation by fixing a permanent no-build line, eliminating the need for each household to independently assess risk. Currently: coordinates a shared identity narrative — 'the village that remembered' — which attracts heritage recognition and disaster tourism.
% TRANSFER_FUNCTION: Moves development rights and land-value uplift from coastal_development_interests and local_residents_seeking_development to cultural_heritage_advocates and tourism_interests (via symbolic capital and visitor economy), mediated by the municipal_planning_authority's non-enforcement/non-revocation stance.
% ABSENT_VOICES: The inter_catastrophe_generation — those who lived through the 78-year gap — are structurally excluded. They would argue the directive is a dead letter imposed by ancestors they never knew. Their absence is maintained because the constraint's legitimacy rests on 'unbroken tradition,' which their dissent would fracture.
% DISAPPEARANCE_RATIONALE: If the stone and its zone designation vanished overnight, coastal_development_interests would submit building permits within months; local_residents_seeking_development would rebuild seaward; municipal_planning_authority would lose its disaster-mitigation grant eligibility tied to 'community memory'; cultural_heritage_advocates would lose a flagship case; tourism would rebrand. The land-use pattern would physically rearrange.
% FOUNDING_PROBLEM: After the 1896 Meiji-Sanriku tsunami killed 22,000+ along the coast, Aneyoshi survivors erected the stone to solve the intergenerational memory problem: how to ensure future generations would not rebuild in the inundation zone when living memory faded.
% FOUNDING_PROBLEM_CORROBORATION: Tsunami geologists (independent of the heritage network) confirm the 2011 tsunami inundation line matched the stone's elevation — the founding problem (tsunami risk at that elevation) is empirically dead at this site due to coastal subsidence/uplift changes and the stone's specific topography. The municipal planning authority's own hazard maps (revised post-2011) show the stone line is no longer the optimal risk boundary. No party outside the heritage network attests the original safety problem remains live in its original form.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because the constraint suppresses development that would be economically rational given modern infrastructure (seawalls, early warning, evacuation routes). Suppression (0.65) is moderate-high — not legal enforcement but cultural/regulatory friction that makes development costly and uncertain. Theater ratio (0.78) is very high: the stone is actively maintained, commemorated, and cited in grant applications, but this maintenance performs 'memory' rather than enforcing safety. Accessibility collapse (0.55) is moderate — developers can technically apply for variances, but the cultural weight makes approval politically toxic. Resistance (0.45) is moderate — development interests push but lack a unified coalition to overcome the heritage narrative.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (municipal authority), the constraint is a scaffold — temporarily maintaining community cohesion and grant eligibility until a formal land-use plan replaces it. From the payer seats, it is a snare — extracting development value for a safety function that no longer operates. From the beneficiary seats, it is a rope — a coordination mechanism for heritage tourism and identity. The engine will compute these divergences from the structural data; this reading's claimed_type (snare) reflects the payer-seat reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal_development_interests and local_residents_seeking_development are payers (d near 1.0) — they bear the opportunity cost of suppressed land value. Cultural_heritage_advocates and tourism_interests are beneficiaries (d near 0.0) — they gain symbolic and economic capital from the constraint's persistence. Municipal_planning_authority is agenda_setter with arbitrage exit — they administer the constraint but could revoke it at low personal cost, so their d is pulled toward beneficiary by exit option. Inter_catastrophe_generation is excluded (no seat in the engine) but would be payer if included. Tsunami_risk_scientists are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (intergenerational tsunami memory) is dead — the 2011 tsunami empirically validated the stone's line, but this validation came too late to revive behavioral compliance; it only reinforced the memorial narrative. The constraint persists because cultural_heritage_advocates and tourism_interests benefit from its symbolic form, and municipal_planning_authority arbitrages between heritage grants and development pressure. No party bears enough cost to force revocation, and no party gains enough from the safety function to enforce it. This is classic mandatrophy: the mandate (tsunami safety) has atrophied, but the constraint remains as a commemorative husk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'Is the commemorative_husk_reading a distinct constraint from the behavioral_competence_reading, or two framings of the same constraint?',
    'Apply the epsilon-invariance test: if measuring the constraint via ''behavioral compliance rate'' yields low epsilon but measuring via ''development value suppressed'' yields high epsilon, they are two constraints. The behavioral_competence_reading measures the former; this reading measures the latter. The test confirms two constraints.',
    'If one constraint: the engine must average or choose one epsilon, obscuring the structural conflict. If two constraints (this reading''s position): each gets its own classification — behavioral_competence_reading computes as rope/tangled_rope; this reading computes as snare. The network.affects_constraints link captures the causal relationship: the husk exists because the behavioral constraint decayed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Whether the kernel ''Aneyoshi stone directive'' instantiates one constraint or two structurally distinct constraints under different readings.').

omega_variable(
    suppression_mechanism_cultural_vs_legal,
    'Is the measured suppression (0.65) structural (legal/regulatory barriers) or internalized (cultural taboo, identity fusion with ''the village that remembers'')?',
    'Post-revocation suppression trajectory: if the zone designation were formally revoked but development still stalled due to community opposition, the suppression is substantially internalized. Track building permit applications in similar non-stone-marked zones as control.',
    'If internalized, effective suppression is higher than the structural measure suggests — the constraint travels with the agents even after formal exit. This would increase the payer seats'' effective extraction and strengthen the snare classification. If purely structural, revocation would immediately release the development value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_cultural_vs_legal, empirical, 'Structural vs. internalized suppression mechanism in the commemorative husk.').

omega_variable(
    memorial_function_coordination_value,
    'Does the memorial/heritage coordination function provide genuine collective-action value (rope) or is it a cover story for extraction (snare)?',
    'Counterfactual: if the stone were relocated to a museum and the zone opened, would the community lose a genuine coordination capacity (e.g., shared evacuation identity, grant eligibility for disaster resilience) or only symbolic capital? Compare with villages that have no stone but similar heritage funding.',
    'If genuine coordination: the constraint is a tangled_rope (coordination + extraction). If cover story: pure snare. The high theater_ratio (0.78) suggests the latter, but the 2011 empirical validation complicates — the stone''s line was empirically correct, which the heritage narrative leverages.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(memorial_function_coordination_value, conceptual, 'Whether the commemorative function is a real coordination mechanism or extraction cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__commemorative_husk_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_husk_tr_t1933, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1933, 0.1).
narrative_ontology:measurement(aneyoshi_husk_tr_t1950, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(aneyoshi_husk_tr_t1970, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1970, 0.5).
narrative_ontology:measurement(aneyoshi_husk_tr_t1990, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1990, 0.68).
narrative_ontology:measurement(aneyoshi_husk_tr_t2005, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 2005, 0.75).
narrative_ontology:measurement(aneyoshi_husk_tr_t2011, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 2011, 0.78).

% Extraction over time
narrative_ontology:measurement(aneyoshi_husk_be_t1933, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1933, 0.15).
narrative_ontology:measurement(aneyoshi_husk_be_t1950, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(aneyoshi_husk_be_t1970, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(aneyoshi_husk_be_t1990, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(aneyoshi_husk_be_t2005, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 2005, 0.7).
narrative_ontology:measurement(aneyoshi_husk_be_t2011, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 2011, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_husk_su_t1933, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 1933, 0.2).
narrative_ontology:measurement(aneyoshi_husk_su_t1950, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(aneyoshi_husk_su_t1970, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(aneyoshi_husk_su_t1990, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(aneyoshi_husk_su_t2005, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement(aneyoshi_husk_su_t2011, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 2011, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_directive__commemorative_husk_reading, 0.1).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint and behavioral_competence_reading form a kernel family decomposed by the epsilon-invariance principle. The behavioral_competence_reading measures the constraint via compliance behavior (low epsilon, coordination function intact). This reading measures it via development suppression (high epsilon, coordination function decayed). They share the same physical referent (the stone and its zone) but have different ε, different stakeholder structures, and different classifications. The commemorative_husk_reading is causally downstream: the husk exists because the behavioral constraint atrophied during the inter-catastrophe period.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aneyoshi_stone_directive__commemorative_husk_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
