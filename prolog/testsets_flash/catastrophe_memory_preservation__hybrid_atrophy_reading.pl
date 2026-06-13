% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__hybrid_atrophy_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe Memory Preservation (Hybrid Atrophy Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes a ritual practice that historically served to
 *   preserve survival-competence in the face of recurring catastrophes. Under
 *   modernity, with changed environmental conditions and scientific
 *   advancements, its direct adaptive function has atrophied. The ritual
 *   persists primarily as a form of collective mourning and identity
 *   reinforcement, but still carries the costly, demanding forms of its
 *   original purpose. This is the 'hybrid atrophy' reading of the
 *   'catastrophe_memory_preservation' kernel.
 *
 * KEY AGENTS:
 *   - ritual_administrators: Agenda setter (institutional/generational) — maintains the ritual structure.
 *   - present_generation_adherents: Payer (moderate/biographical) — bears the cost of ritual practice without full adaptive payoff.
 *   - in_group_identity: Beneficiary (analytical/civilizational) — benefits from the ritual's role in collective memory and cohesion.
 *   - historical_community: Observer (analytical/civilizational) — the original beneficiaries of the survival competence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.3).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe Memory Preservation (Hybrid Atrophy Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, '1928c060-2985-4162-8875-4f170637aaea').
narrative_ontology:cs_kernel_codification('1928c060-2985-4162-8875-4f170637aaea', implicit).
narrative_ontology:cs_authority_grounding('1928c060-2985-4162-8875-4f170637aaea', practice).
narrative_ontology:cs_interpretation_layer_present('1928c060-2985-4162-8875-4f170637aaea').
narrative_ontology:cs_reading_relation('1928c060-2985-4162-8875-4f170637aaea', catastrophe_memory_preservation__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('1928c060-2985-4162-8875-4f170637aaea', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_axiom('1928c060-2985-4162-8875-4f170637aaea', foundational, ritual_function_evolves_with_context).
narrative_ontology:cs_axiom_status(ritual_function_evolves_with_context, holdable).
narrative_ontology:cs_axiom_grounding('1928c060-2985-4162-8875-4f170637aaea', ritual_function_evolves_with_context, empirically_contingent).
narrative_ontology:cs_axiom('1928c060-2985-4162-8875-4f170637aaea', secondary, inherited_practice_carries_latent_cost).
narrative_ontology:cs_axiom_status(inherited_practice_carries_latent_cost, holdable).
narrative_ontology:cs_axiom_grounding('1928c060-2985-4162-8875-4f170637aaea', inherited_practice_carries_latent_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('1928c060-2985-4162-8875-4f170637aaea', adaptive_survival_mechanism).
narrative_ontology:cs_drift_state('1928c060-2985-4162-8875-4f170637aaea', modernity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1928c060-2985-4162-8875-4f170637aaea', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_administrators).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the ritual's form and schedule, interpret its meaning, and ensure its transmission. Their professional and social identity is deeply intertwined with the perpetuation of the ritual, even as its original purpose fades.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_administrators, agenda_setter,
    institutional, generational, identity_locked, local).

% Participate in the ritual, investing time, emotional energy, and sometimes financial resources. They derive a sense of belonging and collective identity, but often question the direct utility or necessity of the more demanding aspects of the practice in modern life.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_adherents, payer,
    moderate, biographical, constrained, local).

% The collective sense of self, shared history, and cultural cohesion that is reinforced by the ritual. This is an abstract beneficiary, not an active agent.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity, beneficiary,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity).

% The ancestral group for whom the ritual originally provided direct, adaptive survival competence. Their perspective is crucial for understanding the constraint's historical function and its current atrophy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_community, observer,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_community).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__hybrid_atrophy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically, it coordinated collective action and knowledge transfer for survival in the face of recurring catastrophes. Currently, it coordinates collective mourning and reinforces group identity and memory.
% TRANSFER_FUNCTION: Transfers emotional labor, time, and adherence from present-generation adherents to the maintenance of collective memory and in-group identity. Historically, it transferred survival knowledge and coordinated adaptive responses.
% ABSENT_VOICES: Skeptical younger generations or external observers who might question the utility of the ritual's more demanding aspects, arguing for adaptation or abandonment of practices that no longer serve a direct adaptive purpose. They are often marginalized by the identity-reinforcing function of the ritual.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the community would lose a central mechanism for collective mourning, memory, and identity reinforcement. While direct survival might not be immediately impacted, the social fabric and cultural continuity would be significantly disrupted, leading to a reorganization of how the group processes loss and maintains cohesion.
% FOUNDING_PROBLEM: The recurring threat of natural disasters or external aggressors, requiring intergenerational transmission of specific survival knowledge and coordinated responses.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts and archaeological evidence corroborate the original adaptive function. However, contemporary scientific understanding and modern infrastructure have largely mitigated the specific threats the ritual was designed to address. External anthropologists and sociologists, as well as some younger community members, attest that the original problem is largely 'dead' in its historical form, while ritual administrators maintain it is 'live' in a broader, symbolic sense.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).
:- end_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and declining (0.45 at end) because the ritual still demands significant time, resources, and emotional investment from adherents, but the direct adaptive payoff has diminished. Suppression is low (0.3) as adherence is largely voluntary, driven by cultural inertia and identity, rather than overt coercion. Theater ratio is high and rising (0.7) as the performative aspects (mourning, identity) have largely superseded the original functional purpose (survival competence). The declining extractiveness and rising theater ratio over time reflect the atrophy of the original function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the historical community, the ritual was a vital Rope for survival. For the present-generation adherents, it is increasingly a Piton: a costly practice whose original justification has faded, maintained by inertia and for diffuse identity benefits. Ritual administrators, however, may still perceive it as a necessary coordination mechanism for group cohesion, even if its original purpose is gone.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual administrators benefit from the perpetuation of the practice, which reinforces their role and the group's structure (d near beneficiary). Present-generation adherents bear the costs of participation without the full original benefit, making them targets (d near target). In-group identity is a beneficiary, as the ritual reinforces collective memory and cohesion. The historical community was a direct beneficiary of the survival competence, but that benefit is now largely absent for the current generation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as a Piton because its primary function (preserving survival competence) has atrophied, but the practice persists due to institutional inertia and its secondary function (mourning/identity). The high and rising theater ratio, coupled with declining extractiveness, indicates that the constraint is maintained more for its performative and identity-reinforcing aspects than for its original adaptive utility. This prevents mislabeling it as a Rope (which would imply active, beneficial coordination) or a Snare (which would imply active, concentrated extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint primarily a mechanism for survival competence, or has it atrophied to a mourning practice, or is it purely symbolic?',
    'Empirical study of ritual efficacy in modern contexts vs. historical records of adaptive function; ethnographic analysis of participant intent and perceived benefits.',
    'If primarily survival competence, extractiveness and suppression might be lower (more ''rope-like''); if purely symbolic, theater_ratio would be higher and extractiveness lower (more ''piton-like'' but less costly). This reading (hybrid atrophy) suggests a piton, but the other readings would shift classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''catastrophe_memory_preservation'' kernel. This ''hybrid_atrophy_reading'' posits a former survival function now largely atrophied to mourning, distinct from a pure survival or pure mourning reading.').

omega_variable(
    mandatrophy_degree,
    'To what extent has the original mandate of preserving survival competence truly atrophied, versus being merely reinterpreted or operating latently?',
    'Longitudinal ethnographic studies tracking the actual adaptive utility of the ritual in contemporary crises, compared to its historical role.',
    'If significant latent survival utility is found, the constraint might be reclassified as a Tangled Rope or even a Rope, with lower theater_ratio and higher effective coordination. If atrophy is complete, the Piton classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_degree, empirical, 'Assesses the degree of functional atrophy in the ritual''s original survival mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 20, 0.65).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 30, 0.7).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 30, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__survival_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_preservation' kernel. This 'hybrid_atrophy_reading' focuses on the transition from a survival-competence function to a mourning-practice function, distinct from readings that emphasize only one of these functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
