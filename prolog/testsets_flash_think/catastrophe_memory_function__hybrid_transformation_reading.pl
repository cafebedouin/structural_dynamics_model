% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__hybrid_transformation_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Catastrophe Memory Function: Hybrid Transformation Reading
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint describes the function of ritual in encoding both the
 *   memory of catastrophic loss (mourning practice) and the transmission of
 *   adaptive survival mechanisms (survival competence) within a single
 *   commemorative structure. Using the Passover Seder as an example, it
 *   integrates the 'bitter herbs' (D1/D4 mourning) with the 'seder
 *   performance' (D5 survival rehearsal). This reading emphasizes the
 *   ritual's hybrid nature, where the act of remembering loss is
 *   intrinsically linked to the act of preparing for future challenges,
 *   fostering both resilience and continuity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.65).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.7).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Catastrophe Memory Function: Hybrid Transformation Reading").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__hybrid_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, 'a3ea5fe4-0d95-4fa2-a164-fc255f0779ea').
narrative_ontology:cs_kernel_codification('a3ea5fe4-0d95-4fa2-a164-fc255f0779ea', formalized).
narrative_ontology:cs_authority_grounding('a3ea5fe4-0d95-4fa2-a164-fc255f0779ea', lineage).
narrative_ontology:cs_interpretation_layer_present('a3ea5fe4-0d95-4fa2-a164-fc255f0779ea').
narrative_ontology:cs_reading_relation('a3ea5fe4-0d95-4fa2-a164-fc255f0779ea', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3ea5fe4-0d95-4fa2-a164-fc255f0779ea', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('a3ea5fe4-0d95-4fa2-a164-fc255f0779ea', foundational, ritual_integrates_affect_and_action).
narrative_ontology:cs_axiom_status(ritual_integrates_affect_and_action, holdable).
narrative_ontology:cs_axiom_grounding('a3ea5fe4-0d95-4fa2-a164-fc255f0779ea', ritual_integrates_affect_and_action, conventional).
narrative_ontology:cs_axiom('a3ea5fe4-0d95-4fa2-a164-fc255f0779ea', foundational, collective_memory_serves_adaptive_function).
narrative_ontology:cs_axiom_status(collective_memory_serves_adaptive_function, holdable).
narrative_ontology:cs_axiom_grounding('a3ea5fe4-0d95-4fa2-a164-fc255f0779ea', collective_memory_serves_adaptive_function, empirically_contingent).
narrative_ontology:cs_reference_frame('a3ea5fe4-0d95-4fa2-a164-fc255f0779ea', integrated_commemorative_praxis).
narrative_ontology:cs_drift_state('a3ea5fe4-0d95-4fa2-a164-fc255f0779ea', contemporary_secularization_pressure, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a3ea5fe4-0d95-4fa2-a164-fc255f0779ea', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, participating_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, future_generations).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, individual_adherents).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, deviant_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, individual_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective body that performs and transmits the ritual. They benefit from shared identity, social cohesion, and the adaptive mechanisms encoded in the practice. They also enforce conformity through social norms and expectations.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, participating_community, agenda_setter,
    organized, generational, identity_locked, local).

% Individuals who actively participate in the ritual. They bear the costs of time, effort, emotional labor, and conformity to ritual norms, but gain a sense of belonging, meaning, and access to collective memory and adaptive strategies.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, individual_adherents, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, individual_adherents, beneficiary).

% Those who inherit the ritual and its encoded memory and adaptive mechanisms. They benefit from the continuity of identity and the lessons of survival without having directly participated in its formation or initial transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, future_generations, beneficiary,
    powerless, generational, trapped, local).

% Individuals who choose not to adhere to the ritual or interpret it outside accepted norms. They face social pressure, ostracism, or loss of community identity, bearing the costs of non-conformity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, deviant_members, excluded,
    powerless, immediate, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, deviant_members, payer).

% Academics and researchers who study the ritual's structure, function, and evolution. They analyze its mechanisms for memory preservation and adaptive transmission without direct participation or enforcement.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, ritual_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__hybrid_transformation_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__hybrid_transformation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory of catastrophic events, ensuring both the preservation of loss-memory (mourning) and the transmission of adaptive strategies (survival competence) across generations within a community.
% TRANSFER_FUNCTION: Transfers shared identity, historical narrative, emotional processing, and practical coping mechanisms from past generations to present and future participants, in exchange for their time, conformity, and emotional engagement.
% ABSENT_VOICES: Those who have left the community due to non-conformity or a rejection of the ritual's premises would object, arguing that the ritual's demands are overly burdensome or that its adaptive mechanisms are outdated. Their voices are often marginalized or dismissed as external to the tradition.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the community would lose a central mechanism for collective memory, identity formation, and intergenerational transmission of adaptive strategies. This would lead to a fragmentation of historical understanding, a weakening of social cohesion, and a potential loss of resilience in the face of future challenges, forcing the community to reorganize its social and mnemonic structures.
% FOUNDING_PROBLEM: The problem of how to collectively process and remember catastrophic loss while simultaneously encoding and transmitting the lessons learned for future survival and adaptation, preventing both historical amnesia and paralyzing grief.
% FOUNDING_PROBLEM_CORROBORATION: The participating community attests that the problem of balancing memory and adaptation remains live, citing ongoing challenges to collective identity and the need for resilience. Ritual scholars and historians, from outside the immediate community, corroborate the historical efficacy of such rituals in addressing these dual challenges, even while acknowledging contemporary pressures on adherence.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__hybrid_transformation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` (0.65) is substantial because the ritual demands significant time, emotional investment, and conformity from individual adherents, which can be perceived as a cost. `Suppression` (0.70) is high due to strong social pressure and identity-lock mechanisms within the community; deviation can lead to social exclusion. `Theater_ratio` (0.40) is moderate, reflecting that while rituals have performative elements, this reading emphasizes their functional role in memory and adaptation, not mere spectacle. The slight dip in extractiveness and suppression towards the end of the interval reflects a degree of adaptation to modern contexts, making adherence slightly less burdensome for some, though the core demands remain.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the `participating_community`, the ritual is a vital, self-sustaining mechanism for cultural continuity and resilience. From the perspective of `individual_adherents`, it can be experienced as a demanding obligation, even while providing profound meaning. The engine's per-seat classification will highlight this divergence, showing the community as a beneficiary and individuals as payers, despite shared cultural goals.
 *
 * DIRECTIONALITY LOGIC:
 *   The `participating_community` acts as the agenda-setter and primary beneficiary, defining and perpetuating the ritual, and benefiting from its cohesive effects. `Individual_adherents` are payers due to the demands of participation, but also beneficiaries through identity and meaning. `Future_generations` are pure beneficiaries, inheriting the cultural capital. `Deviant_members` are excluded and bear the costs of non-conformity. `Ritual_scholars` are analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balance_of_mourning_and_adaptation,
    'What is the actual balance between the mourning-practice and survival-competence functions in the ritual''s contemporary performance, and how does this balance affect participant experience?',
    'Ethnographic studies and participant surveys measuring perceived emphasis on grief vs. resilience, and correlation with community well-being and adaptive outcomes.',
    'If mourning dominates, the ritual may be perceived as more extractive (emotional burden); if adaptation dominates, it may be seen as more instrumental (less emotional depth). A balanced finding would support the hybrid reading''s claim of integrated function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_of_mourning_and_adaptation, empirical, 'The dynamic equilibrium between the two core functions of the ritual.').

omega_variable(
    ritual_efficacy_in_modern_context,
    'To what extent do the adaptive mechanisms transmitted by the ritual remain relevant and effective for contemporary challenges, or have they become largely symbolic?',
    'Longitudinal studies tracking community resilience and problem-solving capacity in response to modern crises, comparing outcomes in communities with strong ritual adherence versus those with weaker adherence.',
    'If adaptive mechanisms are largely symbolic, the ''survival-competence'' aspect of the hybrid reading weakens, potentially shifting the constraint closer to a ''piton'' (theatrical maintenance) or a ''snare'' (extraction without functional benefit). If still effective, it reinforces the ''tangled_rope'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ritual_efficacy_in_modern_context, empirical, 'The ongoing practical relevance of ritual-encoded adaptive strategies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1950, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(cata_tr_t1960, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 1960, 0.32).
narrative_ontology:measurement(cata_tr_t1970, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 1970, 0.35).
narrative_ontology:measurement(cata_tr_t1980, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 1980, 0.38).
narrative_ontology:measurement(cata_tr_t1990, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(cata_tr_t2000, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(cata_tr_t2010, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 2010, 0.41).
narrative_ontology:measurement(cata_tr_t2020, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t1950, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(cata_be_t1960, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 1960, 0.58).
narrative_ontology:measurement(cata_be_t1970, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 1970, 0.61).
narrative_ontology:measurement(cata_be_t1980, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 1980, 0.63).
narrative_ontology:measurement(cata_be_t1990, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(cata_be_t2000, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 2000, 0.67).
narrative_ontology:measurement(cata_be_t2010, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(cata_be_t2020, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1950, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(cata_su_t1960, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 1960, 0.63).
narrative_ontology:measurement(cata_su_t1970, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 1970, 0.66).
narrative_ontology:measurement(cata_su_t1980, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement(cata_su_t1990, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(cata_su_t2000, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(cata_su_t2010, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 2010, 0.71).
narrative_ontology:measurement(cata_su_t2020, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__survival_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_memory_function' kernel. This 'hybrid_transformation_reading' integrates both mourning and adaptive functions, while the sibling readings emphasize one aspect over the other. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
