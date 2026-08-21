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
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe Memory Preservation (Hybrid Atrophy Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes a ritual practice that originated as a
 *   mechanism for preserving survival competence in the face of recurring
 *   catastrophes. Over time, with the advent of modernity and the mitigation
 *   of original threats, its adaptive function atrophied. The ritual now
 *   primarily serves as a collective mourning practice and a means of
 *   reinforcing in-group identity, while still imposing costs on its
 *   adherents. This is the 'hybrid atrophy' reading of the
 *   catastrophe_memory_preservation kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe Memory Preservation (Hybrid Atrophy Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, 'f3da14d3-3a93-4556-84fe-9139d9b236be').
narrative_ontology:cs_kernel_codification('f3da14d3-3a93-4556-84fe-9139d9b236be', implicit).
narrative_ontology:cs_authority_grounding('f3da14d3-3a93-4556-84fe-9139d9b236be', practice).
narrative_ontology:cs_interpretation_layer_present('f3da14d3-3a93-4556-84fe-9139d9b236be').
narrative_ontology:cs_reading_relation('f3da14d3-3a93-4556-84fe-9139d9b236be', catastrophe_memory_preservation__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('f3da14d3-3a93-4556-84fe-9139d9b236be', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_axiom('f3da14d3-3a93-4556-84fe-9139d9b236be', foundational, ritual_function_evolves_with_context).
narrative_ontology:cs_axiom_status(ritual_function_evolves_with_context, holdable).
narrative_ontology:cs_axiom_grounding('f3da14d3-3a93-4556-84fe-9139d9b236be', ritual_function_evolves_with_context, empirically_contingent).
narrative_ontology:cs_axiom('f3da14d3-3a93-4556-84fe-9139d9b236be', secondary, identity_cohesion_is_a_primary_function).
narrative_ontology:cs_axiom_status(identity_cohesion_is_a_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('f3da14d3-3a93-4556-84fe-9139d9b236be', identity_cohesion_is_a_primary_function, conventional).
narrative_ontology:cs_reference_frame('f3da14d3-3a93-4556-84fe-9139d9b236be', adaptive_survival_ritual).
narrative_ontology:cs_drift_state('f3da14d3-3a93-4556-84fe-9139d9b236be', modernity_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f3da14d3-3a93-4556-84fe-9139d9b236be', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit and perform the ritual practices, bearing the time and resource costs. They experience the ritual primarily as a means of collective mourning and identity affirmation, with little direct adaptive payoff for contemporary threats. Exit is difficult due to strong social and identity ties.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_adherents, payer,
    moderate, biographical, identity_locked, local).

% The ancestral group for whom the ritual originally provided survival competence. This 'beneficiary' is a historical construct, representing the original adaptive function that has since atrophied.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_survival_group, beneficiary,
    institutional, generational, analytical, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_survival_group).

% The collective identity of the group, which is reinforced and preserved through the ritual practice. This is the primary 'benefit' in the atrophied state, providing continuity and belonging.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity, beneficiary,
    institutional, generational, analytical, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity).

% Study the evolution of ritual practices and their changing functions over time, observing the shift from survival competence to symbolic mourning. They analyze the constraint's persistence through cultural inertia.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, cultural_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically, coordinated group action and knowledge transfer for survival in the face of recurring catastrophe. Presently, coordinates collective mourning and reinforces group identity through shared practice.
% TRANSFER_FUNCTION: Historically, transferred practical survival knowledge and threat recognition across generations. Presently, transfers a sense of shared history, belonging, and emotional solidarity within the group.
% ABSENT_VOICES: The 'rational actor' who would question the utility of costly practices without direct adaptive benefit, or those who have left the group due to the perceived anachronism of the rituals. Their absence is due to social pressure and identity lock-in.
% DISAPPEARANCE_RATIONALE: If the ritual disappeared, the present generation would lose a significant anchor for their collective identity and a primary means of expressing shared memory and mourning. While physical survival might not be immediately impacted, the social fabric and sense of continuity would be severely disrupted, leading to a reorganization of in-group cohesion.
% FOUNDING_PROBLEM: To encode and transmit critical survival knowledge and threat-recognition patterns across generations in the face of recurring environmental or social catastrophes.
% FOUNDING_PROBLEM_CORROBORATION: Cultural historians and anthropologists attest that the original survival-competence problem has largely been mitigated by modern infrastructure and knowledge systems, rendering the ritual's adaptive function obsolete. Present-generation adherents, while acknowledging the historical context, primarily experience the ritual as a cultural and identity practice, not a survival guide.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate and declining (0.45 at end) because the direct adaptive payoff has diminished, but the social and identity costs remain. Suppression is moderate (0.6) and also declining, as the external pressures for adherence lessen, but internal identity-lock mechanisms persist. Theater ratio is high and rising (0.7) because the performative aspect of mourning and identity affirmation has largely replaced the original functional purpose. The claimed type is 'piton' because the primary function has atrophied, but the constraint persists due to institutional inertia and identity-based adherence, with no single party benefiting enough to actively maintain its original form, and no party hurt enough to fix it (as the identity function provides some diffuse benefit).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'historical_survival_group', the ritual was a vital 'rope'. From the 'present_generation_adherents', it is a 'piton' – a costly practice whose original purpose is gone, but which they are bound to by identity. The 'in_group_identity' experiences it as a 'rope' for social cohesion. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'historical_survival_group' (a conceptual entity) was the original beneficiary. The 'in_group_identity' is the current diffuse beneficiary, as the ritual reinforces collective belonging. 'Present_generation_adherents' are the victims, bearing the costs of practice without the original adaptive return, but are identity-locked. Cultural historians are observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is a clear case of mandatrophy: its original mandate (survival competence) has largely expired. However, it has been re-purposed (or atrophied into) a new function (mourning and identity preservation). The 'piton' classification captures this by recognizing the atrophy of the primary function while acknowledging the persistence through inertia and secondary, diffuse benefits. The high theater ratio reflects the performative maintenance of a practice whose original utility is gone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_competence_residual,
    'What is the actual residual survival competence or threat-recognition capacity still transferred by the ritual in contemporary contexts?',
    'Empirical study of adherents'' responses to novel threats compared to non-adherents, or ethnographic analysis of how ritual knowledge is applied in modern crises.',
    'If significant residual competence is found, the extractiveness would be lower, and the theater_ratio would be lower, pushing the classification closer to a ''rope'' or ''tangled_rope'' for the adherents. If negligible, the ''piton'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_competence_residual, empirical, 'Quantifying the remaining adaptive utility of the ritual.').

omega_variable(
    identity_lock_strength,
    'To what extent is adherence driven by genuine identity fusion versus social pressure or lack of perceived alternatives?',
    'Longitudinal studies of ex-adherents'' post-exit identity formation and social integration, or comparative analysis of groups with similar rituals but varying social cohesion.',
    'If identity fusion is weaker than perceived, the ''identity_locked'' exit option for adherents would be reclassified to ''constrained'' or ''mobile'', increasing their effective power and potentially leading to higher resistance and lower extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Distinguishing genuine identity lock from social coercion.').

omega_variable(
    kernel_framing_ambiguity,
    'Is this constraint primarily about the ''preservation of memory'' (symbolic) or the ''preservation of competence'' (adaptive)?',
    'Analysis of historical texts and contemporary interpretations by group leaders and scholars, focusing on explicit statements of purpose and the emphasis placed on practical vs. symbolic elements.',
    'If framed primarily as competence preservation, the current low adaptive utility would make the ''piton'' classification more severe. If framed as memory preservation, the ''piton'' classification might be softened, as the current function aligns more closely with a (re-interpreted) mandate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'The core conceptual framing of the ritual''s purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 30, 0.65).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 40, 0.68).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 50, 0.7).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__mourning_practice_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_preservation' kernel. It describes the ritual as having atrophied from a survival-competence mechanism to a mourning and identity-preserving practice. This contrasts with the 'survival_competence_reading' (which emphasizes ongoing adaptive utility) and the 'mourning_practice_reading' (which emphasizes purely symbolic function).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
