% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Catastrophe Memory Preservation (Survival Competence Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes a ritual practice as a mechanism for preserving
 *   operational threat-recognition capacity across generations, ensuring the
 *   long-term survival of a group. It is a 'survival competence' reading of
 *   the broader 'catastrophe memory preservation' kernel. The ritual demands
 *   costly participation from the present generation (victims) to benefit
 *   future generations (beneficiaries), making it a Tangled Rope. The high
 *   extractiveness reflects the significant personal cost and suppression of
 *   individual autonomy required for adherence, while the coordination
 *   function is the genuine transfer of survival skills.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.7).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Catastrophe Memory Preservation (Survival Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, '1be781cc-4bb3-4879-9dfa-7af024e4dd22').
narrative_ontology:cs_kernel_codification('1be781cc-4bb3-4879-9dfa-7af024e4dd22', implicit).
narrative_ontology:cs_authority_grounding('1be781cc-4bb3-4879-9dfa-7af024e4dd22', lineage).
narrative_ontology:cs_interpretation_layer_present('1be781cc-4bb3-4879-9dfa-7af024e4dd22').
narrative_ontology:cs_reading_relation('1be781cc-4bb3-4879-9dfa-7af024e4dd22', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('1be781cc-4bb3-4879-9dfa-7af024e4dd22', catastrophe_memory_preservation__hybrid_atrophy_reading, coexists_with).
narrative_ontology:cs_axiom('1be781cc-4bb3-4879-9dfa-7af024e4dd22', foundational, ritual_transmits_operational_knowledge).
narrative_ontology:cs_axiom_status(ritual_transmits_operational_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('1be781cc-4bb3-4879-9dfa-7af024e4dd22', ritual_transmits_operational_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('1be781cc-4bb3-4879-9dfa-7af024e4dd22', foundational, collective_survival_demands_individual_sacrifice).
narrative_ontology:cs_axiom_status(collective_survival_demands_individual_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('1be781cc-4bb3-4879-9dfa-7af024e4dd22', collective_survival_demands_individual_sacrifice, deontological).
narrative_ontology:cs_reference_frame('1be781cc-4bb3-4879-9dfa-7af024e4dd22', post_catastrophe_founding_era).
narrative_ontology:cs_drift_state('1be781cc-4bb3-4879-9dfa-7af024e4dd22', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1be781cc-4bb3-4879-9dfa-7af024e4dd22', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, future_generations).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in demanding rituals, often involving reenactment of past traumas or strict adherence to prescriptive behaviors. They bear the costs of time, emotional labor, and suppressed individual autonomy, believing it is necessary for the group's future. Their identity is often deeply intertwined with the ritual practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants, payer,
    moderate, biographical, identity_locked, local).

% Are the intended beneficiaries of the ritual, receiving the transferred knowledge and operational competence for recognizing and responding to future threats. They do not directly participate in the current costs but depend on the present generation's adherence for their survival.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, future_generations, beneficiary,
    powerless, generational, analytical, local).

% Administer and enforce the ritual practices, ensuring fidelity to tradition and the effective transfer of knowledge. They benefit from the social status and authority derived from their role, and their primary concern is the long-term survival of the group.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_elders_or_leaders, agenda_setter,
    organized, generational, constrained, local).

% Study the ritual's efficacy in preserving and transmitting operational knowledge. They analyze historical outcomes and contemporary practice to assess whether the claimed survival competence is genuinely transferred or merely a symbolic act.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, external_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and behavior across generations to ensure the group retains the capacity to recognize and respond to existential threats, preventing catastrophic recurrence.
% TRANSFER_FUNCTION: Transfers operational knowledge, threat-recognition patterns, and behavioral protocols from past generations to future ones, at the cost of present-generation autonomy and resources.
% ABSENT_VOICES: Individual members of the present generation who question the efficacy or necessity of the demanding rituals, or who seek greater personal autonomy, are often marginalized or silenced by the collective emphasis on survival and tradition.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the group would lose its primary mechanism for transmitting critical survival knowledge. Future generations would be unprepared for recurring threats, leading to a potential collapse of the group's long-term viability and identity.
% FOUNDING_PROBLEM: The group faced a catastrophic event that nearly led to its extinction, and the ritual was established to prevent such a disaster from ever happening again by embedding the lessons learned into collective practice.
% FOUNDING_PROBLEM_CORROBORATION: Ritual elders and historical records within the community attest to the founding catastrophe and the ritual's role. External anthropologists and historians corroborate the historical event and the community's consistent narrative of its purpose, though they may dispute the ritual's actual efficacy in operational transfer.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_preservation__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__survival_competence_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) due to the demanding nature of the rituals, which often involve significant time, emotional burden, and suppression of individual desires for the sake of collective memory and future safety. Suppression (0.6) is present through social pressure and the deep identity-locking of participants, making exit difficult. Theater ratio is low (0.2) because, from this reading's perspective, the ritual's primary function of operational transfer is still active and effective, not merely performative. The metrics reflect the structural reality of a costly, actively enforced mechanism for intergenerational knowledge transfer.
 *
 * PERSPECTIVAL GAP:
 *   The present generation experiences the ritual as a burden and a constraint on their autonomy, while the elders and the 'idea' of future generations perceive it as a vital coordination mechanism. The engine's per-seat classification will highlight this divergence, showing a high extraction for participants and a coordination function for the collective.
 *
 * DIRECTIONALITY LOGIC:
 *   Present-generation participants are targets (payers) due to the high costs and identity-locked exit options. Future generations are beneficiaries, receiving the survival competence without direct cost. Ritual elders/leaders are agenda-setters, enforcing the practice and benefiting from the authority it confers. External observers are analytical, assessing the system's efficacy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_transfer_efficacy,
    'Is the ritual genuinely transferring operational threat-recognition capacity, or is it primarily symbolic?',
    'Empirical studies of group responses to novel threats, comparing outcomes in groups with and without such rituals, or with different ritual fidelity levels.',
    'If found to be primarily symbolic, the extractiveness for present-generation participants would remain high, but the coordination function would collapse, reclassifying the constraint as a Snare or Piton. If operational transfer is confirmed, the Tangled Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_transfer_efficacy, empirical, 'Whether the ritual''s claimed function of operational knowledge transfer is empirically verifiable.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (social pressure, lack of alternatives) or internalized (identity fusion, belief in necessity)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., individuals who leave the community still feel compelled by the ritual''s tenets), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making true freedom from the constraint more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in ritual adherence.').

omega_variable(
    reading_framing_choice,
    'Is the ''survival_competence_reading'' the most appropriate framing for this ritual, or do the ''mourning_practice_reading'' or ''hybrid_atrophy_reading'' better capture its structural reality?',
    'Longitudinal ethnographic studies combined with historical analysis of ritual evolution and community self-description, focusing on the primary function emphasized by participants and outcomes.',
    'Adopting the ''mourning_practice_reading'' would significantly lower the coordination function and likely reclassify to Snare or Piton, as the ''survival'' benefit would be deemed symbolic. The ''hybrid_atrophy_reading'' would suggest a Piton, with a once-functional core now mostly theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_choice, conceptual, 'Ambiguity in the primary function and structural classification of the ritual across different readings of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 80, 0.72).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 100, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 80, 0.62).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__survival_competence_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
