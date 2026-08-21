% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__operational_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__operational_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__operational_competence_reading
 *   human_readable: Catastrophe Memory Transmission (Operational Competence Reading)
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint describes ritual as a mechanism for encoding and
 *   transmitting operational competence for survival, particularly in the
 *   context of recurring catastrophes. It focuses on the practical,
 *   functional aspects of ritual elements, such as pattern recognition,
 *   resource coordination, and threat assessment rehearsal. The reading
 *   emphasizes the 'how-to' knowledge embedded in ritual, rather than its
 *   purely symbolic or identity-forming functions. The constraint is claimed
 *   as a Rope, reflecting its primary function as a coordination mechanism
 *   for collective survival.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__operational_competence_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_transmission__operational_competence_reading, 0.2).
domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Catastrophe Memory Transmission (Operational Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, '04066149-550c-43e1-9005-cef057b7767a').
narrative_ontology:cs_kernel_codification('04066149-550c-43e1-9005-cef057b7767a', implicit).
narrative_ontology:cs_authority_grounding('04066149-550c-43e1-9005-cef057b7767a', practice).
narrative_ontology:cs_interpretation_layer_present('04066149-550c-43e1-9005-cef057b7767a').
narrative_ontology:cs_reading_relation('04066149-550c-43e1-9005-cef057b7767a', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('04066149-550c-43e1-9005-cef057b7767a', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('04066149-550c-43e1-9005-cef057b7767a', foundational, ritual_as_operational_rehearsal).
narrative_ontology:cs_axiom_status(ritual_as_operational_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('04066149-550c-43e1-9005-cef057b7767a', ritual_as_operational_rehearsal, empirically_contingent).
narrative_ontology:cs_axiom('04066149-550c-43e1-9005-cef057b7767a', secondary, survival_competence_is_transmissible).
narrative_ontology:cs_axiom_status(survival_competence_is_transmissible, holdable).
narrative_ontology:cs_axiom_grounding('04066149-550c-43e1-9005-cef057b7767a', survival_competence_is_transmissible, empirically_contingent).
narrative_ontology:cs_reference_frame('04066149-550c-43e1-9005-cef057b7767a', functional_transmission_paradigm).
narrative_ontology:cs_drift_state('04066149-550c-43e1-9005-cef057b7767a', contemporary_secular_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('04066149-550c-43e1-9005-cef057b7767a', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_generations).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, community_leaders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, ritual_participants).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, ritual_participants).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__operational_competence_reading, adaptive_cultural_evolution).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__operational_competence_reading, practical_wisdom_transmission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives the transmitted operational competence, enhancing their survival capacity in future crises. Their identity is shaped by the community's history and practices, making exit from the ritual tradition difficult.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, future_generations, beneficiary,
    powerless, generational, identity_locked, local).

% Administers and interprets the ritual, ensuring its fidelity to the operational competence it encodes. They benefit from the community's resilience and their role in maintaining it, but are constrained by the tradition itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, community_leaders, agenda_setter,
    organized, biographical, constrained, local).

% Invest time and effort in performing the ritual, internalizing the patterns and rehearsing the responses. They benefit directly from the acquired competence, but bear the cost of participation and potential misinterpretation if the operational meaning is lost.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_participants, payer,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__operational_competence_reading, ritual_participants, beneficiary).

% Studies the ritual's structure and effects, assessing its efficacy in transmitting operational competence. They are outside the direct flow of benefits or costs, providing an objective perspective.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__operational_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__operational_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action and resource allocation in anticipation of future catastrophic events by rehearsing specific responses and transmitting critical survival knowledge across generations.
% TRANSFER_FUNCTION: Transfers practical knowledge, behavioral patterns, and threat assessment heuristics from past generations to future ones, enhancing collective resilience.
% ABSENT_VOICES: Those who dismiss ritual as mere superstition or symbolic performance, failing to recognize its embedded operational content, are absent from the conversation about its functional efficacy. They would argue for more direct, propositional forms of knowledge transfer.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the community would lose a vital, non-propositional mechanism for transmitting survival competence. Future generations would be less prepared for recurring catastrophes, leading to higher costs in terms of lives and resources, and potentially the collapse of the community itself.
% FOUNDING_PROBLEM: How to transmit critical survival knowledge and coordinated responses to recurring catastrophes (e.g., famine, migration, conflict) across generations without relying solely on explicit instruction, which can be lost or misinterpreted.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of resilient communities, historical accounts of catastrophe survival, and cognitive science research on embodied cognition corroborate the ongoing need for non-propositional knowledge transmission in high-stakes environments. These sources, external to the community leaders, attest to the problem's persistent relevance.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__operational_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__operational_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_transmission__operational_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__operational_competence_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__operational_competence_reading_tests).
:- end_tests(catastrophe_memory_transmission__operational_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the primary 'cost' is participation, which yields direct survival benefits. Suppression is low (0.2) as adherence is largely voluntary, driven by perceived utility and communal benefit rather than coercion. Theater ratio is low (0.1) because the ritual's elements are understood to have direct operational significance, minimizing performative-only aspects. Accessibility collapse is moderate (0.7) because while the ritual provides a highly effective, integrated solution, alternative (though less efficient) methods of knowledge transfer might exist. Resistance is low (0.1) due to the clear benefits and communal buy-in.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ritual participants, the constraint is a beneficial coordination mechanism, albeit one requiring effort. From the analytical observer's seat, it is a fascinating example of adaptive cultural evolution. The claimed type (Rope) aligns with this reading's emphasis on functional coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are the primary beneficiaries, receiving vital survival competence. Community leaders act as agenda-setters, ensuring the ritual's integrity and efficacy. Ritual participants are both payers (time/effort) and beneficiaries (acquired competence). Analytical observers assess the system's function without direct participation. No explicit victims are identified, as the constraint is viewed as a net positive for collective survival.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_vs_symbolic_efficacy,
    'To what extent is the ritual''s efficacy truly operational (transmitting practical skills) versus symbolic (fostering identity and cohesion, which indirectly aids survival)?',
    'Empirical studies comparing survival outcomes in communities with varying degrees of explicit operational content in their catastrophe rituals, controlling for other factors. Cognitive science research on how embodied ritual practice translates into actionable knowledge.',
    'If efficacy is primarily symbolic, the constraint might be reclassified towards an ''identity coordination'' Rope or even a Tangled Rope if symbolic maintenance becomes extractive. If purely operational, it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_vs_symbolic_efficacy, empirical, 'Distinguishing the direct operational yield from indirect benefits of symbolic cohesion.').

omega_variable(
    misinterpretation_risk,
    'What is the risk that the operational competence encoded in the ritual becomes detached from its original context and is misinterpreted or misapplied by future generations, leading to maladaptive outcomes?',
    'Longitudinal studies tracking ritual evolution and its impact on community responses to novel or evolving threats. Analysis of historical cases where ritual practices became counterproductive due to loss of original operational meaning.',
    'A high risk of maladaptive misinterpretation would increase the effective extractiveness and suppression, potentially shifting the classification towards a Tangled Rope or even a Snare if the ritual actively harms rather than helps.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(misinterpretation_risk, empirical, 'Assessing the potential for ritual to become maladaptive if its operational meaning is lost.').

omega_variable(
    kernel_framing_operational_vs_hybrid,
    'Is the operational competence truly separable from the symbolic form, or is it inextricably embedded within it, as the ''hybrid embedded'' reading suggests?',
    'Conceptual analysis of the nature of non-propositional knowledge and embodied cognition, combined with ethnographic studies of how ritual knowledge is acquired and applied. If attempts to extract ''pure'' operational content destroy its efficacy, the hybrid reading is stronger.',
    'If the hybrid reading is correct, this ''operational competence'' reading is a partial, incomplete framing of the kernel, and the constraint it describes is structurally incomplete. The true constraint would be a more complex ''hybrid embedded'' type, likely with higher extractiveness due to the costs of maintaining symbolic fidelity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_operational_vs_hybrid, conceptual, 'Ambiguity in whether operational competence can be isolated from symbolic form.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 75, 0.09).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 75, 0.14).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 25, 0.18).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 75, 0.19).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__operational_competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__operational_competence_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_memory_transmission' kernel. This 'operational competence' reading focuses on the practical, functional aspects of ritual in transmitting survival skills, distinct from the 'symbol continuity' reading (identity/mourning) and the 'hybrid embedded' reading (inseparable operational and symbolic functions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
