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
 *   human_readable: Ritual Preserves Operational Threat-Recognition Capacity (Survival Competence Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes a ritual practice interpreted as a mechanism
 *   for preserving operational threat-recognition capacity across
 *   generations. It is one reading of the 'catastrophe_memory_preservation'
 *   kernel. The ritual demands costly participation from the present
 *   generation, which is justified by the imperative to ensure future
 *   generations retain the competence to survive recurring threats. The
 *   constraint is claimed as a Rope by its proponents (a necessary
 *   coordination for survival), but the authored metrics reflect its high
 *   extractiveness and active social enforcement, leading to a computed
 *   Tangled Rope or Snare classification from the payer's seat.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.7).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.65).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Ritual Preserves Operational Threat-Recognition Capacity (Survival Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, '58dda0db-0848-405a-9c54-06823730d364').
narrative_ontology:cs_kernel_codification('58dda0db-0848-405a-9c54-06823730d364', implicit).
narrative_ontology:cs_authority_grounding('58dda0db-0848-405a-9c54-06823730d364', lineage).
narrative_ontology:cs_interpretation_layer_present('58dda0db-0848-405a-9c54-06823730d364').
narrative_ontology:cs_reading_relation('58dda0db-0848-405a-9c54-06823730d364', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('58dda0db-0848-405a-9c54-06823730d364', catastrophe_memory_preservation__hybrid_atrophy_reading, forecloses).
narrative_ontology:cs_axiom('58dda0db-0848-405a-9c54-06823730d364', foundational, ritual_as_operational_transfer).
narrative_ontology:cs_axiom_status(ritual_as_operational_transfer, holdable).
narrative_ontology:cs_axiom_grounding('58dda0db-0848-405a-9c54-06823730d364', ritual_as_operational_transfer, empirically_contingent).
narrative_ontology:cs_axiom('58dda0db-0848-405a-9c54-06823730d364', secondary, present_sacrifice_for_future_survival).
narrative_ontology:cs_axiom_status(present_sacrifice_for_future_survival, holdable).
narrative_ontology:cs_axiom_grounding('58dda0db-0848-405a-9c54-06823730d364', present_sacrifice_for_future_survival, deontological).
narrative_ontology:cs_reference_frame('58dda0db-0848-405a-9c54-06823730d364', ancestral_survival_imperative).
narrative_ontology:cs_drift_state('58dda0db-0848-405a-9c54-06823730d364', contemporary_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('58dda0db-0848-405a-9c54-06823730d364', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, future_generations_survival).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, ritual_practitioners_present_generation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in demanding rituals, sacrificing time, resources, and personal autonomy. Their identity is often deeply intertwined with the ritual, making exit difficult despite the costs. They bear the direct burden of maintaining the 'survival competence'.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_practitioners_present_generation, payer,
    moderate, biographical, identity_locked, local).

% Are the intended recipients of the preserved threat-recognition capacity. They benefit from the accumulated knowledge and behavioral patterns without direct participation in the current generation's sacrifices. Their survival is contingent on the ritual's efficacy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, future_generations_survival, beneficiary,
    powerless, generational, trapped, local).

% Administer and enforce the ritual practices, interpreting their meaning and ensuring their continuity. They hold authority derived from their role as custodians of the collective memory and survival knowledge. They benefit from the social cohesion and authority the ritual provides.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_elders_agenda_setters, agenda_setter,
    institutional, generational, constrained, local).

% Study the ritual from an academic or external perspective, analyzing its social functions, historical context, and claims of efficacy without direct participation or benefit. They can offer an objective assessment of the ritual's operational transfer.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, secular_observers, observer,
    analytical, biographical, analytical, global).

% Would argue that the ritual's primary function is symbolic mourning and collective identity formation, not operational survival competence. They might advocate for less demanding practices or alternative, more direct methods of knowledge transfer, but their voices are often marginalized by the ritual's established authority.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, mourning_advocates, excluded,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intergenerational transfer of tacit knowledge, behavioral patterns, and collective vigilance necessary for recognizing and responding to specific catastrophic threats, ensuring the long-term survival of the community.
% TRANSFER_FUNCTION: Transfers costly participation (time, emotional labor, suppression of individual autonomy) from the present generation to the future, in exchange for the preservation of critical survival competence and reduced vulnerability to recurring catastrophes.
% ABSENT_VOICES: Those who prioritize individual autonomy, question the ritual's efficacy in transferring operational competence, or advocate for alternative, less demanding methods of knowledge transfer are often excluded or silenced, as their perspectives challenge the ritual's foundational claims.
% DISAPPEARANCE_RATIONALE: If the ritual and its enforcement vanished overnight, the specific, often tacit, knowledge and behavioral patterns necessary for recognizing and responding to catastrophic threats would degrade and be lost within a generation, leaving future generations significantly more vulnerable to recurrence. The community's long-term survival competence would be severely compromised.
% FOUNDING_PROBLEM: To prevent the recurrence of a past catastrophe (e.g., famine, flood, invasion) by embedding its lessons, warning signs, and response protocols into the collective memory and embodied practices of successive generations.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of past catastrophes, ecological data indicating recurring environmental threats, and anthropological studies of societies facing similar challenges corroborate the existence and persistence of the founding problem. The ritual elders and practitioners attest to its ongoing relevance, often supported by community narratives and observed environmental patterns.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.7) because the ritual imposes significant, often non-negotiable, costs on present-day practitioners, including time, resources, and suppression of individual desires, for a benefit that accrues primarily to a future, diffuse beneficiary. Suppression (0.65) is substantial, driven by social pressure, identity-lock, and the authority of elders, which discourages deviation or questioning of the ritual's demands. Theater ratio (0.4) is moderate; while there are performative elements, this reading emphasizes the genuine functional transfer of survival competence, meaning a significant portion of the activity is indeed functional, not merely theatrical. The metrics show a slight increase in extractiveness and suppression over time, suggesting a hardening of the demands as the memory of the original catastrophe recedes, requiring more active enforcement to maintain compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ritual elders and the abstract 'future generations,' the ritual is a vital Rope, a necessary coordination mechanism for survival. From the perspective of the present-day practitioners, it is a Tangled Rope, entangling a genuine, if distant, benefit with significant, immediate extraction and suppression of autonomy. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'future_generations_survival' are the primary beneficiaries (d near 0.0), receiving the critical competence without direct cost. The 'ritual_practitioners_present_generation' are the primary targets (d near 1.0), bearing the direct costs and having their autonomy constrained. The 'ritual_elders_agenda_setters' benefit from the authority and social cohesion the ritual provides, while also bearing the burden of its maintenance and enforcement (d near 0.2-0.3). 'Mourning_advocates' are excluded, their alternative interpretations suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_efficacy_verification,
    'Is the ritual genuinely effective in transferring operational threat-recognition capacity, or is the perceived competence largely symbolic or self-reinforcing?',
    'Empirical studies comparing the response capabilities of communities maintaining such rituals versus those that have abandoned them, when faced with actual catastrophic events. Longitudinal ethnographic research on knowledge retention and application.',
    'If found ineffective, the constraint''s functional justification collapses, reclassifying it closer to a Snare (pure extraction) or Piton (theatrical maintenance) from the payer''s seat, as the coordination story would be revealed as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_efficacy_verification, empirical, 'Verifiability of the ritual''s claimed operational transfer function.').

omega_variable(
    cost_benefit_proportionality,
    'Is the cost imposed on the present generation (in terms of autonomy, resources, and emotional labor) proportional to the actual, verifiable survival benefit conferred upon future generations?',
    'Ethical and economic analysis weighing the present-day sacrifices against the statistically projected reduction in future catastrophe impact, considering alternative, less extractive methods of knowledge transfer.',
    'If costs are disproportionate, the constraint''s legitimacy as a coordination mechanism is undermined, strengthening its classification as a Snare or Tangled Rope due to excessive extraction relative to the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_benefit_proportionality, preference, 'Ethical proportionality of present sacrifice to future benefit.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint primarily about preserving operational survival competence (this reading), or is its dominant function symbolic mourning and collective identity (mourning_practice_reading), or has its operational function atrophied (hybrid_atrophy_reading)?',
    'Longitudinal ethnographic studies focusing on the actual behaviors and knowledge applied during crises, and the explicit justifications given by practitioners and elders for the ritual''s demands, compared against the claims of the other readings.',
    'If the ''mourning_practice_reading'' is dominant, the constraint''s extractiveness would be re-evaluated as less justified by operational transfer, potentially shifting its classification. If ''hybrid_atrophy_reading'' is correct, the constraint would be reclassified as a Piton, as its original function would be lost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between the ''survival competence'', ''mourning practice'', and ''hybrid atrophy'' readings of the catastrophe memory preservation kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 10, 0.67).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 50, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 50, 0.65).


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
