% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_embedded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__hybrid_embedded_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__hybrid_embedded_reading
 *   human_readable: Catastrophe Memory Transmission: Hybrid Embedded Reading
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint describes the 'hybrid embedded' reading of catastrophe
 *   memory transmission, where survival competence is understood as
 *   inseparable from its symbolic ritual form. Ritual fidelity is not merely
 *   symbolic but directly transmits operational capacity through
 *   non-propositional knowledge embedded in practice. This reading posits a
 *   co-constitutive relationship between form and function, where altering
 *   the ritual form inherently degrades the functional competence it conveys.
 *   It functions as a Rope, coordinating collective action and knowledge
 *   transfer through shared, embodied practice, with a strong underlying
 *   'mountain substrate' in the sense that the inseparability of form and
 *   function is treated as an irreducible feature of reality for this mode of
 *   knowledge.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.1).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Catastrophe Memory Transmission: Hybrid Embedded Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, '05db099f-4f01-4a67-a850-6e9944993618').
narrative_ontology:cs_kernel_codification('05db099f-4f01-4a67-a850-6e9944993618', implicit).
narrative_ontology:cs_authority_grounding('05db099f-4f01-4a67-a850-6e9944993618', practice).
narrative_ontology:cs_interpretation_layer_present('05db099f-4f01-4a67-a850-6e9944993618').
narrative_ontology:cs_reading_relation('05db099f-4f01-4a67-a850-6e9944993618', catastrophe_memory_transmission__operational_competence_reading, influences).
narrative_ontology:cs_reading_relation('05db099f-4f01-4a67-a850-6e9944993618', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('05db099f-4f01-4a67-a850-6e9944993618', foundational, form_function_inseparable).
narrative_ontology:cs_axiom_status(form_function_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('05db099f-4f01-4a67-a850-6e9944993618', form_function_inseparable, deontological).
narrative_ontology:cs_axiom('05db099f-4f01-4a67-a850-6e9944993618', foundational, non_propositional_transmission_efficacy).
narrative_ontology:cs_axiom_status(non_propositional_transmission_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('05db099f-4f01-4a67-a850-6e9944993618', non_propositional_transmission_efficacy, empirically_contingent).
narrative_ontology:cs_reference_frame('05db099f-4f01-4a67-a850-6e9944993618', integrated_ritual_practice).
narrative_ontology:cs_drift_state('05db099f-4f01-4a67-a850-6e9944993618', contemporary_globalized_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('05db099f-4f01-4a67-a850-6e9944993618', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in and perpetuate the rituals, embodying the non-propositional knowledge. They are the primary beneficiaries of the transmitted survival competence and the agents responsible for maintaining ritual fidelity. Their identity is often intertwined with the practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, community_members, agenda_setter,
    organized, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, community_members, beneficiary).

% Are the ultimate recipients of the survival competence transmitted through ritual. They depend entirely on the fidelity of current community members to receive this embedded knowledge, having no direct agency in its transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, future_generations, beneficiary,
    powerless, generational, trapped, local).

% Serve as the guardians of ritual fidelity, ensuring the correct enactment of symbolic forms. Their authority derives from their deep knowledge of the practice and its embedded competence. They bear the responsibility for accurate transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_elders, agenda_setter,
    powerful, generational, constrained, local).

% Study the mechanisms of cultural transmission and collective memory, analyzing how ritual functions to embed and convey survival competence. They observe the constraint's operation without direct participation or benefit.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, anthropologists_scholars, observer,
    analytical, generational, analytical, global).

% Advocate for purely propositional or scientific methods of knowledge transfer, dismissing ritual as superstitious or merely symbolic. They are excluded from the practice's internal logic and would object to its claims of functional efficacy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, rationalist_critics, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__hybrid_embedded_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__hybrid_embedded_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits complex, non-propositional survival knowledge and operational capacity across generations through embodied practice and symbolic fidelity, enabling collective response to recurring catastrophes.
% TRANSFER_FUNCTION: Moves embodied competence, collective memory, and practical skills, embedded within symbolic ritual forms, from past and present generations to future generations within a community.
% ABSENT_VOICES: Rationalist critics and those who view ritual as purely symbolic would object, arguing that survival competence should be transmitted through explicit, propositional instruction rather than 'embedded' practice. They are excluded by the very premise of the constraint.
% DISAPPEARANCE_RATIONALE: If the ritual and its embedded knowledge vanished, the community would lose its primary mechanism for transmitting critical survival competence. This would lead to a catastrophic loss of operational capacity, making future generations vulnerable to the very disasters the rituals were designed to mitigate. The social and cognitive structures of the community would fundamentally alter.
% FOUNDING_PROBLEM: How to reliably transmit complex, non-propositional survival knowledge and operational capacity across generations in contexts of recurring catastrophe, where explicit instruction is insufficient or easily lost.
% FOUNDING_PROBLEM_CORROBORATION: Ethnographic studies of communities in disaster-prone regions, historical accounts of resilience linked to ritual practice, and cognitive science research on embodied cognition and non-declarative memory provide corroboration from outside the immediate benefiting parties. These sources attest to the ongoing relevance of transmitting such knowledge.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_transmission__hybrid_embedded_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).
:- end_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because the constraint primarily facilitates coordination and knowledge transfer, with no identifiable party extracting rents. Suppression is low, as adherence to ritual is driven by perceived benefit and identity, not coercion. Theater ratio is very low, as the ritual is understood to be directly functional, not merely performative. Accessibility collapse is high because if this specific mode of embedded knowledge is lost, alternatives for transmitting such complex, non-propositional competence are difficult to find or reconstruct. Resistance is low because the practice is seen as vital for community survival.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community members, the constraint is a vital, beneficial Rope. From the perspective of rationalist critics, it might appear as a Piton or Snare, maintaining an 'irrational' practice through social inertia or identity lock-in, but this reading explicitly rejects that interpretation by asserting the functional efficacy of the embedded knowledge.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and future generations are beneficiaries, receiving the vital knowledge. Ritual elders, as guardians of fidelity, are also beneficiaries and agenda-setters, ensuring the practice's integrity. There are no direct victims, as the constraint's operation is seen as universally beneficial for the community's survival. Rationalist critics are excluded, as their epistemic framework does not recognize the constraint's core premise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing the ''hybrid embedded'' reading of the ''catastrophe_memory_transmission'' kernel?',
    'Expert review by scholars of ritual studies and collective memory, comparing this story''s structural claims against the established literature on the hybrid embedded perspective.',
    'If misidentified, the analysis of the kernel''s overall contestation would be skewed, potentially misrepresenting the structural relationships between different readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint as a specific reading of the catastrophe memory transmission kernel.').

omega_variable(
    form_function_separability,
    'To what extent are ritual form and operational function truly inseparable, or could the competence be transmitted through alternative, non-ritualistic means?',
    'Empirical studies comparing communities that maintain ritual fidelity with those that have abandoned it, assessing their differential resilience to catastrophe. Cognitive science research on the limits of propositional knowledge transfer for complex, embodied skills.',
    'If separable, the constraint''s ''mountain substrate'' aspect would weaken, and its classification might shift towards a more conventional Rope or even a Piton if the functional aspect atrophies while the form persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(form_function_separability, empirical, 'Examines the core claim of inseparability between ritual form and functional competence.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the low measured suppression primarily due to intrinsic belief in the ritual''s efficacy, or is there an internalized social pressure to conform that acts as a subtle form of suppression?',
    'Longitudinal ethnographic studies observing community members'' adherence to ritual in the absence of overt external pressure, and their subjective accounts of motivation. Analysis of social sanctions for non-participation.',
    'If internalized social pressure is a significant factor, the effective suppression for individual community members might be higher than the structural measure suggests, potentially shifting individual seats towards a Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for ritual fidelity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 40, 0.04).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 80, 0.06).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 60, 0.16).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 80, 0.15).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 20, 0.09).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 60, 0.11).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 80, 0.1).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
