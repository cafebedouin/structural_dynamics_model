% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__competence_transmission_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__competence_transmission_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__competence_transmission_reading
 *   human_readable: Ritual as Competence Transmission for Catastrophe Survival
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes ritual as a mechanism for encoding and
 *   transmitting practical survival knowledge, such as timing for resource
 *   management, family protocols, and adaptation strategies. It is one
 *   reading of the broader 'catastrophe_memory_survival' kernel. This reading
 *   emphasizes the functional, instrumental aspect of ritual, where its value
 *   lies in the concrete, actionable information it conveys for collective
 *   resilience. The constraint is claimed as a Rope, reflecting its genuine
 *   coordination function, but with a moderate extractiveness as communities
 *   may lose the explicit practical content while maintaining the ritual
 *   form, thus paying a cost in lost adaptive capacity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.2).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Ritual as Competence Transmission for Catastrophe Survival").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, 'd8a30acc-e78b-4a4b-86f6-546ff286c8d7').
narrative_ontology:cs_kernel_codification('d8a30acc-e78b-4a4b-86f6-546ff286c8d7', implicit).
narrative_ontology:cs_authority_grounding('d8a30acc-e78b-4a4b-86f6-546ff286c8d7', practice).
narrative_ontology:cs_interpretation_layer_present('d8a30acc-e78b-4a4b-86f6-546ff286c8d7').
narrative_ontology:cs_reading_relation('d8a30acc-e78b-4a4b-86f6-546ff286c8d7', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8a30acc-e78b-4a4b-86f6-546ff286c8d7', catastrophe_memory_survival__hybrid_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('d8a30acc-e78b-4a4b-86f6-546ff286c8d7', foundational, ritual_as_instrumental_knowledge_transfer).
narrative_ontology:cs_axiom_status(ritual_as_instrumental_knowledge_transfer, holdable).
narrative_ontology:cs_axiom_grounding('d8a30acc-e78b-4a4b-86f6-546ff286c8d7', ritual_as_instrumental_knowledge_transfer, empirically_contingent).
narrative_ontology:cs_reference_frame('d8a30acc-e78b-4a4b-86f6-546ff286c8d7', functional_adaptive_transmission).
narrative_ontology:cs_drift_state('d8a30acc-e78b-4a4b-86f6-546ff286c8d7', contemporary_secularization_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('d8a30acc-e78b-4a4b-86f6-546ff286c8d7', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, future_generations).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, communities_losing_practical_content).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities actively use ritual to transmit and preserve practical knowledge for survival and adaptation in new environments, gaining adaptive capacity. They benefit from the embedded strategies for resource management and social cohesion.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities, beneficiary,
    organized, generational, constrained, regional).

% These generations inherit the practical knowledge encoded in ritual, which can be crucial for their survival and flourishing, especially in the face of future environmental or social catastrophes. Their benefit is latent until needed.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% These communities maintain the form of ritual but have lost the explicit understanding of its practical survival content, treating it as purely symbolic. They bear the cost of reduced adaptive capacity and potential vulnerability in crises, without realizing the original function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, communities_losing_practical_content, payer,
    moderate, biographical, constrained, local).

% Individuals who perform and teach the rituals. They are the primary agents of transmission, often deeply invested in the practice as part of their identity. Their role is to ensure fidelity of transmission, whether or not they fully grasp the practical content.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, ritual_practitioners, agenda_setter,
    moderate, biographical, identity_locked, local).

% Scholars who study the origins and functions of ritual, often uncovering the latent practical knowledge embedded within practices that contemporary practitioners may no longer recognize. They analyze the constraint's effectiveness and evolution.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, anthropologists_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action and resource management strategies across generations and within communities, ensuring the transmission of vital survival knowledge in a memorable and resilient format.
% TRANSFER_FUNCTION: Transfers practical knowledge, adaptive strategies, and social cohesion mechanisms from past generations to present and future ones, enabling collective survival and resilience.
% ABSENT_VOICES: Future generations who might face novel catastrophes would ideally provide feedback on the utility and adaptability of the transmitted knowledge, but their voices are absent by definition. Communities that have lost the practical content of their rituals are also 'absent' in their understanding of the constraint's full function.
% DISAPPEARANCE_RATIONALE: If ritual's capacity to transmit practical survival knowledge vanished, communities would lose a critical, resilient mechanism for intergenerational learning and adaptation. In the face of environmental or social shocks, the ability to coordinate and apply time-tested strategies would be severely diminished, leading to increased vulnerability and potential collapse.
% FOUNDING_PROBLEM: How to reliably transmit complex, context-specific survival knowledge across generations, especially through periods of social upheaval, migration, or environmental catastrophe, without relying solely on written records or direct instruction.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of indigenous communities, historical accounts of diaspora groups, and analyses of post-disaster resilience all corroborate that the problem of transmitting adaptive knowledge remains live, and ritual often plays a key role. This is attested by independent academic research, not just by practitioners.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_survival__competence_transmission_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__competence_transmission_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__competence_transmission_reading_tests).
:- end_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while the knowledge is transmitted, its explicit practical meaning can be lost over time, turning functional content into mere form for some communities. Suppression is low (0.2) as the constraint relies on cultural transmission and social cohesion rather than active coercion. Theater ratio is low (0.1) because the primary function is genuine knowledge transfer, even if the explicit understanding of that function varies. Accessibility collapse is moderate (0.3) as alternative knowledge transmission methods exist but may lack the resilience and memorability of ritual. Resistance is low (0.15) because the constraint is largely internalized and culturally embedded.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of communities actively using ritual for survival, it is a clear Rope. From communities that have lost the practical content, it might appear as a Piton or a less effective Rope, as they perform the ritual without fully realizing its instrumental value. The engine's classification will reflect this divergence based on the specific stakeholder's situation.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities and future generations are beneficiaries, gaining adaptive capacity and survival knowledge. Communities that lose the practical content, while maintaining ritual form, are victims, as they bear the cost of reduced resilience. Ritual practitioners act as agenda-setters, facilitating transmission. Anthropologists and historians are observers, analyzing the constraint's function.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    explicit_vs_implicit_knowledge,
    'To what extent is the ''practical survival knowledge'' explicitly understood by practitioners versus implicitly embedded in the ritual''s structure and actions?',
    'Ethnographic studies and cognitive analyses of ritual performance, comparing explicit practitioner accounts with observed behavioral outcomes and historical records of efficacy.',
    'If knowledge is largely implicit, the constraint''s resilience is higher (less prone to conscious alteration), but its transferability to novel contexts is lower. If explicit, it''s more adaptable but also more vulnerable to intentional distortion or loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(explicit_vs_implicit_knowledge, empirical, 'Ambiguity in the mode of knowledge encoding and transmission.').

omega_variable(
    loss_of_content_detection,
    'How can the ''loss of practical content'' in communities be reliably detected and measured, given that ritual form may persist?',
    'Comparative studies of communities facing similar environmental challenges, where some maintain the practical content of their rituals and others do not, assessing differential survival and adaptation rates.',
    'Clear detection methods would strengthen the victim declaration and the extractiveness metric for communities losing content, potentially shifting the classification towards a Tangled Rope for those specific seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(loss_of_content_detection, empirical, 'Difficulty in measuring the degradation of practical knowledge within persistent ritual forms.').

omega_variable(
    reading_framing_ambiguity,
    'Is this constraint best framed as primarily transmitting ''competence'' (this reading), or is its primary function ''symbolic boundary-maintenance'' (symbol_survival_reading), or a ''hybrid'' of both (hybrid_encoding_reading)?',
    'Longitudinal studies tracking community resilience in response to crises, correlating survival outcomes with the explicit practical content vs. symbolic fidelity of their rituals. This would empirically weigh the relative contribution of each function.',
    'If the ''symbol_survival_reading'' or ''hybrid_encoding_reading'' were adopted, the extractiveness and beneficiary/victim structure would shift, as the ''cost'' of losing practical content might be offset by gains in identity cohesion or vice-versa. This would lead to a different classification for the overall kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Under-determination of the primary function of ritual in catastrophe memory and survival.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 25, 0.35).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 50, 0.4).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 75, 0.43).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 25, 0.17).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 50, 0.18).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 75, 0.19).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__competence_transmission_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_memory_survival' kernel. This reading emphasizes the transmission of practical competence, while sibling readings focus on symbolic survival or a hybrid of both. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
