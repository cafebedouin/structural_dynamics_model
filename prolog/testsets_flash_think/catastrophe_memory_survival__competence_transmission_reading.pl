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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: catastrophe_memory_survival__competence_transmission_reading
 *   human_readable: Ritual as Competence Transmission for Catastrophe Survival
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes the function of ritual in encoding and
 *   transmitting practical survival knowledge across generations,
 *   particularly in communities facing recurring catastrophes. It focuses on
 *   the 'competence transmission' reading of the broader
 *   'catastrophe_memory_survival' kernel. While providing vital coordination
 *   for survival, the process involves costs, including the effort of ritual
 *   maintenance and the risk of losing practical content over time, leading
 *   to an asymmetric outcome for different community members.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.3).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Ritual as Competence Transmission for Catastrophe Survival").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__competence_transmission_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, '8bab1afb-b277-45db-bad8-36108cd0e9b6').
narrative_ontology:cs_kernel_codification('8bab1afb-b277-45db-bad8-36108cd0e9b6', implicit).
narrative_ontology:cs_authority_grounding('8bab1afb-b277-45db-bad8-36108cd0e9b6', practice).
narrative_ontology:cs_interpretation_layer_present('8bab1afb-b277-45db-bad8-36108cd0e9b6').
narrative_ontology:cs_reading_relation('8bab1afb-b277-45db-bad8-36108cd0e9b6', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('8bab1afb-b277-45db-bad8-36108cd0e9b6', catastrophe_memory_survival__hybrid_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('8bab1afb-b277-45db-bad8-36108cd0e9b6', foundational, ritual_transmits_actionable_knowledge).
narrative_ontology:cs_axiom_status(ritual_transmits_actionable_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('8bab1afb-b277-45db-bad8-36108cd0e9b6', ritual_transmits_actionable_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('8bab1afb-b277-45db-bad8-36108cd0e9b6', secondary, ritual_form_without_content_is_vulnerable).
narrative_ontology:cs_axiom_status(ritual_form_without_content_is_vulnerable, holdable).
narrative_ontology:cs_axiom_grounding('8bab1afb-b277-45db-bad8-36108cd0e9b6', ritual_form_without_content_is_vulnerable, empirically_contingent).
narrative_ontology:cs_reference_frame('8bab1afb-b277-45db-bad8-36108cd0e9b6', functional_adaptive_ritual).
narrative_ontology:cs_drift_state('8bab1afb-b277-45db-bad8-36108cd0e9b6', contemporary_globalized_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8bab1afb-b277-45db-bad8-36108cd0e9b6', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, community_elders).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, communities_losing_content).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, younger_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, younger_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities rely on the ritual to transmit critical survival knowledge, enabling them to adapt to new environments and maintain resilience in the face of ongoing challenges. They gain adaptive capacity and social cohesion.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities, beneficiary,
    organized, generational, constrained, global).

% As custodians of the ritual and the knowledge it encodes, elders actively transmit the practices and narratives. Their identity is deeply intertwined with this role, and they bear the responsibility of ensuring accurate transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, community_elders, agenda_setter,
    powerful, generational, identity_locked, local).

% Participate in the rituals, investing time and effort to learn the embedded knowledge. They are beneficiaries of the survival competence but also bear the cost of adherence and the risk of receiving ritual form without its full practical content if transmission is imperfect.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, younger_generations, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__competence_transmission_reading, younger_generations, beneficiary).

% These communities continue to perform the rituals, maintaining the form and social cohesion, but have lost significant portions of the practical survival knowledge originally embedded within them. They pay the cost of ritual maintenance without receiving its full adaptive benefit.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, communities_losing_content, payer,
    powerless, generational, identity_locked, local).

% Study the rituals and their evolution, analyzing their function in transmitting knowledge and their role in community survival. They provide an external, analytical perspective on the constraint's operation and efficacy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, anthropologists_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits critical practical survival knowledge (e.g., timing for planting/harvesting, resource management, family protocols for crisis, adaptation strategies) across generations and contexts, enabling collective action and resilience in the face of recurring catastrophes.
% TRANSFER_FUNCTION: Moves actionable knowledge, social cohesion, and adaptive capacity from past experiences and community elders to younger generations, ensuring the community's long-term survival and well-being.
% ABSENT_VOICES: Individuals or groups who prioritize 'modern' or 'scientific' methods of knowledge transmission and might dismiss ritual as superstitious or inefficient, thereby missing the embedded practical content and its social function. They would advocate for direct, explicit education over ritualized practice.
% DISAPPEARANCE_RATIONALE: If this ritual-based knowledge transmission vanished, communities would lose a vital, culturally embedded mechanism for collective learning and adaptation. This would lead to reduced resilience, increased vulnerability to environmental and social shocks, and potentially the collapse of traditional survival strategies, forcing a rapid and often painful reorganization around new, less integrated knowledge systems.
% FOUNDING_PROBLEM: Ensuring the survival and adaptive capacity of communities in the face of recurring environmental catastrophes, resource scarcity, and social upheavals, particularly in contexts where formal written records or scientific institutions were absent or insufficient.
% FOUNDING_PROBLEM_CORROBORATION: Ethnographic studies of indigenous communities, historical analyses of pre-modern societies, and ecological research on long-term human-environment interactions consistently corroborate the role of ritualized knowledge in survival. Independent scholars and community members (outside the immediate beneficiaries of the ritual's maintenance) attest to its ongoing relevance for adaptive capacity.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The base extractiveness (0.45) reflects the effort required for ritual performance and learning, as well as the potential for content loss for some participants. Suppression (0.3) is moderate, driven by social pressure and the perceived necessity for collective survival, rather than overt coercion. The theater ratio (0.4) acknowledges the performative aspect of ritual, but its functional core in transmitting knowledge keeps it from being purely theatrical. The increasing extractiveness and theater ratio over the interval suggest a gradual erosion of practical content, leading to more effort for diminishing returns, or a shift towards form over function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of elders and thriving diaspora communities, the ritual is a vital rope, ensuring survival and cultural continuity. However, for communities where the practical content has atrophied, or for younger generations struggling to connect form to function, it can feel more extractive, demanding adherence without clear benefit. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities and community elders are primary beneficiaries, gaining adaptive capacity and maintaining their social role, respectively. Younger generations are beneficiaries of the knowledge but also payers, bearing the cost of learning and the risk of content degradation. Communities that maintain ritual form but lose practical content are victims, paying the cost without full benefit. The social enforcement of ritual adherence ensures the coordination function, but also contributes to the 'tangled' aspect by imposing costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    practical_vs_symbolic_content,
    'What proportion of the ritual''s content is genuinely practical survival knowledge versus purely symbolic or identity-affirming?',
    'Detailed ethnographic analysis, historical reconstruction of crisis responses, and empirical testing of transmitted ''knowledge'' against ecological realities. If the ''knowledge'' consistently fails empirical tests, its practical component is low.',
    'If the practical component is low, the extractiveness for communities losing content is higher, and the constraint leans more towards a Snare (pure extraction of effort for symbolic return) or Piton (atrophied function). If high, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_vs_symbolic_content, empirical, 'Ambiguity in the functional vs. symbolic nature of ritual content.').

omega_variable(
    content_loss_measurement,
    'How accurately can the ''loss of practical content'' be measured and attributed to the ritual''s transmission mechanism versus external factors (e.g., environmental change making old knowledge obsolete)?',
    'Longitudinal studies comparing communities with varying degrees of ritual adherence and external pressures, alongside detailed content analysis of ritual narratives over time. Requires isolating the effect of transmission fidelity from knowledge obsolescence.',
    'If content loss is primarily due to transmission failure, the ''victim'' status of communities losing content is strongly supported. If due to obsolescence, the constraint might be a Rope that simply needs updating, rather than one with inherent extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(content_loss_measurement, empirical, 'Challenges in measuring and attributing the loss of practical knowledge within ritual.').

omega_variable(
    reading_framing_choice,
    'Is the ''competence_transmission_reading'' the most appropriate framing, or does the ''hybrid_encoding_reading'' (which includes symbolic function) better capture the constraint''s operation?',
    'Analysis of community self-description and observed adaptive outcomes: if communities explicitly prioritize practical outcomes and show direct correlation between ritual adherence and survival, this reading is strengthened. If symbolic cohesion is equally or more critical, the hybrid reading gains ground.',
    'Adopting the hybrid reading would likely lower the perceived extractiveness (as symbolic benefits would offset some costs) and shift the classification towards a more balanced Tangled Rope or even Rope, acknowledging a broader coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_choice, conceptual, 'Under-determination of the primary function of ritual (practical vs. hybrid).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 10, 0.27).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__competence_transmission_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
