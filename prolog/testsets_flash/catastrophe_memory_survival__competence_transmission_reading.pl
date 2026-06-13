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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   This constraint models ritual as a mechanism for encoding and
 *   transmitting practical survival knowledge across generations,
 *   particularly in communities facing recurring catastrophes. It focuses on
 *   the direct utility of ritual content (e.g., timing for planting, resource
 *   management protocols, family-level adaptation strategies) rather than its
 *   symbolic or identity-preserving functions. The constraint ensures the
 *   continuity of adaptive capacity, but can become extractive if the
 *   practical content is lost while the ritual form is maintained, leading to
 *   communities performing rituals without understanding their original
 *   survival purpose.
 *
 * KEY AGENTS:
 *   - diaspora_communities: Primary beneficiary (institutional/generational) — gains adaptive capacity
 *   - communities_losing_practical_content: Primary victim (organized/generational) — bears costs of maintaining form without content
 *   - ritual_practitioners: Agenda setter (moderate/biographical) — transmits and interprets ritual
 *   - anthropologists_and_historians: Observer (analytical/civilizational) — analyzes the function and evolution of ritual
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.2).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Ritual as Competence Transmission for Catastrophe Survival").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, '155bf018-c530-4c25-a06b-4d62f1deb35e').
narrative_ontology:cs_kernel_codification('155bf018-c530-4c25-a06b-4d62f1deb35e', implicit).
narrative_ontology:cs_authority_grounding('155bf018-c530-4c25-a06b-4d62f1deb35e', practice).
narrative_ontology:cs_interpretation_layer_present('155bf018-c530-4c25-a06b-4d62f1deb35e').
narrative_ontology:cs_reading_relation('155bf018-c530-4c25-a06b-4d62f1deb35e', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('155bf018-c530-4c25-a06b-4d62f1deb35e', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('155bf018-c530-4c25-a06b-4d62f1deb35e', foundational, ritual_as_adaptive_algorithm).
narrative_ontology:cs_axiom_status(ritual_as_adaptive_algorithm, holdable).
narrative_ontology:cs_axiom_grounding('155bf018-c530-4c25-a06b-4d62f1deb35e', ritual_as_adaptive_algorithm, empirically_contingent).
narrative_ontology:cs_axiom('155bf018-c530-4c25-a06b-4d62f1deb35e', secondary, survival_knowledge_is_encoded).
narrative_ontology:cs_axiom_status(survival_knowledge_is_encoded, holdable).
narrative_ontology:cs_axiom_grounding('155bf018-c530-4c25-a06b-4d62f1deb35e', survival_knowledge_is_encoded, empirically_contingent).
narrative_ontology:cs_reference_frame('155bf018-c530-4c25-a06b-4d62f1deb35e', ancestral_adaptive_practice).
narrative_ontology:cs_drift_state('155bf018-c530-4c25-a06b-4d62f1deb35e', contemporary_globalized_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('155bf018-c530-4c25-a06b-4d62f1deb35e', '').
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

% These communities rely on the ritual to transmit critical survival knowledge from their ancestral lands, enabling adaptation to new environments or preserving resilience against future threats. They actively seek out and maintain these practices.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities, beneficiary,
    institutional, generational, constrained, global).

% These communities continue to perform the rituals, often due to cultural inertia or identity preservation, but have lost the explicit understanding of the practical survival knowledge originally embedded within them. They bear the cost of maintenance without receiving the full original benefit.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, communities_losing_practical_content, payer,
    organized, generational, identity_locked, local).

% Individuals or groups responsible for learning, performing, and transmitting the rituals. They act as custodians of the knowledge, often interpreting it for contemporary contexts, but may also contribute to the loss of practical content if their understanding shifts.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, ritual_practitioners, agenda_setter,
    moderate, biographical, constrained, local).

% Academic researchers who study the origins, evolution, and functions of such rituals. They analyze the content and context, often identifying the practical knowledge that has been lost or transformed, and provide external corroboration for the constraint's function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, anthropologists_and_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intergenerational transmission of complex, context-specific survival knowledge and adaptive strategies within communities, ensuring collective resilience against recurring catastrophic events.
% TRANSFER_FUNCTION: Transfers practical knowledge (e.g., timing for resource harvesting, disaster preparedness protocols, social cohesion strategies) from past generations to present and future ones, often through embodied practice and narrative, at the cost of adherence to ritual forms.
% ABSENT_VOICES: Future generations who might benefit from more explicit or updated knowledge, but are bound by the forms transmitted. Also, communities that have abandoned such rituals due to perceived irrelevance, who might argue for more direct, less ritualized forms of knowledge transfer.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, communities would lose a vital, culturally embedded mechanism for transmitting survival knowledge. This would likely lead to a significant decrease in adaptive capacity, increased vulnerability to recurring catastrophes, and a fragmentation of collective memory regarding past challenges and solutions. Communities would have to re-learn or re-discover critical survival strategies, often at great cost.
% FOUNDING_PROBLEM: The recurring threat of catastrophic events (e.g., floods, droughts, famines, conflicts) that necessitated the encoding and reliable transmission of complex, context-specific survival knowledge across generations to ensure community persistence.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of indigenous communities, historical records of disaster resilience, and contemporary analyses of climate change adaptation all corroborate that the problem of transmitting survival knowledge remains live, especially for vulnerable populations. Ritual practitioners and diaspora communities also attest to its ongoing relevance, often citing specific examples of knowledge applied in crises.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_survival__competence_transmission_reading, 'none', 1).

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
 *   The extractiveness (0.45) is moderate because while the knowledge is valuable, its transmission often involves adherence to forms that may not be immediately transparent or efficient, and some communities may lose the practical content while maintaining the ritual. Suppression (0.2) is low, as adherence is largely cultural and voluntary, though social pressure exists. Theater ratio (0.3) is moderate and rising, reflecting a tendency for ritual form to persist even as its practical content atrophies or becomes less relevant to current conditions. Accessibility collapse (0.3) is low because alternative ways of transmitting knowledge exist, but ritual offers a unique, robust channel. Resistance (0.1) is low as the constraint is generally seen as beneficial or culturally essential.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of diaspora communities, the ritual is a clear Rope, providing vital adaptive knowledge. For communities that have lost the practical content but maintain the form, it can feel like a mild Snare or Piton, demanding effort without clear benefit. Ritual practitioners often see themselves as preserving essential knowledge, while external observers might note the drift towards theatricality.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities are beneficiaries (d=0.0-0.2) as they gain crucial survival knowledge. Communities losing practical content are victims (d=0.7-0.9) as they expend resources on rituals whose original utility is no longer understood. Ritual practitioners are closer to symmetric (d=0.4-0.6), balancing the effort of transmission with the cultural and social capital gained. The low suppression and voluntary nature of adherence keep directionality from reaching extreme values.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling genuine knowledge transmission as pure extraction. The moderate extractiveness and rising theater ratio indicate a potential for mandatrophy, where the original mandate (survival knowledge) atrophies, but the ritual persists due to inertia or symbolic value. The 'competence_transmission_reading' specifically highlights the functional aspect, allowing for detection of its decay into a more extractive or performative state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint primarily about competence transmission, or is it a hybrid encoding or purely symbolic survival mechanism?',
    'Empirical studies tracing the direct impact of ritual practice on adaptive capacity and resource management in communities facing recurring environmental or social stressors.',
    'If primarily competence transmission, the constraint functions as a Rope for beneficiaries (adaptive capacity) and a mild Snare for those who lose the practical content. If hybrid or symbolic, the classification would shift to Tangled Rope or Piton, reflecting different extraction mechanisms or atrophy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''competence_transmission_reading'' of the ''catastrophe_memory_survival'' kernel. Sibling readings (''symbol_survival_reading'', ''hybrid_encoding_reading'') would emphasize identity preservation or dual functionality, leading to different classifications.').

omega_variable(
    practical_content_decay,
    'To what extent does the practical survival knowledge embedded in ritual decay or become unintelligible over time, even if the ritual form persists?',
    'Longitudinal ethnographic studies comparing ritual content and community adaptive outcomes across generations, particularly in contexts where the original catastrophic conditions have changed or been forgotten.',
    'If practical content decays significantly, the constraint''s function shifts from competence transmission to mere symbolic performance, increasing its theater_ratio and potentially reclassifying it as a Piton or a Snare if the form is maintained coercively.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_content_decay, empirical, 'Assesses the rate at which the practical knowledge within ritual is lost, even if the ritual itself continues to be performed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 10, 0.17).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 20, 0.19).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 30, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__competence_transmission_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__competence_transmission_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_survival' kernel, focusing on the transmission of practical competence. It is linked to sibling readings that emphasize symbolic survival and hybrid encoding, as they all address different facets of how communities cope with and remember catastrophic events through ritual.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
