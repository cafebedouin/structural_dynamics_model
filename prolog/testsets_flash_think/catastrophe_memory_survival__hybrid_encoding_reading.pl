% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__hybrid_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__hybrid_encoding_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Catastrophe Memory Survival: Hybrid Encoding Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes ritual practice as a hybrid encoding mechanism
 *   for catastrophe memory, simultaneously serving symbolic
 *   boundary-maintenance and transmitting practical survival knowledge. The
 *   constraint's efficacy and persistence depend on maintaining both
 *   registers without theoretical reduction. This reading posits that the
 *   constraint itself is beneficial and low-extraction; 'victims' are those
 *   whose analytical frameworks fail to grasp its integrated nature.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.1).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Catastrophe Memory Survival: Hybrid Encoding Reading").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, 'cfae352c-6a37-4221-bb01-91b168431a63').
narrative_ontology:cs_kernel_codification('cfae352c-6a37-4221-bb01-91b168431a63', implicit).
narrative_ontology:cs_authority_grounding('cfae352c-6a37-4221-bb01-91b168431a63', practice).
narrative_ontology:cs_interpretation_layer_present('cfae352c-6a37-4221-bb01-91b168431a63').
narrative_ontology:cs_reading_relation('cfae352c-6a37-4221-bb01-91b168431a63', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('cfae352c-6a37-4221-bb01-91b168431a63', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_axiom('cfae352c-6a37-4221-bb01-91b168431a63', foundational, symbolic_and_practical_are_intertwined).
narrative_ontology:cs_axiom_status(symbolic_and_practical_are_intertwined, holdable).
narrative_ontology:cs_axiom_grounding('cfae352c-6a37-4221-bb01-91b168431a63', symbolic_and_practical_are_intertwined, conventional).
narrative_ontology:cs_axiom('cfae352c-6a37-4221-bb01-91b168431a63', secondary, holistic_transmission_is_key_to_survival).
narrative_ontology:cs_axiom_status(holistic_transmission_is_key_to_survival, holdable).
narrative_ontology:cs_axiom_grounding('cfae352c-6a37-4221-bb01-91b168431a63', holistic_transmission_is_key_to_survival, instrumental).
narrative_ontology:cs_reference_frame('cfae352c-6a37-4221-bb01-91b168431a63', integrated_ritual_practice).
narrative_ontology:cs_drift_state('cfae352c-6a37-4221-bb01-91b168431a63', contemporary_analytical_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('cfae352c-6a37-4221-bb01-91b168431a63', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_survivor_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, reductionist_analysts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities actively maintain and transmit rituals that simultaneously preserve collective identity and convey practical survival knowledge. Their survival and cohesion depend on this integrated approach, making them the primary beneficiaries and stewards of the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_survivor_communities, agenda_setter,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_survivor_communities, beneficiary).

% Academics or external observers who attempt to categorize ritual practice into purely symbolic or purely practical functions. Their analytical frameworks often fail to capture the integrated efficacy of the hybrid encoding, leading to incomplete or misleading interpretations. They 'pay' the cost of this analytical inadequacy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, reductionist_analysts, payer,
    analytical, biographical, analytical, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__hybrid_encoding_reading, reductionist_analysts, excluded).

% Individuals within the community who perform and embody the rituals. They gain social status, meaning, and a sense of continuity through their participation, and are instrumental in the intergenerational transmission of the hybrid knowledge.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practitioners, beneficiary,
    moderate, biographical, constrained, local).

% Researchers or interested parties who study the ritual practices without attempting to impose reductive frameworks. They observe the integrated function but are not directly involved in its maintenance or subject to its internal dynamics.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, external_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__hybrid_encoding_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__hybrid_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and adaptive behavior in the face of past catastrophe by simultaneously reinforcing group identity through symbolic acts and transmitting practical knowledge for future survival, ensuring community cohesion and resilience.
% TRANSFER_FUNCTION: Transfers collective identity, shared meaning, and practical survival strategies (e.g., resource management, social protocols, adaptation techniques) across generations within the community, from elders and practitioners to younger members.
% ABSENT_VOICES: Those who would advocate for a purely rational, de-ritualized approach to survival knowledge, or a purely aesthetic/symbolic approach to ritual, are structurally absent from the community's internal discourse on ritual efficacy. Their perspectives are not integrated into the practice's maintenance.
% DISAPPEARANCE_RATIONALE: If the hybrid encoding of catastrophe memory vanished overnight, communities would lose a critical, integrated mechanism for intergenerational transmission of both identity and practical survival knowledge. This would lead to cultural fragmentation, reduced social cohesion, and diminished resilience to future challenges, forcing a reorganization of how collective memory and adaptive strategies are maintained.
% FOUNDING_PROBLEM: The core problem was how to ensure the long-term survival and cohesion of a community after a catastrophic event, by transmitting both the traumatic memory of the event and the adaptive strategies developed in response, across generations, without losing either aspect.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of post-catastrophe communities, historical records of cultural resilience, and the communities' own narratives consistently attest to the ongoing need for such integrated mechanisms. Independent analyses from outside the benefiting parties corroborate that the problem of intergenerational survival and identity maintenance remains pertinent.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_survival__hybrid_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).
:- end_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.10) reflect that this hybrid encoding is a functional, community-driven practice that benefits its participants. It is not imposed coercively, nor does it primarily extract rents. The low theater ratio (0.05) indicates genuine functionality in both symbolic and practical dimensions. Accessibility collapse is moderate (0.45) because while the practice is deeply embedded, external analytical alternatives (though incomplete) exist. Resistance is low (0.20) from within the community, but higher from external analytical perspectives that struggle with its complexity.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap exists between the internal experience of the catastrophe survivor communities and the external analytical frameworks. The communities experience the ritual as a cohesive, functional whole, where symbolic and practical elements are inseparable. External analysts, particularly those seeking to reduce phenomena to discrete categories, struggle to reconcile this integrated function, leading to a 'victimization' of their analytical models rather than of the people themselves.
 *
 * DIRECTIONALITY LOGIC:
 *   Catastrophe survivor communities are the primary beneficiaries and agenda-setters (d near 0.0) as they actively maintain and thrive through this integrated ritual practice. Reductionist analysts are identified as 'victims' (d near 1.0) not because the constraint extracts from them, but because their attempts to force binary classifications on the ritual lead to analytical failure and an inability to fully comprehend its efficacy. Ritual practitioners are also beneficiaries, gaining meaning and social role. External observers are neutral analytical seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate — ensuring community survival and cohesion after catastrophe — remains critically live. The hybrid encoding continues to serve its dual function effectively, preventing any drift towards mandatrophy. The ongoing relevance of the founding problem (how to survive and transmit memory) means the constraint is far from an atrophied or theatrical performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''hybrid_encoding_reading'' of the ''catastrophe_memory_survival'' kernel?',
    'Comparison with other readings of the same kernel, ensuring the unique emphasis on the integrated symbolic and practical registers is maintained without conflation.',
    'Misidentification could lead to incorrect classification, either overstating extraction (if conflated with a purely symbolic reading) or understating coordination (if conflated with a purely practical reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being instantiated from the kernel.').

omega_variable(
    sibling_reading_impact_symbol_survival,
    'If the ''symbol_survival_reading'' were prioritized, would the practical knowledge aspect of the ritual be diminished or lost?',
    'Ethnographic studies of communities where ritual has become primarily symbolic, examining the long-term impact on practical knowledge transmission and community resilience.',
    'If practical knowledge is diminished, the overall efficacy of the constraint for survival would decrease, potentially shifting its classification towards a more theatrical or less functional type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_impact_symbol_survival, empirical, 'Impact of prioritizing symbolic over practical aspects.').

omega_variable(
    sibling_reading_impact_competence_transmission,
    'If the ''competence_transmission_reading'' were prioritized, would the symbolic identity and boundary-maintenance aspects of the ritual be diminished or lost?',
    'Sociological analysis of communities where practical knowledge transmission has been secularized or de-ritualized, observing effects on collective identity and social cohesion.',
    'If symbolic identity is diminished, the community''s cohesion and ability to maintain its distinctiveness could suffer, potentially leading to fragmentation or assimilation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_impact_competence_transmission, empirical, 'Impact of prioritizing practical over symbolic aspects.').

omega_variable(
    analytical_reductionism_impact,
    'Is the ''victimization'' of reductionist analysts an inherent property of the constraint''s complexity, or a consequence of their chosen methodological limitations?',
    'Comparative analysis of different analytical approaches to complex cultural phenomena, assessing which frameworks successfully integrate dual registers versus those that fail.',
    'If primarily due to methodological limitations, it highlights the need for more sophisticated analytical tools rather than an inherent ''extractive'' quality of the constraint itself. If inherent, it underscores the constraint''s unique structural resistance to simplification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(analytical_reductionism_impact, conceptual, 'Source of analytical ''victimization'' by the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 20, 0.04).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 30, 0.16).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 20, 0.11).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 30, 0.1).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 50, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, collective_identity_formation).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, intergenerational_knowledge_transfer).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_memory_survival' kernel. This 'hybrid_encoding_reading' emphasizes the integrated symbolic and practical functions of ritual, distinct from readings that prioritize one over the other. All three readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
