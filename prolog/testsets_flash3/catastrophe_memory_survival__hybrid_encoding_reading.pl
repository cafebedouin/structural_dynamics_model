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
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Catastrophe Memory Survival: Hybrid Encoding Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes the 'hybrid encoding' reading of how
 *   catastrophe memory survives through ritual. It posits that ritual
 *   functions simultaneously on symbolic and practical registers, with both
 *   being crucial for community survival. The constraint's low extractiveness
 *   reflects that this dual function primarily benefits the survivor
 *   communities, while 'victims' are analytical frameworks that fail to grasp
 *   this complexity. The claimed type is 'rope' because it genuinely
 *   coordinates complex intergenerational transmission with minimal coercive
 *   overhead.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.05).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Catastrophe Memory Survival: Hybrid Encoding Reading").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, '7c1d03a0-8173-4711-b504-450f04c5f72e').
narrative_ontology:cs_kernel_codification('7c1d03a0-8173-4711-b504-450f04c5f72e', implicit).
narrative_ontology:cs_authority_grounding('7c1d03a0-8173-4711-b504-450f04c5f72e', practice).
narrative_ontology:cs_interpretation_layer_present('7c1d03a0-8173-4711-b504-450f04c5f72e').
narrative_ontology:cs_reading_relation('7c1d03a0-8173-4711-b504-450f04c5f72e', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c1d03a0-8173-4711-b504-450f04c5f72e', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_axiom('7c1d03a0-8173-4711-b504-450f04c5f72e', foundational, ritual_dual_register_essential).
narrative_ontology:cs_axiom_status(ritual_dual_register_essential, holdable).
narrative_ontology:cs_axiom_grounding('7c1d03a0-8173-4711-b504-450f04c5f72e', ritual_dual_register_essential, empirically_contingent).
narrative_ontology:cs_reference_frame('7c1d03a0-8173-4711-b504-450f04c5f72e', integrated_survival_practice).
narrative_ontology:cs_drift_state('7c1d03a0-8173-4711-b504-450f04c5f72e', contemporary_analytical_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('7c1d03a0-8173-4711-b504-450f04c5f72e', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, survivor_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, reductionist_analysts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities maintain rituals that encode both symbolic meaning and practical knowledge, enabling their long-term survival and cultural continuity after catastrophic events. The dual function is essential for their resilience.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, survivor_communities, beneficiary,
    organized, generational, identity_locked, local).

% Academics or researchers who attempt to force a binary classification of ritual (either purely symbolic or purely practical) find their models fail to capture the full adaptive capacity of these communities. They pay in explanatory power and theoretical coherence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, reductionist_analysts, payer,
    analytical, biographical, constrained, global).

% Individuals who actively perform and transmit the rituals. They embody and pass on the hybrid encoding, often without explicit theoretical awareness of its dual nature, ensuring its continuity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practitioners, agenda_setter,
    moderate, biographical, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transmission of complex, multi-layered cultural memory and survival strategies across generations, ensuring both identity continuity and practical adaptive capacity in the face of past and potential future catastrophes.
% TRANSFER_FUNCTION: Transfers both symbolic meaning (identity, belonging, boundary-maintenance) and practical, embodied knowledge (how to cope, when to act, what to remember) from older to younger generations within survivor communities.
% ABSENT_VOICES: Analysts who insist on a purely symbolic or purely practical reading of ritual are excluded from fully understanding the adaptive success of these communities; their frameworks cannot account for the observed resilience.
% DISAPPEARANCE_RATIONALE: If the hybrid encoding of catastrophe memory vanished, survivor communities would lose a critical mechanism for intergenerational transmission of both identity and practical resilience, leading to cultural fragmentation and reduced adaptive capacity.
% FOUNDING_PROBLEM: How to ensure the long-term survival and cultural continuity of communities after catastrophic events, by transmitting both the symbolic meaning of their experience and the practical knowledge needed to adapt.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of post-catastrophe communities, historical accounts of cultural resilience, and the lived experience of survivor communities themselves corroborate the ongoing need for this hybrid encoding. This is attested by community elders and independent ethnographers, not just the ritual practitioners.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is low (0.15) because the primary 'cost' is borne by analytical frameworks that struggle to integrate the dual nature of ritual, rather than by the communities themselves. Suppression is negligible (0.05) as the constraint is maintained by cultural transmission and intrinsic value, not coercion. Theater ratio is low (0.1) because the rituals are highly functional in both symbolic and practical terms. Accessibility collapse is high (0.8) because once the hybrid nature is understood, simpler, reductionist alternatives are seen as inadequate. Resistance is low (0.1) because the communities themselves largely embrace this integrated approach.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the lived experience of survivor communities, who implicitly understand and benefit from the hybrid encoding, and external analytical observers who may struggle to reconcile the symbolic and practical dimensions within a single theoretical framework. The communities experience it as a vital, low-cost coordination mechanism, while some analysts experience a 'cost' in the form of theoretical inadequacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Survivor communities are the primary beneficiaries, as the hybrid encoding directly supports their resilience and cultural continuity. Reductionist analysts are 'victims' in the sense that their theoretical frameworks are challenged and found wanting by the observed efficacy of these rituals. Ritual practitioners act as agenda-setters by embodying and transmitting the practices.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_practical_primacy,
    'In specific contexts, does the symbolic or practical register of ritual take primacy for survival, or is their balance always equally critical?',
    'Comparative ethnographic studies across diverse post-catastrophe communities, analyzing cases where one register appears to be emphasized over the other, and correlating with long-term survival outcomes.',
    'If one register consistently takes primacy in certain contexts, it might suggest a more nuanced ''tangled rope'' or ''scaffold'' dynamic where one function is temporarily or contextually more extractive or supportive than the other. If always equally critical, the ''rope'' classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_practical_primacy, empirical, 'Examines the relative importance and potential for imbalance between symbolic and practical functions of ritual in catastrophe memory.').

omega_variable(
    analytical_framing_impact,
    'To what extent does the ''victim'' status of reductionist analysts stem from an inherent structural property of the constraint versus their chosen analytical framework?',
    'Development and adoption of new analytical frameworks that successfully integrate both symbolic and practical dimensions of ritual. If such frameworks emerge and gain traction, the ''victim'' status would diminish.',
    'If the ''victim'' status is primarily due to chosen frameworks, the constraint''s ''extractiveness'' from analysts is conceptual, not structural. If it''s an inherent property of the phenomenon, the ''victim'' status is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(analytical_framing_impact, conceptual, 'Assesses whether the ''cost'' to analysts is due to the ritual''s nature or the limitations of current analytical tools.').

omega_variable(
    kernel_reading_identity,
    'Is this ''hybrid_encoding_reading'' truly distinct from its sibling readings, or does it represent an integration that could be subsumed by a more comprehensive ''competence_transmission_reading'' or ''symbol_survival_reading''?',
    'Further theoretical development and empirical testing of the core axioms of each reading. If the hybridity axiom proves to be a necessary, irreducible component for explanatory power, its distinct identity is confirmed.',
    'If subsumed, this constraint would be reclassified as a ''component'' or ''aspect'' of a sibling constraint, potentially altering its extractiveness and classification depending on the dominant sibling''s profile. If distinct, its current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Clarifies the distinctiveness of the hybrid encoding reading within the catastrophe memory kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 30, 0.16).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 30, 0.05).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 40, 0.05).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 50, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_memory_survival' kernel, focusing on the hybrid encoding of symbolic and practical knowledge. It is linked to its sibling readings via the cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
