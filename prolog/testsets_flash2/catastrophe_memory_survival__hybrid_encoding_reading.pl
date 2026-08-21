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
 *   being essential for community survival. The constraint itself is the
 *   resistance to separating these registers, which is seen as a necessary
 *   condition for the ritual's efficacy. The low extractiveness reflects that
 *   the constraint primarily benefits the communities by enabling their
 *   resilience, with minimal extraction from those who fail to grasp its dual
 *   nature.
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
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, 'fbf4a25c-07b6-43bb-83b9-c407b1516c26').
narrative_ontology:cs_kernel_codification('fbf4a25c-07b6-43bb-83b9-c407b1516c26', implicit).
narrative_ontology:cs_authority_grounding('fbf4a25c-07b6-43bb-83b9-c407b1516c26', practice).
narrative_ontology:cs_interpretation_layer_present('fbf4a25c-07b6-43bb-83b9-c407b1516c26').
narrative_ontology:cs_reading_relation('fbf4a25c-07b6-43bb-83b9-c407b1516c26', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('fbf4a25c-07b6-43bb-83b9-c407b1516c26', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_axiom('fbf4a25c-07b6-43bb-83b9-c407b1516c26', foundational, ritual_dual_register_essential).
narrative_ontology:cs_axiom_status(ritual_dual_register_essential, holdable).
narrative_ontology:cs_axiom_grounding('fbf4a25c-07b6-43bb-83b9-c407b1516c26', ritual_dual_register_essential, conventional).
narrative_ontology:cs_reference_frame('fbf4a25c-07b6-43bb-83b9-c407b1516c26', integrated_ritual_practice).
narrative_ontology:cs_drift_state('fbf4a25c-07b6-43bb-83b9-c407b1516c26', contemporary_academic_discourse, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('fbf4a25c-07b6-43bb-83b9-c407b1516c26', '').
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

% These communities maintain rituals that encode both symbolic meaning and practical knowledge, enabling their long-term survival and cultural continuity after catastrophic events. The dual encoding is seen as essential, not separable.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, survivor_communities, beneficiary,
    organized, generational, identity_locked, local).

% Academics or researchers who attempt to force a binary classification of ritual (either purely symbolic or purely practical) find their models fail to capture the full adaptive function, leading to incomplete or misleading analyses. The constraint extracts from them by invalidating their simplified frameworks.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, reductionist_analysts, payer,
    analytical, biographical, constrained, global).

% Individuals within the communities who actively perform and transmit the rituals. They embody the hybrid encoding, often without explicit theoretical articulation, ensuring both symbolic continuity and practical efficacy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practitioners, agenda_setter,
    moderate, biographical, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transmission of complex, multi-layered cultural memory and survival strategies across generations by embedding both symbolic identity markers and practical adaptive knowledge within ritual forms.
% TRANSFER_FUNCTION: Transfers cultural identity, social cohesion, and critical survival skills from older to younger generations within communities that have experienced catastrophe, ensuring long-term resilience.
% ABSENT_VOICES: Analysts who insist on a purely symbolic or purely practical interpretation of ritual are excluded from fully understanding its adaptive function; they would argue for theoretical purity but miss the hybrid reality.
% DISAPPEARANCE_RATIONALE: If the hybrid encoding of catastrophe memory rituals disappeared, communities would lose a vital mechanism for intergenerational transmission of both identity and practical survival knowledge, leading to cultural fragmentation and reduced resilience in the face of future challenges.
% FOUNDING_PROBLEM: How to ensure the long-term survival and cultural continuity of communities after catastrophic events, by transmitting both the symbolic meaning of their experience and the practical knowledge needed to adapt.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of post-catastrophe communities and historical accounts of cultural resilience corroborate that the dual function of ritual remains critical for survival and identity, attested by community elders and cultural historians outside the immediate beneficiary group.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The low extractiveness (0.15) and suppression (0.05) reflect that this constraint is a functional aspect of cultural transmission, not a coercive mechanism. The 'extraction' is primarily from analytical frameworks that fail to account for the hybrid nature of ritual, forcing them to either oversimplify or be incomplete. The theater ratio is low (0.1) because the rituals are genuinely functional, not performative for external validation. Accessibility collapse is high (0.8) because once the dual nature is understood, the alternatives of purely symbolic or purely practical interpretations collapse as insufficient.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the survivor communities, the hybrid encoding is a natural and essential aspect of their survival, a 'rope' that binds them to their past and future. From the perspective of reductionist analysts, it's a 'snare' that traps their theories in inadequacy, forcing them to confront a more complex reality. The engine's classification will reflect this divergence based on the structural roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Survivor communities are the primary beneficiaries, as the hybrid encoding directly supports their resilience and cultural continuity. Reductionist analysts are the 'victims' in the sense that their simplified frameworks are invalidated by the constraint's complexity, forcing them to adapt or fail in their analysis. Ritual practitioners are agenda-setters, actively maintaining the hybrid encoding through their practice.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    analytical_framing_bias,
    'Is the ''extraction'' from reductionist analysts a genuine structural property of the constraint, or a consequence of an analytical framing that prioritizes theoretical parsimony over empirical complexity?',
    'Development of new analytical frameworks that successfully integrate both symbolic and practical registers without reduction, demonstrating that the ''cost'' was to an inadequate theory, not an inherent property of the ritual.',
    'If it''s purely a framing bias, the constraint''s effective extractiveness for analysts would be lower, potentially reclassifying their seat as ''observer'' rather than ''payer''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analytical_framing_bias, conceptual, 'Ambiguity regarding whether analytical ''cost'' is inherent to the constraint or a result of theoretical limitations.').

omega_variable(
    ritual_efficacy_measurement,
    'How can the ''survival'' efficacy of hybrid-encoded rituals be empirically measured and distinguished from other adaptive strategies?',
    'Longitudinal ethnographic studies comparing communities with and without such rituals, or historical analysis correlating ritual maintenance with resilience outcomes, controlling for other variables.',
    'Strong empirical evidence of efficacy would solidify the ''rope'' classification by demonstrating clear, measurable benefits. Lack of clear evidence might shift the classification towards ''piton'' if the function is more theatrical than effective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ritual_efficacy_measurement, empirical, 'Empirical challenge in measuring the direct survival efficacy of hybrid-encoded rituals.').


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
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 30, 0.15).
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
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__competence_transmission_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
