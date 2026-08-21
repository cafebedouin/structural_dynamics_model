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
 *   This constraint models ritual as a mechanism for encoding and
 *   transmitting practical survival knowledge, such as timing for
 *   agricultural cycles, resource management, family protocols, and
 *   adaptation strategies, particularly in the context of historical or
 *   anticipated catastrophes. This is one specific reading of the broader
 *   'catastrophe_memory_survival' kernel. The focus is on the functional,
 *   adaptive content of ritual, rather than its symbolic or
 *   identity-preserving aspects.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.3).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Ritual as Competence Transmission for Catastrophe Survival").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, 'b549e2ee-e1f2-42f2-b468-eee960ac3c88').
narrative_ontology:cs_kernel_codification('b549e2ee-e1f2-42f2-b468-eee960ac3c88', implicit).
narrative_ontology:cs_authority_grounding('b549e2ee-e1f2-42f2-b468-eee960ac3c88', practice).
narrative_ontology:cs_interpretation_layer_present('b549e2ee-e1f2-42f2-b468-eee960ac3c88').
narrative_ontology:cs_reading_relation('b549e2ee-e1f2-42f2-b468-eee960ac3c88', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('b549e2ee-e1f2-42f2-b468-eee960ac3c88', catastrophe_memory_survival__hybrid_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('b549e2ee-e1f2-42f2-b468-eee960ac3c88', foundational, ritual_as_adaptive_algorithm).
narrative_ontology:cs_axiom_status(ritual_as_adaptive_algorithm, holdable).
narrative_ontology:cs_axiom_grounding('b549e2ee-e1f2-42f2-b468-eee960ac3c88', ritual_as_adaptive_algorithm, empirically_contingent).
narrative_ontology:cs_reference_frame('b549e2ee-e1f2-42f2-b468-eee960ac3c88', functional_adaptive_ritual).
narrative_ontology:cs_drift_state('b549e2ee-e1f2-42f2-b468-eee960ac3c88', contemporary_secularization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b549e2ee-e1f2-42f2-b468-eee960ac3c88', '').
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

% These communities actively use ritual to transmit practical knowledge for adapting to new environments or recovering from past catastrophes. They benefit from the embedded survival strategies and resource management techniques.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities, beneficiary,
    organized, generational, constrained, global).

% These generations inherit the practical knowledge encoded in ritual, which can be crucial for their survival and adaptation to future challenges. They are passive recipients of this transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, future_generations, beneficiary,
    powerless, generational, trapped, universal).

% These communities maintain the ritual forms but have lost the explicit understanding of the practical survival knowledge they once encoded. They bear the cost of performing rituals without fully realizing their adaptive function, potentially leading to maladaptation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, communities_losing_practical_content, payer,
    moderate, biographical, constrained, local).

% These individuals are responsible for the faithful performance and transmission of rituals. They act as custodians of the encoded knowledge, even if their understanding of its practical content varies. Their identity is often fused with their role.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, ritual_practitioners, agenda_setter,
    organized, biographical, identity_locked, local).

% These observers analyze the structure and function of rituals across cultures, seeking to understand their role in knowledge transmission and survival. They provide an external, analytical perspective on the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, anthropological_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intergenerational transmission of practical survival knowledge and adaptive strategies within communities, especially those facing or recovering from catastrophic events.
% TRANSFER_FUNCTION: Transfers tacit and explicit knowledge about resource management, timing, social protocols, and environmental adaptation from older to younger generations, and across dispersed communities.
% ABSENT_VOICES: Communities that have fully secularized or rationalized their practices might dismiss ritual as superstition, missing the embedded practical knowledge. They would argue for direct, explicit instruction over ritual encoding.
% DISAPPEARANCE_RATIONALE: If the ritual encoding of practical knowledge vanished, communities would lose a vital, resilient mechanism for transmitting survival strategies, especially in times of crisis. This would necessitate new, potentially less effective, methods of knowledge transfer, leading to significant social and adaptive reorganization.
% FOUNDING_PROBLEM: The challenge of transmitting complex, context-dependent survival knowledge across generations and through periods of social disruption or environmental catastrophe, without relying solely on explicit written or oral traditions.
% FOUNDING_PROBLEM_CORROBORATION: Historical and ethnographic studies from outside the benefiting communities (e.g., disaster sociology, ecological anthropology) corroborate that ritual has served this function effectively in many societies, and that the problem of resilient knowledge transmission remains live, particularly in the face of climate change and social upheaval.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.45) because the 'cost' is the effort of maintaining ritual form, which may obscure the practical content for some, or lead to misapplication if the context changes. Suppression is low (0.3) as adherence is often cultural or identity-based rather than coercively enforced. Theater ratio is low (0.2) because the primary function (knowledge transmission) is still active, even if not always explicitly recognized. Accessibility collapse is moderate (0.4) because while ritual provides a robust channel, alternative, more explicit forms of knowledge transmission exist but may be less resilient.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of communities actively using the encoded knowledge, the ritual is a vital rope. From communities that have lost the practical content, it may appear more like a piton, maintained out of inertia or symbolic value, without realizing its original adaptive function. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities and future generations are beneficiaries, gaining adaptive capacity. Communities that lose the practical content while maintaining ritual form are victims, bearing the cost of 'empty' performance. Ritual practitioners are agenda-setters, maintaining the constraint, often with identity-locked exit options. Anthropological observers provide an analytical perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (transmitting survival knowledge) is still live for many communities, preventing mislabeling as a piton. However, for communities where the practical content has atrophied, the constraint's persistence by inertia (theater) could lead to a piton-like classification for those specific seats, even if the overall constraint is a rope for others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    practical_content_decay,
    'To what extent has the practical survival knowledge embedded in rituals decayed or become misapplied due to changing contexts, while the ritual form persists?',
    'Longitudinal ethnographic studies comparing ritual performance with actual adaptive outcomes in different ecological and social contexts, or historical analysis of ritual evolution post-catastrophe.',
    'If practical content decay is widespread, the constraint''s effective extractiveness for ''communities_losing_practical_content'' would be higher, pushing it towards a piton for those seats. If content remains robust, it reinforces the rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_content_decay, empirical, 'Measures the functional integrity of the knowledge transmitted by ritual.').

omega_variable(
    symbolic_vs_practical_primacy,
    'Is the primary function of ritual in catastrophe memory survival the transmission of practical knowledge, or the maintenance of group identity and symbolic cohesion?',
    'Comparative analysis of communities that prioritize one function over the other in their ritual practice, examining their long-term survival and adaptive capacity. This is a conceptual distinction that empirical data can inform but not fully resolve.',
    'If symbolic primacy is established, this reading''s extractiveness might be lower (as the ''cost'' of losing practical content is less central), and the ''symbol_survival_reading'' would gain explanatory power. If practical primacy holds, this reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_practical_primacy, conceptual, 'Distinguishes the core adaptive mechanism of ritual in survival contexts.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (adherence to ritual form) structural (e.g., social ostracism for non-compliance) or internalized (e.g., deep-seated belief in ritual efficacy, identity fusion)?',
    'Post-migration studies of diaspora communities: if ritual adherence persists strongly after structural pressures (e.g., community elders, traditional authorities) are reduced, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making exit from ritual practice more costly than it appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for ritual adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 60, 0.45).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 80, 0.43).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 60, 0.28).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 80, 0.29).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 100, 0.3).


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
