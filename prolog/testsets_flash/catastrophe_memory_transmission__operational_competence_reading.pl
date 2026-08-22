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
 *   constraint_id: catastrophe_memory_transmission__operational_competence_reading
 *   human_readable: Catastrophe Memory Transmission (Operational Competence Reading)
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint describes ritual as a mechanism for transmitting
 *   operational competence for survival, focusing on its functional role in
 *   pattern recognition, resource coordination, and threat assessment
 *   rehearsal. Examples include the Passover Seder's emphasis on rapid
 *   departure readiness or Tisha B'Av's implicit training in resource
 *   scarcity. This is one specific reading of a broader kernel concerning
 *   catastrophe memory transmission, emphasizing the practical, rather than
 *   purely symbolic or identity-preserving, aspects of ritual. The constraint
 *   is claimed as a Rope, reflecting its genuine coordination function with
 *   low extraction, but the metrics are authored independently to reflect its
 *   actual operation.
 *
 * KEY AGENTS:
 *   - ritual_practitioners: Primary beneficiaries (moderate power/constrained exit) – gain survival competence.
 *   - future_community_members: Ultimate beneficiaries (powerless/analytical exit) – inherit preparedness.
 *   - analytical_observers: External evaluators (analytical power/analytical exit) – assess operational yield.
 *   - symbol_focused_practitioners: Excluded voices (moderate power/identity_locked exit) – prioritize symbolic meaning.
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
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Catastrophe Memory Transmission (Operational Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, 'ad31ad48-1e64-4b06-8dd6-45bb6455b646').
narrative_ontology:cs_kernel_codification('ad31ad48-1e64-4b06-8dd6-45bb6455b646', implicit).
narrative_ontology:cs_authority_grounding('ad31ad48-1e64-4b06-8dd6-45bb6455b646', practice).
narrative_ontology:cs_interpretation_layer_present('ad31ad48-1e64-4b06-8dd6-45bb6455b646').
narrative_ontology:cs_reading_relation('ad31ad48-1e64-4b06-8dd6-45bb6455b646', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad31ad48-1e64-4b06-8dd6-45bb6455b646', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('ad31ad48-1e64-4b06-8dd6-45bb6455b646', foundational, ritual_as_operational_training).
narrative_ontology:cs_axiom_status(ritual_as_operational_training, holdable).
narrative_ontology:cs_axiom_grounding('ad31ad48-1e64-4b06-8dd6-45bb6455b646', ritual_as_operational_training, empirically_contingent).
narrative_ontology:cs_reference_frame('ad31ad48-1e64-4b06-8dd6-45bb6455b646', functional_survival_mechanism).
narrative_ontology:cs_drift_state('ad31ad48-1e64-4b06-8dd6-45bb6455b646', contemporary_academic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ad31ad48-1e64-4b06-8dd6-45bb6455b646', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, ritual_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in rituals that, from this reading, directly train for future survival scenarios (e.g., rapid departure, resource management). They benefit from enhanced preparedness and the social cohesion that comes from shared competence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_practitioners, beneficiary,
    moderate, biographical, constrained, local).

% Are the ultimate beneficiaries of the transmitted operational competence, inheriting a community better equipped to face future catastrophes. They do not actively participate in the constraint's maintenance but benefit from its successful operation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, future_community_members, beneficiary,
    powerless, generational, analytical, local).

% Study the functional aspects of ritual, identifying how specific practices contribute to tangible survival skills and collective resilience. They evaluate the 'operational yield' of ritual elements.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% Prioritize the symbolic and identity-preserving aspects of ritual, potentially overlooking or downplaying its direct operational training function. They might object to a purely functional interpretation, seeing it as reductive.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, symbol_focused_practitioners, excluded,
    moderate, biographical, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action and prepares a community for future catastrophic events by encoding and transmitting practical survival skills, pattern recognition, and resource management strategies through ritualized rehearsal.
% TRANSFER_FUNCTION: Transfers practical knowledge, behavioral patterns, and collective readiness for crisis response across generations, from past experiences to future community members.
% ABSENT_VOICES: Those who view ritual primarily as symbolic or identity-preserving might object, arguing that reducing ritual to mere operational competence misses its deeper cultural and spiritual significance. They are often present within the community but their perspective is not centered in this reading.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, communities would lose a vital, non-explicit mechanism for transmitting survival competence. Future generations would be less prepared for recurring catastrophes, leading to higher mortality, social fragmentation, and a loss of collective resilience. The world would rearrange itself around less effective, more costly, and less coordinated responses to crisis.
% FOUNDING_PROBLEM: The recurring threat of catastrophe (e.g., famine, exodus, invasion) and the challenge of transmitting complex, non-propositional survival knowledge across generations without formal schooling.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of disaster-prone communities, historical accounts of ritualized responses to crisis, and cognitive science research on embodied learning corroborate the problem's historical and ongoing relevance. These sources, external to the immediate ritual practitioners, attest to the efficacy of such transmission mechanisms.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__operational_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__operational_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.15) because the primary 'cost' is the time and effort of ritual participation, which is offset by the direct benefit of enhanced survival competence. Suppression is also low (0.2) as participation is largely voluntary, driven by perceived utility and social cohesion rather than coercion. Theater ratio is minimal (0.1) because the rituals, in this reading, are genuinely functional, with little performative excess beyond what serves the transmission goal. Accessibility collapse is moderate (0.7) because while alternatives for transmitting this specific type of embodied, non-propositional knowledge are limited, other forms of learning exist. Resistance is low (0.05) as the benefits are widely recognized within the community.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ritual practitioners, the constraint is a beneficial coordination mechanism. From an analytical observer's perspective, it is a functional system for knowledge transfer. There is no significant divergence in perceived type, as the operational benefits are clear. However, other readings of the same kernel would emphasize different aspects, leading to different classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual practitioners are beneficiaries because they directly acquire survival skills. Future community members are also beneficiaries, inheriting a more resilient community. There are no direct 'victims' in this reading, as the 'cost' of ritual is outweighed by its functional benefits. Symbol-focused practitioners are 'excluded' in the sense that their primary interpretive frame is not centered here, but they are not 'victims' of extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in this reading because the founding problem (catastrophe preparedness) remains live, and the ritual continues to effectively address it. The classification as a Rope prevents mislabeling it as pure extraction, as its coordination function is genuine and ongoing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_vs_symbolic_primacy,
    'Is the primary function of this ritual the transmission of operational competence, or is it the preservation of symbolic continuity and identity?',
    'Empirical studies comparing community resilience outcomes with fidelity to operational vs. symbolic ritual elements, or ethnographic analysis of participant self-understanding and stated goals.',
    'If symbolic primacy is established, the constraint might reclassify towards a different type (e.g., identity_coordination Rope or even a Piton if the operational function atrophies). If operational primacy is confirmed, the Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_vs_symbolic_primacy, conceptual, 'Ambiguity regarding the primary function of ritual: operational training vs. symbolic preservation.').

omega_variable(
    universality_of_competence_extraction,
    'Is the mechanism of transmitting survival competence through ritual a universal, irreducible feature of human collective memory (a Mountain), or a culturally contingent coordination strategy (a Rope)?',
    'Cross-cultural comparative studies of disaster response and knowledge transmission, or cognitive science research on the fundamental mechanisms of embodied collective memory.',
    'If universal, the constraint would reclassify as a Mountain, with negligible extraction. If culturally contingent, the Rope classification holds, emphasizing its constructed nature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universality_of_competence_extraction, empirical, 'Whether the competence transmission is a universal law or a cultural construct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 25, 0.09).
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

narrative_ontology:coordination_type(catastrophe_memory_transmission__operational_competence_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
