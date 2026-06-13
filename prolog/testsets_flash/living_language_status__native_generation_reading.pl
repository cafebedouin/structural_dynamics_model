% ============================================================================
% CONSTRAINT STORY: living_language_status__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__native_generation_reading, []).

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
 *   constraint_id: living_language_status__native_generation_reading
 *   human_readable: Living Language Status: Native Generational Transmission Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint defines a 'living language' exclusively by native,
 *   generational transmission as a mother tongue in daily life. It frames
 *   liturgical recitation or literary production as insufficient for
 *   vitality, often serving to legitimize secular nationalist movements
 *   seeking linguistic sovereignty and to marginalize communities whose
 *   language use does not fit this narrow definition. It is a reading of the
 *   'living_language_status' kernel, which has competing definitions.
 *
 * KEY AGENTS:
 *   - secular_nationalist_movements: Primary beneficiary (institutional/arbitrage) — gains legitimacy and political power.
 *   - linguistic_revitalization_programs: Secondary beneficiary (organized/constrained) — receives funding and mandates based on this definition.
 *   - liturgical_only_communities: Primary victim (powerless/identity_locked) — their language is delegitimized, often leading to cultural marginalization.
 *   - diaspora_communities_without_territory: Secondary victim (powerless/constrained) — struggle to meet the criteria without a contiguous native-speaking territory.
 *   - sociolinguists: Observer (analytical/analytical) — study the effects of this definition and its alternatives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.6).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.7).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Living Language Status: Native Generational Transmission Reading").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, '308e7f62-431b-498b-a758-e7b69c3a1e1c').
narrative_ontology:cs_kernel_codification('308e7f62-431b-498b-a758-e7b69c3a1e1c', implicit).
narrative_ontology:cs_authority_grounding('308e7f62-431b-498b-a758-e7b69c3a1e1c', extraction).
narrative_ontology:cs_interpretation_layer_present('308e7f62-431b-498b-a758-e7b69c3a1e1c').
narrative_ontology:cs_reading_relation('308e7f62-431b-498b-a758-e7b69c3a1e1c', living_language_status__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('308e7f62-431b-498b-a758-e7b69c3a1e1c', living_language_status__literary_continuity_reading, forecloses).
narrative_ontology:cs_axiom('308e7f62-431b-498b-a758-e7b69c3a1e1c', foundational, generational_transmission_is_sole_vitality_metric).
narrative_ontology:cs_axiom_status(generational_transmission_is_sole_vitality_metric, holdable).
narrative_ontology:cs_axiom_grounding('308e7f62-431b-498b-a758-e7b69c3a1e1c', generational_transmission_is_sole_vitality_metric, conventional).
narrative_ontology:cs_axiom('308e7f62-431b-498b-a758-e7b69c3a1e1c', secondary, liturgical_use_is_preservation_not_life).
narrative_ontology:cs_axiom_status(liturgical_use_is_preservation_not_life, holdable).
narrative_ontology:cs_axiom_grounding('308e7f62-431b-498b-a758-e7b69c3a1e1c', liturgical_use_is_preservation_not_life, conventional).
narrative_ontology:cs_reference_frame('308e7f62-431b-498b-a758-e7b69c3a1e1c', ideal_native_speaker_community).
narrative_ontology:cs_drift_state('308e7f62-431b-498b-a758-e7b69c3a1e1c', contemporary_globalized_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('308e7f62-431b-498b-a758-e7b69c3a1e1c', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_movements).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, linguistic_revitalization_programs).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, diaspora_communities_without_territory).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(living_language_status__native_generation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_language_status__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(living_language_status__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) stems from the resources and legitimacy diverted to languages meeting this criterion, and the cultural capital denied to those that don't. Suppression (0.7) is high due to institutional policies (e.g., state funding for education, media) that privilege native-transmitted languages and implicitly or explicitly devalue others. Theater ratio is low (0.1) as the definition is actively enforced and shapes real-world policy, not merely performative. Accessibility collapse is moderate (0.4) as alternative definitions exist but are often suppressed in public discourse. Resistance is moderate (0.6) from communities challenging this narrow definition.
 *
 * PERSPECTIVAL GAP:
 *   Secular nationalist movements experience this as a Rope, providing a clear, objective criterion for national identity and linguistic policy. Liturgical-only communities experience it as a Snare, as their deeply held cultural practices are delegitimized and their language declared 'dead' despite continuous use. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular nationalist movements are full beneficiaries (d=0.0) as this definition provides a powerful tool for nation-building and cultural homogenization. Liturgical-only communities and diaspora communities are targets (d=1.0) as their forms of language vitality are explicitly excluded and devalued. Linguistic revitalization programs are beneficiaries (d=0.2) as they gain a clear mandate and resources, but also bear some cost of enforcing the narrow definition. Sociolinguists are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling a politically constructed definition as a natural linguistic fact. By identifying beneficiaries and victims, it highlights how a seemingly objective linguistic criterion can function as an extractive mechanism, channeling resources and legitimacy towards certain groups while marginalizing others. The 'corpse' metaphor is a key part of the extraction, framing non-native transmission as inherently inferior.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine, universally applicable definition of a ''living language'', or one specific reading of a contested kernel?',
    'Analysis of competing definitions and their social/political functions; identification of alternative readings with different beneficiary/victim structures.',
    'If a specific reading, its classification as a Tangled Rope is strengthened by revealing its constructed nature and the interests it serves. If a universal definition, its classification might shift towards Mountain, but the presence of beneficiaries would trigger False Summit detection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''native generational transmission'' reading of the ''living_language_status'' kernel.').

omega_variable(
    impact_of_sibling_readings,
    'How would the structural classification of this ''native generational transmission'' reading change if the ''liturgical preservation'' or ''literary continuity'' readings were adopted?',
    'Constructing separate constraint stories for each sibling reading and comparing their computed classifications and beneficiary/victim structures.',
    'The ''liturgical preservation'' reading would likely classify as a Rope or Mountain for its communities, while the ''literary continuity'' reading might be a Rope or Tangled Rope for intellectual elites. This highlights the extractive nature of the ''native generation'' reading for non-native speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_sibling_readings, conceptual, 'Examines the classification delta if sibling readings of the ''living_language_status'' kernel were adopted.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (lack of state support for non-native transmission) or internalized (communities internalizing the ''dead language'' label)?',
    'Post-intervention linguistic vitality: if communities thrive after state support for non-native transmission, suppression was structural. If decline persists, internalized suppression is at play.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as communities carry the suppression with them. If structural, policy changes can more directly address the issue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for non-native language communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__native_generation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(livi_tr_t10, living_language_status__native_generation_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(livi_tr_t20, living_language_status__native_generation_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__native_generation_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(livi_be_t10, living_language_status__native_generation_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(livi_be_t20, living_language_status__native_generation_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__native_generation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(livi_su_t10, living_language_status__native_generation_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(livi_su_t20, living_language_status__native_generation_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'living_language_status' kernel. Each reading defines 'living language' differently, leading to distinct beneficiary/victim structures and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
