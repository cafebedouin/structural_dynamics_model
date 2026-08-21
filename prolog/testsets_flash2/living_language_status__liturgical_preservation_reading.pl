% ============================================================================
% CONSTRAINT STORY: living_language_status__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__liturgical_preservation_reading, []).

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
 *   constraint_id: living_language_status__liturgical_preservation_reading
 *   human_readable: Living Language Status: Liturgical Preservation Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint defines a language as 'living' if its sacred texts are
 *   continuously recited, studied, and used in ritual. It is one reading of
 *   the broader 'living_language_status' kernel. This reading emphasizes
 *   preservation through liturgical transmission as sufficient for vitality,
 *   contrasting with readings that require native generational transmission
 *   or new literary production. The claimed type is 'rope' because it
 *   genuinely coordinates religious communities around a shared linguistic
 *   practice, but the metrics reflect a low level of extraction from those
 *   whose linguistic practices are delegitimized by this definition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.25).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.4).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Living Language Status: Liturgical Preservation Reading").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, 'c1cbc499-03c3-4690-ac86-253f22d375ef').
narrative_ontology:cs_kernel_codification('c1cbc499-03c3-4690-ac86-253f22d375ef', fixed_text).
narrative_ontology:cs_authority_grounding('c1cbc499-03c3-4690-ac86-253f22d375ef', lineage).
narrative_ontology:cs_interpretation_layer_present('c1cbc499-03c3-4690-ac86-253f22d375ef').
narrative_ontology:cs_reading_relation('c1cbc499-03c3-4690-ac86-253f22d375ef', living_language_status__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1cbc499-03c3-4690-ac86-253f22d375ef', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('c1cbc499-03c3-4690-ac86-253f22d375ef', foundational, sacred_text_transmission_is_vitality).
narrative_ontology:cs_axiom_status(sacred_text_transmission_is_vitality, holdable).
narrative_ontology:cs_axiom_grounding('c1cbc499-03c3-4690-ac86-253f22d375ef', sacred_text_transmission_is_vitality, theological).
narrative_ontology:cs_axiom('c1cbc499-03c3-4690-ac86-253f22d375ef', secondary, ritual_use_preserves_essence).
narrative_ontology:cs_axiom_status(ritual_use_preserves_essence, holdable).
narrative_ontology:cs_axiom_grounding('c1cbc499-03c3-4690-ac86-253f22d375ef', ritual_use_preserves_essence, conventional).
narrative_ontology:cs_reference_frame('c1cbc499-03c3-4690-ac86-253f22d375ef', ancient_liturgical_continuity).
narrative_ontology:cs_drift_state('c1cbc499-03c3-4690-ac86-253f22d375ef', contemporary_secularization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c1cbc499-03c3-4690-ac86-253f22d375ef', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, religious_communities).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_speech_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces the criteria for a language's 'living' status within religious contexts, maintaining interpretive monopoly over sacred texts. Benefits from the continued relevance and authority of the liturgical tradition.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Derive identity and continuity from the liturgical use of the language. The constraint provides a clear, stable framework for cultural and religious transmission, reinforcing group cohesion.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, religious_communities, beneficiary,
    organized, generational, identity_locked, local).

% Their use of the language outside of liturgical contexts, or their efforts to modernize it, are often delegitimized or seen as desecration by proponents of liturgical preservation. They bear the cost of having their linguistic vitality questioned or dismissed.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_speech_community, payer,
    moderate, biographical, constrained, national).

% Analyze the various criteria for language vitality, including liturgical use, native generation, and literary production. They observe the social and political implications of each definition without directly participating in the constraint's enforcement or benefit.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, linguistic_scholars, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation and transmission of sacred texts and religious traditions across generations by establishing a clear, stable definition of linguistic vitality tied to ritual practice.
% TRANSFER_FUNCTION: Transfers interpretive authority and cultural legitimacy to religious institutions and traditions that maintain liturgical use, while potentially diminishing the status of secular or modernizing uses of the language.
% ABSENT_VOICES: Advocates for a 'living' status based on daily native speech or modern literary production are often dismissed or excluded from the discourse on the language's true vitality within this framework. They would argue that liturgical use alone is insufficient.
% DISAPPEARANCE_RATIONALE: If this definition of 'living language' vanished, the authority of rabbinical institutions over linguistic matters would diminish, and religious communities would lose a clear framework for their linguistic identity. The debate over language vitality would shift, potentially empowering secular or modernizing interpretations.
% FOUNDING_PROBLEM: The problem of ensuring the continuity and sacred status of ancient languages and their associated religious traditions in the face of linguistic evolution and secularization.
% FOUNDING_PROBLEM_CORROBORATION: Religious scholars and community leaders attest that the problem of preserving sacred languages is ongoing and central to their mission. Independent sociolinguists acknowledge the historical role of liturgical preservation in maintaining linguistic continuity, even if they dispute its sufficiency for 'living' status.
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__liturgical_preservation_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__liturgical_preservation_reading_tests).
:- end_tests(living_language_status__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the primary function is coordination around a fixed corpus, not direct material extraction. Suppression (0.4) is moderate, as this definition implicitly suppresses alternative criteria for linguistic vitality, but does not actively coerce individuals into liturgical practice. Theater ratio is low (0.1) as the liturgical activities are genuinely functional for religious communities. Accessibility collapse (0.6) is moderate, as alternative definitions of 'living' exist but are often dismissed within this framework. Resistance (0.3) is present from secular communities but not overwhelming.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinical authority, this is a pure rope, a necessary coordination mechanism for religious continuity. From the secular speech community, it functions as a subtle snare, delegitimizing their linguistic practices and identity. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinical authority and religious communities are beneficiaries (d near 0.0) as this definition reinforces their cultural and religious roles. The secular speech community is a victim (d near 1.0) as their linguistic practices are implicitly devalued. Linguistic scholars are observers (d near 0.5) as they analyze the phenomenon without direct benefit or cost.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_vitality,
    'Is the ''living'' status of a language, as defined by liturgical preservation, an inherent property of its continuous use, or a constructed claim that benefits specific religious authorities?',
    'Analysis of historical shifts in linguistic authority and the political economy of religious institutions. If the definition''s prominence correlates with institutional power rather than intrinsic linguistic features, it leans towards constructed.',
    'If constructed, the constraint''s extractiveness and suppression for secular communities would be re-evaluated upwards, potentially shifting its classification towards a Tangled Rope or Snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_vitality, conceptual, 'Ambiguity between inherent linguistic vitality and institutionally defined status.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of secular linguistic practices structural (e.g., institutional delegitimization) or internalized (e.g., self-censorship by secular speakers)?',
    'Post-exit suppression trajectory: if secular speakers continue to devalue their own linguistic practices even after leaving religious communities, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for secular language use.').

omega_variable(
    coordination_vs_exclusion,
    'To what extent does this definition primarily coordinate religious practice versus actively exclude or delegitimize alternative forms of linguistic vitality?',
    'Comparative analysis of resource allocation: if significant resources are dedicated to actively discrediting non-liturgical uses, it indicates a stronger exclusionary function.',
    'A stronger exclusionary function would increase the perceived extractiveness and suppression for non-beneficiary seats, potentially reclassifying it as a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_exclusion, empirical, 'Balance between coordination and exclusionary effects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(livi_tr_t10, living_language_status__liturgical_preservation_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(livi_tr_t20, living_language_status__liturgical_preservation_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(livi_tr_t30, living_language_status__liturgical_preservation_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(livi_tr_t40, living_language_status__liturgical_preservation_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(livi_tr_t50, living_language_status__liturgical_preservation_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(livi_be_t10, living_language_status__liturgical_preservation_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(livi_be_t20, living_language_status__liturgical_preservation_reading, base_extractiveness, 20, 0.23).
narrative_ontology:measurement(livi_be_t30, living_language_status__liturgical_preservation_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement(livi_be_t40, living_language_status__liturgical_preservation_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(livi_be_t50, living_language_status__liturgical_preservation_reading, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__liturgical_preservation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(livi_su_t10, living_language_status__liturgical_preservation_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(livi_su_t20, living_language_status__liturgical_preservation_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(livi_su_t30, living_language_status__liturgical_preservation_reading, suppression_requirement, 30, 0.39).
narrative_ontology:measurement(livi_su_t40, living_language_status__liturgical_preservation_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(livi_su_t50, living_language_status__liturgical_preservation_reading, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'living_language_status' kernel. This reading emphasizes liturgical preservation, while others focus on native generational transmission or new literary production. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
