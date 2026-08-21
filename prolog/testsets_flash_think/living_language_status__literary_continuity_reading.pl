% ============================================================================
% CONSTRAINT STORY: living_language_status__literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__literary_continuity_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: living_language_status__literary_continuity_reading
 *   human_readable: Living Language Status: Literary Continuity Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint is the `literary_continuity_reading` of the
 *   `living_language_status` kernel. It defines a language as living based on
 *   its continuous production of new literary and intellectual work,
 *   regardless of native speaker status. This reading contrasts with the
 *   `liturgical_preservation_reading` (vitality through ritual use) and the
 *   `native_generation_reading` (vitality through generational transmission
 *   as a mother tongue). The Haskalah movement and modern Hebrew literature
 *   are prime examples of this definition in action, demonstrating how a
 *   language can be 'revived' or maintained as living through elite literary
 *   efforts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.25).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.15).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Living Language Status: Literary Continuity Reading").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistics/religious_studies/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, 'b185e381-cd6f-404a-9cbd-64863fd2750f').
narrative_ontology:cs_kernel_codification('b185e381-cd6f-404a-9cbd-64863fd2750f', implicit).
narrative_ontology:cs_authority_grounding('b185e381-cd6f-404a-9cbd-64863fd2750f', expertise).
narrative_ontology:cs_interpretation_layer_present('b185e381-cd6f-404a-9cbd-64863fd2750f').
narrative_ontology:cs_reading_relation('b185e381-cd6f-404a-9cbd-64863fd2750f', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('b185e381-cd6f-404a-9cbd-64863fd2750f', living_language_status__native_generation_reading, coexists_with).
narrative_ontology:cs_axiom('b185e381-cd6f-404a-9cbd-64863fd2750f', foundational, literary_production_is_vitality).
narrative_ontology:cs_axiom_status(literary_production_is_vitality, holdable).
narrative_ontology:cs_axiom_grounding('b185e381-cd6f-404a-9cbd-64863fd2750f', literary_production_is_vitality, conventional).
narrative_ontology:cs_axiom('b185e381-cd6f-404a-9cbd-64863fd2750f', secondary, native_speaker_status_is_secondary).
narrative_ontology:cs_axiom_status(native_speaker_status_is_secondary, holdable).
narrative_ontology:cs_axiom_grounding('b185e381-cd6f-404a-9cbd-64863fd2750f', native_speaker_status_is_secondary, conventional).
narrative_ontology:cs_reference_frame('b185e381-cd6f-404a-9cbd-64863fd2750f', continuous_literary_creation_as_norm).
narrative_ontology:cs_drift_state('b185e381-cd6f-404a-9cbd-64863fd2750f', contemporary_sociolinguistic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b185e381-cd6f-404a-9cbd-64863fd2750f', '').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, maskilim_and_secular_intellectuals).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, illiterate_or_non_literary_speakers).
narrative_ontology:constraint_vindicates(living_language_status__literary_continuity_reading, cultural_renaissance_through_literature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups actively produce new literary and intellectual work in the language, thereby defining and demonstrating its 'living' status according to this reading. They gain cultural authority and legitimacy for their cultural project.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, maskilim_and_secular_intellectuals, agenda_setter,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(living_language_status__literary_continuity_reading, maskilim_and_secular_intellectuals, beneficiary).

% Their daily or oral use of the language, if not tied to literary production, is implicitly devalued or deemed insufficient to confer 'living' status. They bear the cost of their linguistic practice being excluded from the definition of vitality, despite their identity being tied to the language.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, illiterate_or_non_literary_speakers, payer,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(living_language_status__literary_continuity_reading, illiterate_or_non_literary_speakers, excluded).

% They analyze and debate the criteria for language vitality, including this reading, from an academic perspective. Their work can either reinforce or challenge the definition.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, linguists_and_scholars, observer,
    analytical, generational, analytical, global).

% Their focus on the language's use in sacred texts and ritual is implicitly sidelined by this reading's emphasis on new literary production. They are excluded from the primary definition of vitality, though their practices contribute to the language's continuity in other ways.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, religious_scholars_and_liturgists, excluded,
    organized, civilizational, constrained, regional).

% They advocate for generational transmission as a mother tongue in daily life as the primary criterion for a living language. This reading's 'regardless of native speaker status' clause directly challenges their position, effectively excluding their preferred metric from the definition of vitality.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, native_speakers_advocates, excluded,
    organized, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the efforts of intellectuals and writers around defining and demonstrating a language's vitality through continuous literary and intellectual output, fostering a shared cultural project and legitimizing its use in modern contexts.
% TRANSFER_FUNCTION: Transfers cultural authority and recognition of 'vitality' from those who use the language primarily in daily speech or liturgical contexts to those who produce new literary and intellectual works in it.
% ABSENT_VOICES: Native speakers who transmit the language orally but do not engage in literary production, and religious communities who maintain the language through liturgy, are excluded. They would argue for their forms of use as equally valid indicators of a living language, challenging the literary-centric definition.
% DISAPPEARANCE_RATIONALE: If this definition vanished, the criteria for a 'living language' would immediately shift, potentially elevating other forms of use (like daily speech or liturgical use) and altering the cultural authority of literary producers. The narrative of modern Hebrew's revival, which heavily relied on this definition, would be fundamentally re-evaluated.
% FOUNDING_PROBLEM: To establish a legitimate claim for the vitality of Hebrew, which lacked a large native-speaking population but had a rich literary tradition and a burgeoning modern literary movement, against definitions that prioritized daily spoken use or solely liturgical preservation.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Haskalah movement and scholars of modern Hebrew literature corroborate this problem, noting the explicit arguments made by intellectuals of the era to define Hebrew as a living language through its literary output, often in direct response to critics who deemed it 'dead' based on other criteria. This debate continues in sociolinguistics and nationalism studies.
narrative_ontology:disappearance_verdict(living_language_status__literary_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__literary_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__literary_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(living_language_status__literary_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__literary_continuity_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__literary_continuity_reading_tests).
:- end_tests(living_language_status__literary_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.25) because the constraint primarily functions as a coordinating definition for a cultural project, rather than coercively extracting resources. Suppression is also low (0.15) as it's a definitional framing, not actively enforced with coercive power, though it implicitly devalues other forms of linguistic vitality. Theater ratio is low (0.10) because the literary and intellectual work produced under this definition is genuinely productive. Accessibility collapse is moderate (0.40) because while it doesn't physically prevent other forms of language use, it makes them less 'accessible' as criteria for vitality. Resistance is moderate (0.50) due to ongoing debates with proponents of other definitions of language vitality.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the maskilim and secular intellectuals, this constraint is a productive 'rope' that enables a cultural renaissance. From the perspective of those excluded by this definition (e.g., non-literary speakers, liturgical users), it functions as a 'snare' that devalues their linguistic practices and cultural contributions, even if not coercively enforced. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Maskilim and secular intellectuals are the primary beneficiaries and agenda-setters, as their activities directly fulfill this definition of vitality, granting them cultural authority. Illiterate or non-literary speakers are implicitly payers/excluded, as their forms of language use are not recognized as primary indicators of vitality. Religious scholars and native speaker advocates are also excluded, as their preferred criteria for vitality are sidelined by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as it is a definitional framework rather than a functional institution. Its 'mandate' is to define vitality, which remains a live conceptual problem. The ongoing contestation with other readings prevents it from becoming a 'piton' of theatrical maintenance, as its proponents must continuously demonstrate its relevance through literary output.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''living_language_status'' kernel, or merely a component of a broader, unified definition?',
    'Analysis of historical and contemporary debates: if proponents of this reading explicitly differentiate it from other criteria for vitality, it confirms its status as a distinct reading.',
    'If not a distinct reading, the ''living_language_status'' kernel might be a single, more complex constraint, altering the classification of its components.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint as a specific reading of the ''living_language_status'' kernel.').

omega_variable(
    definition_impact_on_resource_allocation,
    'Does this definition of ''living language'' influence the allocation of resources (e.g., funding for literary programs vs. oral tradition preservation)?',
    'Empirical study of cultural funding bodies and educational curricula: track resource flows to literary vs. other linguistic initiatives.',
    'If it significantly influences resource allocation, its extractiveness and suppression might be higher than currently estimated, as it actively channels support away from other forms of linguistic vitality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_impact_on_resource_allocation, empirical, 'Impact of definition on resource allocation for language preservation.').

omega_variable(
    exclusion_as_suppression,
    'To what extent does the exclusion of non-literary forms of language use from the definition of ''vitality'' constitute a form of suppression?',
    'Qualitative sociological studies of affected communities: assess the psychological and social impact of their linguistic practices being devalued by dominant definitions.',
    'If the exclusion leads to significant social or psychological disempowerment, the effective suppression for ''illiterate_or_non_literary_speakers'' would be higher, potentially shifting their seat classification towards a ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_as_suppression, conceptual, 'Assessing the suppressive effect of definitional exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 1800, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t1800, living_language_status__literary_continuity_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(livi_tr_t1830, living_language_status__literary_continuity_reading, theater_ratio, 1830, 0.06).
narrative_ontology:measurement(livi_tr_t1860, living_language_status__literary_continuity_reading, theater_ratio, 1860, 0.07).
narrative_ontology:measurement(livi_tr_t1890, living_language_status__literary_continuity_reading, theater_ratio, 1890, 0.08).
narrative_ontology:measurement(livi_tr_t1920, living_language_status__literary_continuity_reading, theater_ratio, 1920, 0.09).
narrative_ontology:measurement(livi_tr_t1950, living_language_status__literary_continuity_reading, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(livi_be_t1800, living_language_status__literary_continuity_reading, base_extractiveness, 1800, 0.2).
narrative_ontology:measurement(livi_be_t1830, living_language_status__literary_continuity_reading, base_extractiveness, 1830, 0.21).
narrative_ontology:measurement(livi_be_t1860, living_language_status__literary_continuity_reading, base_extractiveness, 1860, 0.22).
narrative_ontology:measurement(livi_be_t1890, living_language_status__literary_continuity_reading, base_extractiveness, 1890, 0.23).
narrative_ontology:measurement(livi_be_t1920, living_language_status__literary_continuity_reading, base_extractiveness, 1920, 0.24).
narrative_ontology:measurement(livi_be_t1950, living_language_status__literary_continuity_reading, base_extractiveness, 1950, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t1800, living_language_status__literary_continuity_reading, suppression_requirement, 1800, 0.1).
narrative_ontology:measurement(livi_su_t1830, living_language_status__literary_continuity_reading, suppression_requirement, 1830, 0.11).
narrative_ontology:measurement(livi_su_t1860, living_language_status__literary_continuity_reading, suppression_requirement, 1860, 0.12).
narrative_ontology:measurement(livi_su_t1890, living_language_status__literary_continuity_reading, suppression_requirement, 1890, 0.13).
narrative_ontology:measurement(livi_su_t1920, living_language_status__literary_continuity_reading, suppression_requirement, 1920, 0.14).
narrative_ontology:measurement(livi_su_t1950, living_language_status__literary_continuity_reading, suppression_requirement, 1950, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__literary_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, hebrew_language_revival_narrative).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
