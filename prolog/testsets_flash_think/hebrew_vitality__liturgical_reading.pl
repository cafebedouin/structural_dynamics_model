% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__liturgical_reading, []).

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
 *   constraint_id: hebrew_vitality__liturgical_reading
 *   human_readable: Hebrew Vitality: Liturgical Use as Kernel
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint represents the 'liturgical reading' of Hebrew vitality,
 *   which posits that the unbroken ritual preservation and use of Hebrew
 *   constitutes its ongoing life and relevance. It is a core tenet within
 *   traditional Jewish religious frameworks, emphasizing continuity with
 *   sacred texts and tradition over modern spoken fluency. The constraint
 *   functions as a 'rope' by coordinating a shared, voluntary practice that
 *   benefits the community without significant extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.15).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.1).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Hebrew Vitality: Liturgical Use as Kernel").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistics/language_revitalization/jewish_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, 'b1e7277d-3ca4-4d06-bc2d-4aff0edd55b4').
narrative_ontology:cs_kernel_codification('b1e7277d-3ca4-4d06-bc2d-4aff0edd55b4', formalized).
narrative_ontology:cs_authority_grounding('b1e7277d-3ca4-4d06-bc2d-4aff0edd55b4', lineage).
narrative_ontology:cs_interpretation_layer_present('b1e7277d-3ca4-4d06-bc2d-4aff0edd55b4').
narrative_ontology:cs_reading_relation('b1e7277d-3ca4-4d06-bc2d-4aff0edd55b4', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1e7277d-3ca4-4d06-bc2d-4aff0edd55b4', hebrew_vitality__hybrid_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('b1e7277d-3ca4-4d06-bc2d-4aff0edd55b4', foundational, liturgical_use_is_vitality).
narrative_ontology:cs_axiom_status(liturgical_use_is_vitality, holdable).
narrative_ontology:cs_axiom_grounding('b1e7277d-3ca4-4d06-bc2d-4aff0edd55b4', liturgical_use_is_vitality, deontological).
narrative_ontology:cs_reference_frame('b1e7277d-3ca4-4d06-bc2d-4aff0edd55b4', unbroken_liturgical_tradition).
narrative_ontology:cs_drift_state('b1e7277d-3ca4-4d06-bc2d-4aff0edd55b4', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b1e7277d-3ca4-4d06-bc2d-4aff0edd55b4', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and transmit the tradition of liturgical Hebrew, defining its proper use and ensuring its continuity as the core of Jewish religious life. They benefit from the stability and authority derived from this unbroken tradition.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Participate in and uphold the liturgical use of Hebrew, deriving spiritual, cultural, and historical continuity from it. The constraint provides a shared, sacred language that binds communities across time and space.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, jewish_communities, beneficiary,
    organized, generational, constrained, global).

% Observe and analyze the role of liturgical Hebrew, often from a secular or modern revival perspective. While they may not fully endorse this reading of vitality, they acknowledge its historical and cultural significance.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, linguists_and_revivalists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continuous ritual use of Hebrew across generations and diverse communities, ensuring its sacred status, textual transmission, and role as a unifying element of Jewish identity.
% TRANSFER_FUNCTION: Transfers religious, cultural, and historical continuity across generations, from ancient texts and traditions to contemporary communities, through the medium of Hebrew.
% ABSENT_VOICES: Secular linguists who define language vitality purely by the number of native speakers, or proponents of modern Hebrew who prioritize daily spoken use over ritual use, are not central to this reading's definition of vitality. They would argue that liturgical use alone is insufficient for true linguistic 'life'.
% DISAPPEARANCE_RATIONALE: If the constraint of liturgical Hebrew use vanished, the continuous thread of Jewish religious and cultural identity, deeply intertwined with its sacred language, would be fundamentally broken. The religious landscape and communal practices would reorganize around other linguistic or cultural anchors, or fragment significantly.
% FOUNDING_PROBLEM: The need to preserve Hebrew as a sacred language and a continuous link to ancient texts, religious practice, and tradition, despite the historical loss of daily spoken use.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic texts, historical liturgical practice, and the ongoing theological and communal emphasis on Hebrew's sacred role, attested by religious scholars and community leaders across various Jewish denominations.
narrative_ontology:disappearance_verdict(hebrew_vitality__liturgical_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__liturgical_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__liturgical_reading_tests).
:- end_tests(hebrew_vitality__liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low because the constraint primarily involves voluntary participation in religious practice, with minimal material cost imposed. Suppression is low as adherence is driven by communal identity and religious commitment, not coercion. Theater ratio is low because the liturgical use is genuine and central to the stated purpose. Accessibility collapse is moderate, as alternatives for 'language vitality' (e.g., secular spoken use) exist, but are not relevant to this specific definition of vitality. Resistance is low due to broad acceptance within the communities that uphold this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, the constraint is a beneficial coordination mechanism. However, from the perspective of other readings (e.g., 'native_daily_reading'), this constraint might be seen as insufficient or even a distraction from what they consider 'true' language vitality. The engine's per-seat classification would reflect the low extraction for participants in this reading, while other readings would generate different classifications for their own constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities are the agenda-setters and beneficiaries, as they define and transmit the tradition, deriving authority and continuity from it. Jewish communities are beneficiaries, gaining spiritual and cultural cohesion. There are no direct victims, as participation is voluntary and perceived as beneficial within this framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_vitality,
    'Is ''vitality'' for a language constituted by unbroken liturgical use, or does it require native speakers and daily vernacular use?',
    'Conceptual analysis and community consensus on the definition of ''language vitality'' within different cultural and linguistic frameworks. Empirical data on language use patterns would inform, but not resolve, the normative definition.',
    'If vitality is defined by native speakers, this constraint''s claim to ''vitality'' would be reclassified as ''preservation'' or ''ritual continuity'' rather than ''life'', potentially shifting its perceived function from a ''rope'' of vitality to a ''piton'' of ritual maintenance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_vitality, conceptual, 'Ambiguity in the definition of ''language vitality'' for Hebrew.').

omega_variable(
    sufficiency_of_liturgical_use,
    'Is liturgical use alone sufficient for the ''vitality'' of Hebrew, or is it a necessary but insufficient condition, requiring a modern spoken component?',
    'Comparative study of language revitalization efforts and their outcomes, alongside ongoing community discourse and the evolving role of modern Hebrew in daily life.',
    'If liturgical use is deemed insufficient, this constraint''s classification might shift from a ''rope'' of full vitality to a ''scaffold'' for a broader revival, or a ''piton'' if its function is seen as having atrophied relative to a more comprehensive definition of vitality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_of_liturgical_use, empirical, 'Whether liturgical use is a sufficient condition for Hebrew''s vitality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__liturgical_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebr_tr_t25, hebrew_vitality__liturgical_reading, theater_ratio, 25, 0.05).
narrative_ontology:measurement(hebr_tr_t50, hebrew_vitality__liturgical_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(hebr_tr_t75, hebrew_vitality__liturgical_reading, theater_ratio, 75, 0.05).
narrative_ontology:measurement(hebr_tr_t100, hebrew_vitality__liturgical_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__liturgical_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hebr_be_t25, hebrew_vitality__liturgical_reading, base_extractiveness, 25, 0.15).
narrative_ontology:measurement(hebr_be_t50, hebrew_vitality__liturgical_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(hebr_be_t75, hebrew_vitality__liturgical_reading, base_extractiveness, 75, 0.15).
narrative_ontology:measurement(hebr_be_t100, hebrew_vitality__liturgical_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_vitality__liturgical_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(hebr_su_t25, hebrew_vitality__liturgical_reading, suppression_requirement, 25, 0.1).
narrative_ontology:measurement(hebr_su_t50, hebrew_vitality__liturgical_reading, suppression_requirement, 50, 0.1).
narrative_ontology:measurement(hebr_su_t75, hebrew_vitality__liturgical_reading, suppression_requirement, 75, 0.1).
narrative_ontology:measurement(hebr_su_t100, hebrew_vitality__liturgical_reading, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__liturgical_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__native_daily_reading).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_vitality' kernel. This 'liturgical_reading' emphasizes ritual preservation as vitality, distinct from readings focused on native daily use or a hybrid approach.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
