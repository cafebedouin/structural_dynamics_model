% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__liturgical_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__liturgical_preservation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_continuity__liturgical_preservation
 *   human_readable: Hebrew Continuity via Liturgical Preservation
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the preservation of Hebrew through its use in
 *   religious liturgy and scholarly textual transmission, a reading of the
 *   'hebrew_continuity' kernel. It emphasizes symbolic preservation and
 *   ritual recitation over daily generative use. The constraint ensures that
 *   Hebrew remains accessible for religious purposes even without a native
 *   speaker base. The core tension is between this mode of preservation and
 *   the forces of secularization or alternative revitalization efforts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.15).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.2).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Hebrew Continuity via Liturgical Preservation").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, 'dc5c46eb-e652-40cf-afc0-1d8c3d0d96cc').
narrative_ontology:cs_kernel_codification('dc5c46eb-e652-40cf-afc0-1d8c3d0d96cc', fixed_text).
narrative_ontology:cs_authority_grounding('dc5c46eb-e652-40cf-afc0-1d8c3d0d96cc', lineage).
narrative_ontology:cs_interpretation_layer_present('dc5c46eb-e652-40cf-afc0-1d8c3d0d96cc').
narrative_ontology:cs_reading_relation('dc5c46eb-e652-40cf-afc0-1d8c3d0d96cc', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_reading_relation('dc5c46eb-e652-40cf-afc0-1d8c3d0d96cc', hebrew_continuity__bridge_pidginized, coexists_with).
narrative_ontology:cs_axiom('dc5c46eb-e652-40cf-afc0-1d8c3d0d96cc', foundational, hebrew_lives_through_sacred_text).
narrative_ontology:cs_axiom_status(hebrew_lives_through_sacred_text, holdable).
narrative_ontology:cs_axiom_grounding('dc5c46eb-e652-40cf-afc0-1d8c3d0d96cc', hebrew_lives_through_sacred_text, theological).
narrative_ontology:cs_axiom('dc5c46eb-e652-40cf-afc0-1d8c3d0d96cc', foundational, ritual_recitation_sustains_language).
narrative_ontology:cs_axiom_status(ritual_recitation_sustains_language, holdable).
narrative_ontology:cs_axiom_grounding('dc5c46eb-e652-40cf-afc0-1d8c3d0d96cc', ritual_recitation_sustains_language, conventional).
narrative_ontology:cs_reference_frame('dc5c46eb-e652-40cf-afc0-1d8c3d0d96cc', ancient_liturgical_tradition).
narrative_ontology:cs_drift_state('dc5c46eb-e652-40cf-afc0-1d8c3d0d96cc', contemporary_secular_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dc5c46eb-e652-40cf-afc0-1d8c3d0d96cc', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, religious_scholars).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, liturgical_communities).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, secularizing_forces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Custodians of the Hebrew textual tradition, responsible for its accurate transmission, interpretation, and ritual use. Their professional and spiritual identity is deeply intertwined with the preservation of liturgical Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, religious_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Participate in rituals and prayers conducted in Hebrew, deriving spiritual and communal identity from this practice. The constraint provides a stable, shared linguistic foundation for their religious life.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, liturgical_communities, beneficiary,
    organized, generational, identity_locked, global).

% Represent cultural and social trends that de-emphasize traditional religious practice and textual study, leading to reduced engagement with liturgical Hebrew. They bear the 'cost' of maintaining a separate linguistic tradition that may seem archaic or irrelevant to modern life.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secularizing_forces, payer,
    powerful, generational, mobile, global).

% Advocate for Hebrew as a living, generative language, often viewing purely liturgical preservation as insufficient for true linguistic vitality. Their efforts are often distinct from, and sometimes in tension with, the liturgical preservation model.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, linguistic_revivalists, excluded,
    moderate, biographical, constrained, national).

% Study the historical development and sociolinguistic dynamics of Hebrew, including its liturgical and modern forms, without direct participation in its preservation or revitalization efforts.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, analytical_linguists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, shared linguistic medium for religious ritual, textual study, and communal identity across generations and geographies, ensuring continuity of tradition.
% TRANSFER_FUNCTION: Transfers the responsibility and resources for Hebrew language maintenance from individual generative use to institutionalized ritual and scholarly transmission, preserving a symbolic connection to the past.
% ABSENT_VOICES: Advocates for Hebrew as a purely generative, native language are often excluded from the discourse of liturgical preservation, as their focus on spoken fluency and modern usage can be seen as a threat to the sanctity of the traditional text. They would argue that a language truly 'lives' only when spoken daily.
% DISAPPEARANCE_RATIONALE: If liturgical preservation vanished, the continuity of Jewish religious practice and scholarly tradition would be severely disrupted. Communities would lose a core element of their shared identity, and the transmission of sacred texts would become fragmented, leading to a profound cultural and spiritual reorganization.
% FOUNDING_PROBLEM: The historical challenge of maintaining Hebrew as a sacred language after the loss of its native speakers, ensuring its survival and relevance for religious and cultural identity.
% FOUNDING_PROBLEM_CORROBORATION: Religious leaders and scholars universally attest to the ongoing challenge of maintaining liturgical Hebrew in a rapidly secularizing world. Historical records and demographic studies from outside the benefiting parties corroborate the continuous effort required to prevent linguistic attrition in diaspora communities.
narrative_ontology:disappearance_verdict(hebrew_continuity__liturgical_preservation, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__liturgical_preservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_continuity__liturgical_preservation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__liturgical_preservation_tests).
:- end_tests(hebrew_continuity__liturgical_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the primary goal is preservation, not material gain, though it requires significant communal investment. Suppression is also low (0.20) as participation is largely voluntary, driven by identity and belief, rather than coercion. Theater ratio is moderate (0.40) reflecting the performative aspect of ritual recitation, which is functional for identity but not for generative language use. Accessibility collapse is high (0.70) because for those outside the liturgical tradition, access to Hebrew is largely through academic study, not organic immersion. Resistance is low (0.10) as direct opposition to liturgical Hebrew is rare; most 'resistance' comes from indifference or competing priorities.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious scholars, this constraint is a vital Rope, a necessary coordination mechanism for cultural and spiritual survival. From the perspective of secularizing forces, it might appear as a Piton, an archaic practice maintained by inertia. The engine's classification will reflect the overall structural reality, which is closer to a Rope due to its genuine coordination function and relatively low extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious scholars and liturgical communities are beneficiaries, as the constraint directly supports their spiritual and communal life. Secularizing forces are victims, as they represent the erosion of the traditional context that sustains liturgical Hebrew. Linguistic revivalists are excluded, as their focus on modern, generative Hebrew often operates on a different axis than liturgical preservation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_vs_generative_vitality,
    'Is liturgical preservation sufficient for the ''life'' of a language, or does true linguistic vitality require native, generative speakers?',
    'Conceptual analysis of ''language vitality'' definitions, and empirical study of language communities that have maintained identity through liturgical use without native speakers.',
    'If generative use is deemed essential, this constraint''s claim to ''continuity'' is weakened, potentially reclassifying it as a more theatrical or inertial form of preservation (Piton). If liturgical use is sufficient, its Rope classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liturgical_vs_generative_vitality, conceptual, 'The definitional boundary of language ''life'' in the context of Hebrew.').

omega_variable(
    secularization_impact_threshold,
    'At what point do secularizing forces become an active ''victim'' rather than a passive ''payer'' of the constraint?',
    'Empirical study of community engagement with liturgical Hebrew over time, correlating with measures of secularization. Identify thresholds where active resistance or abandonment of practice becomes widespread.',
    'If secularization reaches a critical threshold, the constraint''s suppression requirement might increase, or its coordination function might collapse, potentially shifting its classification towards a Snare or Piton if maintenance becomes purely coercive or inertial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secularization_impact_threshold, empirical, 'The dynamic threshold at which secularization actively threatens liturgical preservation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 1000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1000, hebrew_continuity__liturgical_preservation, theater_ratio, 1000, 0.2).
narrative_ontology:measurement(hebr_tr_t1300, hebrew_continuity__liturgical_preservation, theater_ratio, 1300, 0.25).
narrative_ontology:measurement(hebr_tr_t1600, hebrew_continuity__liturgical_preservation, theater_ratio, 1600, 0.3).
narrative_ontology:measurement(hebr_tr_t1800, hebrew_continuity__liturgical_preservation, theater_ratio, 1800, 0.35).
narrative_ontology:measurement(hebr_tr_t1950, hebrew_continuity__liturgical_preservation, theater_ratio, 1950, 0.45).
narrative_ontology:measurement(hebr_tr_t2024, hebrew_continuity__liturgical_preservation, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1000, hebrew_continuity__liturgical_preservation, base_extractiveness, 1000, 0.1).
narrative_ontology:measurement(hebr_be_t1300, hebrew_continuity__liturgical_preservation, base_extractiveness, 1300, 0.12).
narrative_ontology:measurement(hebr_be_t1600, hebrew_continuity__liturgical_preservation, base_extractiveness, 1600, 0.14).
narrative_ontology:measurement(hebr_be_t1800, hebrew_continuity__liturgical_preservation, base_extractiveness, 1800, 0.16).
narrative_ontology:measurement(hebr_be_t1950, hebrew_continuity__liturgical_preservation, base_extractiveness, 1950, 0.18).
narrative_ontology:measurement(hebr_be_t2024, hebrew_continuity__liturgical_preservation, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1000, hebrew_continuity__liturgical_preservation, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement(hebr_su_t1300, hebrew_continuity__liturgical_preservation, suppression_requirement, 1300, 0.12).
narrative_ontology:measurement(hebr_su_t1600, hebrew_continuity__liturgical_preservation, suppression_requirement, 1600, 0.15).
narrative_ontology:measurement(hebr_su_t1800, hebrew_continuity__liturgical_preservation, suppression_requirement, 1800, 0.18).
narrative_ontology:measurement(hebr_su_t1950, hebrew_continuity__liturgical_preservation, suppression_requirement, 1950, 0.22).
narrative_ontology:measurement(hebr_su_t2024, hebrew_continuity__liturgical_preservation, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_continuity' kernel, focusing on liturgical and textual preservation. It influences and coexists with other readings that emphasize generative use or pidginized forms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
