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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_continuity__liturgical_preservation
 *   human_readable: Hebrew Continuity via Liturgical Preservation
 *   domain: sociolinguistics/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the preservation of Hebrew primarily through
 *   its use in religious liturgy, ritual, and textual study, rather than
 *   through everyday spoken use. It is a reading of the 'hebrew_continuity'
 *   kernel, focusing on symbolic and ritualistic survival. The constraint is
 *   claimed as a Rope because it genuinely coordinates a collective good
 *   (language preservation) with low extraction, but the metrics reflect the
 *   ongoing effort and minor costs associated with maintaining a
 *   non-generative language against secularizing pressures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.15).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.05).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Hebrew Continuity via Liturgical Preservation").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistics/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, '1d1f05b4-73d1-441b-a97e-47af3d664dfa').
narrative_ontology:cs_kernel_codification('1d1f05b4-73d1-441b-a97e-47af3d664dfa', fixed_text).
narrative_ontology:cs_authority_grounding('1d1f05b4-73d1-441b-a97e-47af3d664dfa', lineage).
narrative_ontology:cs_interpretation_layer_present('1d1f05b4-73d1-441b-a97e-47af3d664dfa').
narrative_ontology:cs_reading_relation('1d1f05b4-73d1-441b-a97e-47af3d664dfa', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_reading_relation('1d1f05b4-73d1-441b-a97e-47af3d664dfa', hebrew_continuity__bridge_pidginized, coexists_with).
narrative_ontology:cs_axiom('1d1f05b4-73d1-441b-a97e-47af3d664dfa', foundational, sacred_text_preservation_is_life).
narrative_ontology:cs_axiom_status(sacred_text_preservation_is_life, holdable).
narrative_ontology:cs_axiom_grounding('1d1f05b4-73d1-441b-a97e-47af3d664dfa', sacred_text_preservation_is_life, theological).
narrative_ontology:cs_axiom('1d1f05b4-73d1-441b-a97e-47af3d664dfa', foundational, ritual_recitation_sustains_language).
narrative_ontology:cs_axiom_status(ritual_recitation_sustains_language, holdable).
narrative_ontology:cs_axiom_grounding('1d1f05b4-73d1-441b-a97e-47af3d664dfa', ritual_recitation_sustains_language, conventional).
narrative_ontology:cs_reference_frame('1d1f05b4-73d1-441b-a97e-47af3d664dfa', ancient_liturgical_tradition).
narrative_ontology:cs_drift_state('1d1f05b4-73d1-441b-a97e-47af3d664dfa', contemporary_secular_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('1d1f05b4-73d1-441b-a97e-47af3d664dfa', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, religious_communities).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, scholars_of_hebraic_texts).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, secularizing_forces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively maintain Hebrew through prayer, study, and ritual. Their identity is deeply intertwined with the language's preservation, viewing it as a sacred duty. They benefit from the continuity of tradition and access to sacred texts.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, religious_communities, agenda_setter,
    organized, generational, identity_locked, global).

% Benefit from the continued existence of a textual tradition and a community that can interpret it. Their academic work depends on the accessibility and study of these texts, which is enabled by liturgical preservation.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, scholars_of_hebraic_texts, beneficiary,
    moderate, biographical, constrained, global).

% Represent societal trends that de-emphasize religious practice and traditional textual study, leading to a decline in engagement with liturgical Hebrew. They bear the 'cost' of maintaining a language that is not part of modern daily life, often through cultural drift rather than direct payment.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secularizing_forces, payer,
    institutional, generational, mobile, global).

% Advocate for Hebrew as a living, generative language, often viewing liturgical preservation as insufficient or even hindering full revitalization. They are excluded from the core definition of 'Hebrew lives' within this reading, which prioritizes ritual over everyday speech.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, linguistic_revivalists, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation of Hebrew as a sacred language across dispersed Jewish communities, ensuring continuity of religious practice and access to foundational texts through shared ritual and study.
% TRANSFER_FUNCTION: Transfers the responsibility and effort of language maintenance from individual generative use to collective ritual and scholarly transmission, from religious communities to future generations.
% ABSENT_VOICES: Advocates for Hebrew as a modern, generative language (e.g., Zionist revivalists) would argue that mere liturgical preservation is insufficient for true linguistic vitality. They are absent from this reading's definition of 'Hebrew lives' because their focus is on spoken, everyday use rather than ritual transmission.
% DISAPPEARANCE_RATIONALE: If liturgical preservation vanished, the continuity of Jewish religious practice and textual scholarship would be severely disrupted. Access to sacred texts would become a purely academic exercise, and a core element of Jewish identity would be lost, forcing a profound reorganization of religious and cultural life.
% FOUNDING_PROBLEM: The problem of maintaining Hebrew as a sacred language and a link to ancient texts across centuries of diaspora, without a continuous native-speaking population.
% FOUNDING_PROBLEM_CORROBORATION: Religious leaders and scholars universally attest that the problem of maintaining textual and ritual continuity remains live, as secularization and assimilation continue to pose threats to traditional practices. This is corroborated by demographic studies of religious observance and linguistic engagement within Jewish communities globally, from outside the directly benefiting parties.
narrative_ontology:disappearance_verdict(hebrew_continuity__liturgical_preservation, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__liturgical_preservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_continuity__liturgical_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__liturgical_preservation, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.15) because the primary 'cost' is the effort of learning and maintaining a liturgical language, which is largely self-imposed by religious communities for identity and spiritual benefit. Suppression is very low (0.05) as there's no active coercion; the 'victims' (secularizing forces) are diffuse societal trends, not agents being actively suppressed. Theater ratio is low (0.1) because the ritual recitation is genuinely functional for its purpose, not merely performative. Accessibility collapse is high (0.8) because for those outside the religious tradition, the language is largely inaccessible without significant effort. Resistance is low (0.1) because the primary 'resistance' comes from cultural drift, not active opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious communities, this is a vital, low-cost coordination mechanism. From the perspective of secularizing forces, it's an anachronistic effort with diffuse costs. The engine's classification will reflect the low extraction and suppression, aligning with the Rope claim, but the omegas capture the conceptual contestation.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious communities are the agenda-setters and primary beneficiaries, as they actively maintain the language and derive identity and spiritual value from it. Scholars of Hebraic texts are beneficiaries, as their field relies on this preservation. Secularizing forces are the 'victims' in the sense that their cultural trends erode the constraint, but they are not actively paying or being suppressed by it. Linguistic revivalists are excluded, as their vision of Hebrew's life differs from this reading's focus.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_language_life,
    'Is ''liturgical preservation'' a sufficient condition for a language to be considered ''alive'', or does ''life'' require generative native speakers?',
    'Conceptual clarification within sociolinguistics and philosophy of language, or a shift in cultural consensus regarding the purpose of language preservation.',
    'If generative use is deemed essential, this constraint would be reclassified as a Piton (maintaining a ''dead'' language theatrically) or Snare (if it actively suppresses generative efforts). If liturgical use is sufficient, its Rope classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_language_life, conceptual, 'Ambiguity in the definition of ''language life'' for a sacred language.').

omega_variable(
    secularization_impact_measurement,
    'What is the precise, measurable impact of ''secularizing forces'' on the actual practice of liturgical Hebrew, beyond anecdotal observation?',
    'Longitudinal demographic studies tracking rates of Hebrew literacy, prayer attendance, and engagement with religious texts across generations in various communities.',
    'If the impact is negligible, the ''victim'' status of secularizing forces is overstated. If severe, it highlights the ongoing effort required to maintain the constraint, potentially shifting its extractiveness upwards for religious communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secularization_impact_measurement, empirical, 'Quantifying the ''cost'' imposed by secularizing forces on liturgical Hebrew.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__liturgical_preservation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebr_tr_t25, hebrew_continuity__liturgical_preservation, theater_ratio, 25, 0.09).
narrative_ontology:measurement(hebr_tr_t50, hebrew_continuity__liturgical_preservation, theater_ratio, 50, 0.1).
narrative_ontology:measurement(hebr_tr_t75, hebrew_continuity__liturgical_preservation, theater_ratio, 75, 0.11).
narrative_ontology:measurement(hebr_tr_t100, hebrew_continuity__liturgical_preservation, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__liturgical_preservation, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(hebr_be_t25, hebrew_continuity__liturgical_preservation, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(hebr_be_t50, hebrew_continuity__liturgical_preservation, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(hebr_be_t75, hebrew_continuity__liturgical_preservation, base_extractiveness, 75, 0.14).
narrative_ontology:measurement(hebr_be_t100, hebrew_continuity__liturgical_preservation, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__liturgical_preservation, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(hebr_su_t25, hebrew_continuity__liturgical_preservation, suppression_requirement, 25, 0.05).
narrative_ontology:measurement(hebr_su_t50, hebrew_continuity__liturgical_preservation, suppression_requirement, 50, 0.05).
narrative_ontology:measurement(hebr_su_t75, hebrew_continuity__liturgical_preservation, suppression_requirement, 75, 0.05).
narrative_ontology:measurement(hebr_su_t100, hebrew_continuity__liturgical_preservation, suppression_requirement, 100, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_continuity' kernel. This reading focuses on liturgical and textual preservation, distinct from generative use or pidginized contact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
