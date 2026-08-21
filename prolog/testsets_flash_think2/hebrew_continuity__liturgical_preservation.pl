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
 *   human_readable: Hebrew Continuity through Liturgical and Textual Preservation
 *   domain: sociolinguistics/cultural_preservation/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the preservation of Hebrew through its use in
 *   religious ritual and the transmission of sacred texts, a mode of
 *   continuity that does not require native speakers. It is one reading of
 *   the broader 'hebrew_continuity' kernel, focusing on symbolic and
 *   ritualistic maintenance against secularizing and assimilationist
 *   pressures. The constraint coordinates identity and cultural transmission
 *   but demands significant adherence and actively resists alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.48).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.55).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.48).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Hebrew Continuity through Liturgical and Textual Preservation").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistics/cultural_preservation/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__liturgical_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, 'bacb14e0-6aad-4a7e-957a-ebe758639d8f').
narrative_ontology:cs_kernel_codification('bacb14e0-6aad-4a7e-957a-ebe758639d8f', fixed_text).
narrative_ontology:cs_authority_grounding('bacb14e0-6aad-4a7e-957a-ebe758639d8f', lineage).
narrative_ontology:cs_interpretation_layer_present('bacb14e0-6aad-4a7e-957a-ebe758639d8f').
narrative_ontology:cs_reading_relation('bacb14e0-6aad-4a7e-957a-ebe758639d8f', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_reading_relation('bacb14e0-6aad-4a7e-957a-ebe758639d8f', hebrew_continuity__bridge_pidginized, coexists_with).
narrative_ontology:cs_axiom('bacb14e0-6aad-4a7e-957a-ebe758639d8f', foundational, hebrew_is_sacred_language).
narrative_ontology:cs_axiom_status(hebrew_is_sacred_language, holdable).
narrative_ontology:cs_axiom_grounding('bacb14e0-6aad-4a7e-957a-ebe758639d8f', hebrew_is_sacred_language, theological).
narrative_ontology:cs_axiom('bacb14e0-6aad-4a7e-957a-ebe758639d8f', foundational, continuity_through_text_and_ritual).
narrative_ontology:cs_axiom_status(continuity_through_text_and_ritual, holdable).
narrative_ontology:cs_axiom_grounding('bacb14e0-6aad-4a7e-957a-ebe758639d8f', continuity_through_text_and_ritual, conventional).
narrative_ontology:cs_reference_frame('bacb14e0-6aad-4a7e-957a-ebe758639d8f', diaspora_liturgical_tradition).
narrative_ontology:cs_drift_state('bacb14e0-6aad-4a7e-957a-ebe758639d8f', contemporary_secular_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bacb14e0-6aad-4a7e-957a-ebe758639d8f', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, religious_communities).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, scholars_of_hebrew_texts).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, secularizing_forces).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, assimilationist_pressures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively maintain the tradition of Hebrew through ritual recitation, textual study, and communal adherence. They derive core identity and spiritual meaning from this continuity, and enforce norms to preserve it.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, religious_communities, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the preserved texts and rituals as objects of study and transmission. They contribute to the maintenance of the tradition through scholarship, teaching, and editing, but are not necessarily the primary enforcers of ritual adherence.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, scholars_of_hebrew_texts, beneficiary,
    organized, generational, constrained, global).

% Represent cultural and social trends that de-emphasize religious practice and traditional texts. Their influence is resisted by the constraint, as it threatens the continuity of the liturgical tradition. They bear the 'cost' of being actively opposed by the constraint's adherents.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secularizing_forces, payer,
    powerful, generational, mobile, global).

% Similar to secularizing forces, these pressures encourage integration into dominant cultures, often at the expense of distinct linguistic and religious practices. The constraint actively works to counter these pressures, making them 'victims' of its enforcement.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, assimilationist_pressures, payer,
    powerful, generational, mobile, global).

% Speak Hebrew as a daily, generative language, primarily in Israel. From the perspective of liturgical preservation, their mode of use is not the primary mechanism for the language's sacred continuity, and they may view liturgical use as archaic or insufficient. They are excluded from this reading's definition of 'living Hebrew'.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, modern_hebrew_speakers, excluded,
    moderate, biographical, mobile, national).

% Study the phenomenon of language preservation, revitalization, and the sociolinguistics of Hebrew without direct participation in its religious or cultural maintenance. They analyze the mechanisms and outcomes of the constraint.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, analytical_linguists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the Hebrew language and associated cultural/religious identity across generations by coordinating ritual practice, textual study, and communal adherence to a shared tradition.
% TRANSFER_FUNCTION: Transfers cultural and religious continuity, identity, and spiritual meaning to adherents, in exchange for their adherence, effort, and the exclusion of alternative modes of linguistic engagement that might dilute the tradition.
% ABSENT_VOICES: Modern Hebrew speakers (who might argue for a different definition of 'living' Hebrew focused on generative use) and those who have assimilated or secularized (who might question the value or necessity of this mode of preservation).
% DISAPPEARANCE_RATIONALE: If the liturgical and textual preservation of Hebrew vanished overnight, the continuity of Hebrew as a sacred language would be severely disrupted, profoundly impacting religious practice, cultural identity, and the historical connection for many Jewish communities globally. The religious and cultural landscape would reorganize around this loss.
% FOUNDING_PROBLEM: The historical dispersion of Jewish people and the loss of Hebrew as a spoken vernacular, which threatened the continuity of religious practice, cultural identity, and the direct engagement with sacred texts.
% FOUNDING_PROBLEM_CORROBORATION: Religious leaders, cultural historians, and sociologists of religion (from outside the immediate benefiting communities) attest to the ongoing challenges of maintaining cultural and linguistic distinctiveness in diaspora, confirming the continued relevance of the founding problem.
narrative_ontology:disappearance_verdict(hebrew_continuity__liturgical_preservation, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__liturgical_preservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_continuity__liturgical_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__liturgical_preservation, 0.48, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__liturgical_preservation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__liturgical_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.48) reflects the significant commitment and adherence demanded from individuals and communities to maintain the tradition, including time, effort, and the exclusion of competing cultural influences. Suppression (0.55) arises from social pressure within communities to conform to ritual and textual norms, and the active resistance against external forces that threaten the tradition. The theater ratio (0.15) is low because the ritual and textual transmission are considered the core, functional aspects of this preservation, not mere performance. Accessibility collapse (0.75) is high for adherents, as abandoning this mode of preservation is seen as a loss of the language's sacred status. Resistance (0.60) is substantial due to ongoing external pressures from secularization and assimilation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious communities, this constraint is a vital rope for cultural and spiritual continuity. From the perspective of secularizing forces, it might appear as an anachronistic snare, actively resisting natural cultural evolution. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious communities are the primary agenda-setters and beneficiaries, actively maintaining the tradition and deriving identity from it. Scholars of Hebrew texts also benefit from the preserved corpus. Secularizing forces and assimilationist pressures are the targets/victims, as the constraint's enforcement actively works against their influence. Modern Hebrew speakers are excluded from this specific definition of 'living' Hebrew, as their generative use is not the primary mechanism of preservation here.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_living_language,
    'Is Hebrew truly ''living'' if it is primarily maintained through ritual and text, without a broad base of native, generative speakers?',
    'Conceptual analysis of sociolinguistic criteria for language vitality, potentially informed by comparative studies of other ''sacred'' or ''classical'' languages.',
    'If a ''living language'' is strictly defined by generative use, this reading''s claim to ''continuity'' might be reclassified as a form of ''archival maintenance'' rather than ''living preservation'', potentially shifting its classification towards a piton or even a snare if the demands for adherence are seen as disproportionate to the actual linguistic vitality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_living_language, conceptual, 'Ambiguity in the definition of a ''living language'' and its implications for Hebrew''s status.').

omega_variable(
    sufficiency_of_liturgical_preservation,
    'Is liturgical and textual preservation alone sufficient to ensure the long-term continuity and cultural relevance of Hebrew, or does it require a generative spoken component?',
    'Longitudinal sociolinguistic studies comparing communities relying solely on liturgical preservation versus those integrating modern spoken Hebrew, assessing cultural transmission and engagement over generations.',
    'If found insufficient, the constraint''s effectiveness as a ''rope'' for continuity would be undermined, potentially reclassifying it as a ''piton'' (if maintained theatrically despite functional atrophy) or a ''tangled_rope'' (if it extracts adherence without fully delivering on its stated goal of continuity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sufficiency_of_liturgical_preservation, empirical, 'Whether liturgical preservation is a functionally complete strategy for language continuity.').

omega_variable(
    victim_set_legitimacy,
    'Are ''secularizing_forces'' and ''assimilationist_pressures'' legitimate ''victims'' of this constraint, or are they natural societal processes that the constraint merely resists?',
    'Analysis of the normative framing: whether the constraint''s resistance is framed as defending an intrinsic good (making external pressures ''victims'') or as an attempt to halt natural change (making the constraint itself potentially extractive of individual autonomy).',
    'If these forces are not considered legitimate ''victims'', the constraint''s ''suppression'' metric might be re-evaluated as a more direct form of extraction from individual choice, potentially strengthening its ''snare'' characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_legitimacy, preference, 'Normative framing of external societal pressures as ''victims'' of the preservation effort.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__liturgical_preservation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebr_tr_t333, hebrew_continuity__liturgical_preservation, theater_ratio, 333, 0.11).
narrative_ontology:measurement(hebr_tr_t666, hebrew_continuity__liturgical_preservation, theater_ratio, 666, 0.12).
narrative_ontology:measurement(hebr_tr_t1000, hebrew_continuity__liturgical_preservation, theater_ratio, 1000, 0.13).
narrative_ontology:measurement(hebr_tr_t1333, hebrew_continuity__liturgical_preservation, theater_ratio, 1333, 0.14).
narrative_ontology:measurement(hebr_tr_t1666, hebrew_continuity__liturgical_preservation, theater_ratio, 1666, 0.14).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_continuity__liturgical_preservation, theater_ratio, 2000, 0.15).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__liturgical_preservation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hebr_be_t333, hebrew_continuity__liturgical_preservation, base_extractiveness, 333, 0.38).
narrative_ontology:measurement(hebr_be_t666, hebrew_continuity__liturgical_preservation, base_extractiveness, 666, 0.4).
narrative_ontology:measurement(hebr_be_t1000, hebrew_continuity__liturgical_preservation, base_extractiveness, 1000, 0.42).
narrative_ontology:measurement(hebr_be_t1333, hebrew_continuity__liturgical_preservation, base_extractiveness, 1333, 0.44).
narrative_ontology:measurement(hebr_be_t1666, hebrew_continuity__liturgical_preservation, base_extractiveness, 1666, 0.46).
narrative_ontology:measurement(hebr_be_t2000, hebrew_continuity__liturgical_preservation, base_extractiveness, 2000, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__liturgical_preservation, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(hebr_su_t333, hebrew_continuity__liturgical_preservation, suppression_requirement, 333, 0.43).
narrative_ontology:measurement(hebr_su_t666, hebrew_continuity__liturgical_preservation, suppression_requirement, 666, 0.46).
narrative_ontology:measurement(hebr_su_t1000, hebrew_continuity__liturgical_preservation, suppression_requirement, 1000, 0.49).
narrative_ontology:measurement(hebr_su_t1333, hebrew_continuity__liturgical_preservation, suppression_requirement, 1333, 0.52).
narrative_ontology:measurement(hebr_su_t1666, hebrew_continuity__liturgical_preservation, suppression_requirement, 1666, 0.54).
narrative_ontology:measurement(hebr_su_t2000, hebrew_continuity__liturgical_preservation, suppression_requirement, 2000, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
