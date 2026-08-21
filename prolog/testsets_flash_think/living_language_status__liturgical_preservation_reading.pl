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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Liturgical Preservation as Living Language Status
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint defines a language as 'living' if its sacred texts are
 *   continuously recited, studied, and used in ritual, asserting that
 *   liturgical transmission alone suffices for vitality. This is one reading
 *   of the broader 'living_language_status' kernel, which is contested by
 *   other definitions emphasizing native generational transmission or modern
 *   literary output. The constraint primarily functions as a coordination
 *   mechanism for religious communities to maintain the sacred status of
 *   their languages, with low extraction but significant implications for
 *   those whose linguistic practices fall outside this definition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.15).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.3).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Liturgical Preservation as Living Language Status").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, '3d4ff6b1-7acc-4209-a72b-aa6bac5bbd53').
narrative_ontology:cs_kernel_codification('3d4ff6b1-7acc-4209-a72b-aa6bac5bbd53', fixed_text).
narrative_ontology:cs_authority_grounding('3d4ff6b1-7acc-4209-a72b-aa6bac5bbd53', lineage).
narrative_ontology:cs_interpretation_layer_present('3d4ff6b1-7acc-4209-a72b-aa6bac5bbd53').
narrative_ontology:cs_reading_relation('3d4ff6b1-7acc-4209-a72b-aa6bac5bbd53', living_language_status__native_generation_reading, forecloses).
narrative_ontology:cs_reading_relation('3d4ff6b1-7acc-4209-a72b-aa6bac5bbd53', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('3d4ff6b1-7acc-4209-a72b-aa6bac5bbd53', foundational, sacred_text_transmission_is_life).
narrative_ontology:cs_axiom_status(sacred_text_transmission_is_life, holdable).
narrative_ontology:cs_axiom_grounding('3d4ff6b1-7acc-4209-a72b-aa6bac5bbd53', sacred_text_transmission_is_life, theological).
narrative_ontology:cs_axiom('3d4ff6b1-7acc-4209-a72b-aa6bac5bbd53', secondary, ritual_use_maintains_vitality).
narrative_ontology:cs_axiom_status(ritual_use_maintains_vitality, holdable).
narrative_ontology:cs_axiom_grounding('3d4ff6b1-7acc-4209-a72b-aa6bac5bbd53', ritual_use_maintains_vitality, conventional).
narrative_ontology:cs_reference_frame('3d4ff6b1-7acc-4209-a72b-aa6bac5bbd53', ancient_liturgical_tradition).
narrative_ontology:cs_drift_state('3d4ff6b1-7acc-4209-a72b-aa6bac5bbd53', contemporary_sociolinguistics_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3d4ff6b1-7acc-4209-a72b-aa6bac5bbd53', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, liturgical_practitioners).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_speech_community).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, traditional_authority_of_texts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Custodians of sacred texts and liturgical traditions. Their authority and interpretive monopoly are preserved by this definition of a living language, as it centers their domain of expertise and practice. They actively promote and maintain the liturgical transmission.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Individuals and communities who continuously recite, study, and use sacred texts in ritual. They benefit from the validation of their practices as the primary means of language preservation and vitality, reinforcing their cultural and religious identity.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, liturgical_practitioners, beneficiary,
    moderate, biographical, identity_locked, regional).

% Speakers who use the language in daily, non-liturgical contexts, or for modern literary creation. They are implicitly delegitimized by this constraint, as their usage is not considered sufficient for 'living' status, potentially marginalizing their contributions and cultural expressions.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_speech_community, payer,
    moderate, biographical, constrained, national).

% Academics who study language vitality from a scientific perspective, often emphasizing native speaker transmission or productive literary output. They analyze this constraint as a cultural phenomenon but do not directly participate in its enforcement or benefit from its operation.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, linguistic_scholars, observer,
    analytical, biographical, analytical, global).

% Groups who argue that a language is only truly living if transmitted generationally as a mother tongue. They are excluded from the definition of vitality promoted by this constraint, viewing liturgical use as insufficient or even a sign of linguistic dormancy.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, native_generation_advocates, excluded,
    organized, generational, constrained, global).

% Groups who define a living language by its capacity for new literary and intellectual production. They are excluded from this constraint's definition, as their criteria for vitality are distinct from, though not necessarily opposed to, liturgical preservation.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, literary_continuity_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:fixing_cost_class(living_language_status__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, stable framework for defining and maintaining the 'living' status of a language, particularly for those with a strong connection to sacred texts and traditions, by centering liturgical transmission.
% TRANSFER_FUNCTION: Transfers legitimacy and cultural authority regarding language vitality to religious institutions and practices, and away from secular or purely demographic criteria.
% ABSENT_VOICES: Advocates for native generational transmission and modern literary continuity are absent from the conversation that defines language vitality through liturgical preservation. They would argue for broader, more inclusive definitions of 'living' status.
% DISAPPEARANCE_RATIONALE: If this definition vanished, the status of many historically significant languages (e.g., Latin, Classical Arabic, Ge'ez) would be re-evaluated. Languages currently considered 'living' due to liturgical use might be reclassified as 'dead' or 'ritual languages,' shifting academic and cultural discourse significantly.
% FOUNDING_PROBLEM: To establish a clear and enduring criterion for the vitality of languages primarily preserved through religious texts and ritual, ensuring their continued relevance and sacred status.
% FOUNDING_PROBLEM_CORROBORATION: Religious scholars and traditional communities attest that the problem of maintaining the sacred status and perceived vitality of these languages is ongoing. Sociolinguists and secular cultural critics, while disagreeing with the solution, acknowledge the historical and cultural impetus behind such definitions.
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__liturgical_preservation_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.15) because the primary function is coordination around a shared cultural/religious practice, not material extraction. Suppression is moderate (0.3) as it implicitly delegitimizes alternative definitions of vitality, but does not actively coerce non-compliance. Theater ratio is moderate (0.4) reflecting the performative aspect of ritual, which is integral to its function. Accessibility collapse is high (0.8) for those who accept this premise, as other paths to 'living' status are deemed insufficient. Resistance is low (0.2) within the framework of those who adhere to this reading, though external resistance from other readings is significant.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinical authority and liturgical practitioners, this constraint is a vital rope, ensuring the continuity and sacred status of their language. From the perspective of secular speech communities or advocates for native generational transmission, it may appear as a subtle snare, delegitimizing their linguistic practices and potentially hindering broader revitalization efforts.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinical authority and liturgical practitioners are beneficiaries, as their roles and practices are validated and centered by this definition. The secular speech community is a payer/victim, as their forms of language use are implicitly devalued. Linguistic scholars act as observers, analyzing the constraint's cultural and social effects.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate (preserving sacred language vitality through ritual) is continuously fulfilled by ongoing practice. The contestation arises from alternative definitions of 'living' rather than a decay of its original function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a complete and independent definition of ''living language status'', or is it one reading of a contested kernel?',
    'Analysis of broader sociolinguistic discourse and competing definitions of language vitality, identifying the ''living_language_status'' kernel and its various readings.',
    'If it is one reading, its classification must be understood in relation to sibling readings, and its ''truth'' is relative to the interpretive community. If independent, its classification stands alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''liturgical_preservation_reading'' of the ''living_language_status'' kernel.').

omega_variable(
    legitimacy_of_secular_use,
    'Does the emphasis on liturgical preservation implicitly delegitimize or hinder the development of secular, modern uses of the language, or can both coexist without conflict?',
    'Empirical study of language use patterns in communities adhering to this definition, observing trends in secular literary production, media, and daily conversation, and interviewing speakers about perceived legitimacy.',
    'If secular use is significantly hindered, the constraint''s effective suppression and extractiveness for the secular speech community would be higher than currently measured, potentially shifting its classification towards a Tangled Rope or Snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_secular_use, empirical, 'Impact of liturgical emphasis on secular language development.').

omega_variable(
    actual_linguistic_vitality,
    'Does continuous liturgical transmission genuinely reflect a language''s vitality, or is it a form of preservation that maintains a ''sacred corpse'' without true living dynamism?',
    'Comparative sociolinguistic analysis of languages with similar liturgical preservation, contrasting them with languages that have robust native speaker communities or vibrant modern literary traditions, using metrics beyond ritual use.',
    'If liturgical use is found to be insufficient for broader vitality, the ''rope'' classification would be challenged, as its coordination function would be seen as maintaining a static form rather than a truly ''living'' language, potentially reclassifying it as a Piton or even a Snare (if the claim of ''living'' is seen as a cover for institutional control).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actual_linguistic_vitality, conceptual, 'Whether liturgical use equates to true linguistic vitality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(livi_tr_t10, living_language_status__liturgical_preservation_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(livi_tr_t20, living_language_status__liturgical_preservation_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(livi_tr_t30, living_language_status__liturgical_preservation_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(livi_tr_t40, living_language_status__liturgical_preservation_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(livi_tr_t50, living_language_status__liturgical_preservation_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(livi_be_t10, living_language_status__liturgical_preservation_reading, base_extractiveness, 10, 0.13).
narrative_ontology:measurement(livi_be_t20, living_language_status__liturgical_preservation_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(livi_be_t30, living_language_status__liturgical_preservation_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(livi_be_t40, living_language_status__liturgical_preservation_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(livi_be_t50, living_language_status__liturgical_preservation_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__liturgical_preservation_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(livi_su_t10, living_language_status__liturgical_preservation_reading, suppression_requirement, 10, 0.29).
narrative_ontology:measurement(livi_su_t20, living_language_status__liturgical_preservation_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(livi_su_t30, living_language_status__liturgical_preservation_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(livi_su_t40, living_language_status__liturgical_preservation_reading, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(livi_su_t50, living_language_status__liturgical_preservation_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'living_language_status' kernel, which is decomposed into three distinct constraint stories to capture different structural properties and ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
