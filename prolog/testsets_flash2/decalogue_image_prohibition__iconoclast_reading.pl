% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__iconoclast_reading, []).

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
 *   constraint_id: decalogue_image_prohibition__iconoclast_reading
 *   human_readable: Decalogue Image Prohibition (Iconoclast Reading)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This constraint represents the iconoclast reading of the Decalogue's
 *   image prohibition, asserting that any material representation used in
 *   worship constitutes idolatry. It is a wall-type constraint, categorically
 *   forbidding material mediation of the holy. The victim set includes icon
 *   producers, monastic communities, and devotional practitioners dependent
 *   on imagery. The primary beneficiary is centralizing imperial authority,
 *   which monopolizes religious form and eliminates rival centers of
 *   spiritual power. This is one reading of the 'decalogue_image_prohibition'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.85).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.92).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Decalogue Image Prohibition (Iconoclast Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, '7ba7a0c3-c07e-47d8-a845-cbb11e30cc94').
narrative_ontology:cs_kernel_codification('7ba7a0c3-c07e-47d8-a845-cbb11e30cc94', fixed_text).
narrative_ontology:cs_authority_grounding('7ba7a0c3-c07e-47d8-a845-cbb11e30cc94', lineage).
narrative_ontology:cs_interpretation_layer_present('7ba7a0c3-c07e-47d8-a845-cbb11e30cc94').
narrative_ontology:cs_reading_relation('7ba7a0c3-c07e-47d8-a845-cbb11e30cc94', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_reading_relation('7ba7a0c3-c07e-47d8-a845-cbb11e30cc94', decalogue_image_prohibition__moderate_iconoclast_reading, influences).
narrative_ontology:cs_axiom('7ba7a0c3-c07e-47d8-a845-cbb11e30cc94', foundational, material_mediation_is_idolatry).
narrative_ontology:cs_axiom_status(material_mediation_is_idolatry, holdable).
narrative_ontology:cs_axiom_grounding('7ba7a0c3-c07e-47d8-a845-cbb11e30cc94', material_mediation_is_idolatry, deontological).
narrative_ontology:cs_axiom('7ba7a0c3-c07e-47d8-a845-cbb11e30cc94', foundational, divine_transcendence_forbids_representation).
narrative_ontology:cs_axiom_status(divine_transcendence_forbids_representation, holdable).
narrative_ontology:cs_axiom_grounding('7ba7a0c3-c07e-47d8-a845-cbb11e30cc94', divine_transcendence_forbids_representation, theological).
narrative_ontology:cs_reference_frame('7ba7a0c3-c07e-47d8-a845-cbb11e30cc94', aniconic_purity_of_worship).
narrative_ontology:cs_drift_state('7ba7a0c3-c07e-47d8-a845-cbb11e30cc94', post_seventh_ecumenical_council, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('7ba7a0c3-c07e-47d8-a845-cbb11e30cc94', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, iconoclast_theologians).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_producers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the prohibition by removing alternative centers of religious authority and consolidating control over religious expression. Actively enforces the ban through decrees and destruction of images.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Their theological interpretations are vindicated and promoted by the prohibition. They provide the intellectual justification for the ban, gaining influence and patronage.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconoclast_theologians, beneficiary,
    powerful, generational, mobile, regional).

% Their livelihood is directly destroyed by the prohibition. They face economic ruin and potential persecution for continuing their craft. Exit means abandoning their trade and cultural heritage.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_producers, payer,
    powerless, immediate, trapped, local).

% Many monastic traditions rely heavily on icons for devotional practice and spiritual instruction. The prohibition disrupts their spiritual life and communal identity, forcing them to abandon cherished practices or go underground.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_communities, payer,
    moderate, biographical, constrained, regional).

% Individuals whose personal piety and worship are deeply intertwined with the use of religious images. The prohibition severs a vital connection to their spiritual life, leaving them feeling alienated or forced to practice in secret. Their identity is fused with these practices.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Their theological arguments for the veneration of images are suppressed and condemned. They are excluded from official discourse and face persecution, but their ideas persist in underground movements or exile.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconodule_theologians, excluded,
    powerful, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate religious practice around an aniconic ideal, preventing perceived idolatry and standardizing worship forms across a diverse populace.
% TRANSFER_FUNCTION: Transfers religious authority and control over spiritual expression from local communities and monastic centers to a central imperial and theological authority. It also transfers material wealth (icons) from producers to destruction.
% ABSENT_VOICES: Iconodule theologians and the vast majority of devotional practitioners who cherish images are actively suppressed or excluded from the discourse. They would argue for the legitimacy of images as conduits to the divine and the spiritual harm caused by their destruction.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished, there would be an immediate resurgence of icon production and veneration, a re-establishment of traditional devotional practices, and a challenge to the centralized religious authority that enforced the ban. The religious landscape would fundamentally shift.
% FOUNDING_PROBLEM: The problem of idolatry: the perceived risk that material representations of the divine would lead to the worship of the image itself rather than the divine prototype, corrupting true faith.
% FOUNDING_PROBLEM_CORROBORATION: Iconoclast theologians and the imperial authority attest that the risk of idolatry is an ever-present danger requiring strict vigilance. Iconodule theologians and historians, from outside the benefiting parties, attest that the original problem was often a pretext for political consolidation and that the theological arguments for idolatry were often exaggerated or misapplied.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(decalogue_image_prohibition__iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconoclast_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconoclast_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(decalogue_image_prohibition__iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Snare due to its high extractiveness (0.85) and suppression (0.92). It extracts religious and political authority from local and monastic communities, concentrating it in imperial hands. Suppression is high because the enforcement involves active destruction of images and persecution of those who resist. The theater ratio is low (0.1) as the enforcement is direct and functional, not merely performative. Resistance is high (0.7) due to the deep cultural and spiritual attachment to icons, leading to significant underground activity and open rebellion.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the imperial authority and iconoclast theologians, this is a necessary measure to preserve true faith and order (claimed as a Rope or even a Mountain of divine law). From the perspective of icon producers and practitioners, it is a brutal act of cultural destruction and spiritual oppression (experienced as a Snare). The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Centralizing imperial authority and iconoclast theologians are clear beneficiaries, gaining power and validation. Icon producers, monastic communities, and devotional practitioners are direct victims, suffering economic ruin, spiritual disruption, and identity-locked coercion. Iconodule theologians are excluded, their arguments suppressed to maintain the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_motivation,
    'To what extent was the iconoclast prohibition genuinely driven by theological concerns about idolatry, versus political motivations to centralize imperial power and weaken monastic influence?',
    'Historical analysis of imperial decrees, theological treatises, and contemporary political events, focusing on the timing and beneficiaries of enforcement actions.',
    'If primarily political, the constraint''s extractiveness is higher and its coordination function (preventing idolatry) is more theatrical, pushing it further towards a Snare. If primarily theological, the coordination function is more genuine, though still highly extractive for those who disagree.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_motivation, empirical, 'Ambiguity in the primary motivation for the iconoclast prohibition.').

omega_variable(
    internalized_suppression_of_devotion,
    'Does the suppression of icon veneration lead to internalized suppression, where devotional practitioners cease to desire or imagine images even after external enforcement wanes?',
    'Longitudinal studies of communities where iconoclasm was enforced, observing the re-emergence (or lack thereof) of iconographic traditions after the lifting of prohibitions.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher and more persistent than structural measures suggest, indicating a deeper impact on religious identity and practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_of_devotion, empirical, 'Structural vs. internalized suppression mechanism for devotional practices.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the ''decalogue_image_prohibition'' kernel best framed as a theological commandment, a political decree, or a cultural norm?',
    'Analysis of the primary mode of enforcement and justification across different historical periods and regions. If enforcement is primarily state-driven, it leans political; if primarily doctrinal, theological; if primarily social pressure, cultural.',
    'Framing as a theological commandment (as this reading does) emphasizes its ''natural law'' claim, potentially masking its extractive political function. A political framing would highlight the power dynamics and extraction more directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity in the fundamental framing of the image prohibition kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(deca_tr_t10, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(deca_tr_t30, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(deca_tr_t50, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(deca_be_t10, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 20, 0.85).
narrative_ontology:measurement(deca_be_t30, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(deca_be_t50, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(deca_su_t10, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 10, 0.9).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 20, 0.92).
narrative_ontology:measurement(deca_su_t30, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 30, 0.9).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 40, 0.91).
narrative_ontology:measurement(deca_su_t50, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 50, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconoclast_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__iconodule_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'decalogue_image_prohibition' kernel. This 'iconoclast_reading' asserts a categorical ban on all religious imagery, directly opposing the 'iconodule_reading' and influencing the 'moderate_iconoclast_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
