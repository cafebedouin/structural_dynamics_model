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
 *   prohibition against images, which interprets the commandment as a
 *   categorical ban on all religious imagery used in worship, equating it
 *   with idolatry. This reading was historically enforced by imperial and
 *   ecclesiastical authorities to centralize power and eliminate competing
 *   forms of religious expression. It is a 'wall-type' constraint,
 *   establishing a strict boundary against material mediation of the holy.
 *   The victim set includes icon producers, monastic communities, and
 *   devotional practitioners, while the beneficiaries are the centralizing
 *   imperial authority and iconoclast theologians.
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
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Decalogue Image Prohibition (Iconoclast Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, '5d05714f-6c24-47f9-91f8-cdb6db3ca5d2').
narrative_ontology:cs_kernel_codification('5d05714f-6c24-47f9-91f8-cdb6db3ca5d2', fixed_text).
narrative_ontology:cs_authority_grounding('5d05714f-6c24-47f9-91f8-cdb6db3ca5d2', lineage).
narrative_ontology:cs_interpretation_layer_present('5d05714f-6c24-47f9-91f8-cdb6db3ca5d2').
narrative_ontology:cs_reading_relation('5d05714f-6c24-47f9-91f8-cdb6db3ca5d2', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_reading_relation('5d05714f-6c24-47f9-91f8-cdb6db3ca5d2', decalogue_image_prohibition__moderate_iconoclast_reading, forecloses).
narrative_ontology:cs_axiom('5d05714f-6c24-47f9-91f8-cdb6db3ca5d2', foundational, material_mediation_is_idolatry).
narrative_ontology:cs_axiom_status(material_mediation_is_idolatry, holdable).
narrative_ontology:cs_axiom_grounding('5d05714f-6c24-47f9-91f8-cdb6db3ca5d2', material_mediation_is_idolatry, deontological).
narrative_ontology:cs_axiom('5d05714f-6c24-47f9-91f8-cdb6db3ca5d2', foundational, divine_transcendence_forbids_representation).
narrative_ontology:cs_axiom_status(divine_transcendence_forbids_representation, holdable).
narrative_ontology:cs_axiom_grounding('5d05714f-6c24-47f9-91f8-cdb6db3ca5d2', divine_transcendence_forbids_representation, theological).
narrative_ontology:cs_reference_frame('5d05714f-6c24-47f9-91f8-cdb6db3ca5d2', aniconic_purity_of_worship).
narrative_ontology:cs_drift_state('5d05714f-6c24-47f9-91f8-cdb6db3ca5d2', post_incarnation_theology, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5d05714f-6c24-47f9-91f8-cdb6db3ca5d2', '').
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

% Enforces the prohibition, often through state power, to consolidate religious and political control. Benefits from eliminating alternative centers of religious devotion and wealth associated with icons.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Provide the theological justification for the prohibition, gaining influence and authority within the religious hierarchy by defining orthodoxy. Their careers and intellectual projects are advanced by the constraint.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconoclast_theologians, beneficiary,
    powerful, generational, mobile, regional).

% Artists, artisans, and workshops whose livelihoods depend on creating religious images. The prohibition directly criminalizes their craft, leading to economic ruin and persecution.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_producers, payer,
    powerless, biographical, trapped, local).

% Often centers of icon production and veneration, they face suppression, confiscation of property, and forced re-education or dissolution under the iconoclast regime. Their spiritual practices are directly targeted.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_communities, payer,
    moderate, generational, constrained, regional).

% Lay believers whose personal and communal worship practices are deeply intertwined with the use of religious imagery. They experience a profound disruption to their spiritual lives and face coercion to abandon cherished traditions.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Advocates for the veneration of images, they are systematically silenced, exiled, or persecuted. Their arguments are suppressed, and their followers are targeted by the enforcing authority.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconodule_theologians, excluded,
    powerful, generational, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate religious practice around an aniconic ideal, preventing perceived idolatry and standardizing worship forms under a central theological interpretation.
% TRANSFER_FUNCTION: Transfers religious authority and material wealth (from confiscated icons and monastic properties) to the centralizing imperial and ecclesiastical powers, by eliminating competing forms of religious expression and their associated economic bases.
% ABSENT_VOICES: Iconodule theologians and their followers are actively suppressed and excluded from the discourse; they would argue that the prohibition misinterprets scripture and that images can be legitimate conduits to the divine.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished, icon production would resume, monastic communities would restore their practices, and devotional life would re-integrate imagery. The centralizing authority would lose a key tool for control, and religious expression would diversify.
% FOUNDING_PROBLEM: The problem of idolatry: ensuring the worship of God alone and preventing the veneration of created things, as commanded in the Decalogue.
% FOUNDING_PROBLEM_CORROBORATION: Iconoclast theologians attest the problem is live and ever-present, citing scriptural warnings against idolatry. Opponents argue the problem is misidentified or exaggerated to serve political ends, but the scriptural basis for concern about idolatry is widely acknowledged across traditions.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness is high (0.85) because the prohibition demands a complete cessation of a widespread religious and economic practice, leading to significant loss for those involved. Suppression is very high (0.92) due to the active, often violent, enforcement by state and religious authorities, including destruction of images, persecution of iconodules, and confiscation of property. Theater ratio is low (0.1) because the enforcement is direct and functional, aimed at eradicating the practice, not merely performing compliance. Resistance is high (0.75) reflecting the significant opposition and rebellions this prohibition often provoked.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the centralizing authority, this is a necessary measure to uphold divine law and maintain order. From the perspective of icon producers and practitioners, it is a destructive and unjust imposition that severs their connection to the divine and destroys their livelihoods. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The centralizing imperial authority and iconoclast theologians are clear beneficiaries, gaining power, wealth, and doctrinal control. Icon producers, monastic communities, and devotional practitioners are direct targets, bearing the full cost of the prohibition through economic ruin, persecution, and spiritual disruption. Iconodule theologians are excluded, their voices actively suppressed to maintain the constraint's legitimacy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_interpretation_ambiguity,
    'Is the iconoclast interpretation of the Decalogue''s image prohibition the only valid theological reading, or are alternative interpretations (e.g., iconodule) equally defensible within the scriptural tradition?',
    'Comparative theological analysis across historical and contemporary traditions, examining hermeneutical principles and contextual factors. Resolution would involve a shift in accepted interpretive authority.',
    'If alternative readings are deemed equally valid, the constraint''s theological grounding weakens, reclassifying it from a divinely mandated ''mountain'' (in its own framing) to a ''snare'' of human interpretation and power. This would reduce its perceived legitimacy and increase its effective extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_interpretation_ambiguity, conceptual, 'Ambiguity in the theological interpretation of the Decalogue''s image prohibition.').

omega_variable(
    power_consolidation_vs_idolatry_prevention,
    'To what extent was the enforcement of the image prohibition genuinely driven by a concern for idolatry, versus a desire by imperial and ecclesiastical authorities to consolidate power and wealth?',
    'Historical analysis of primary sources, focusing on the motivations of enforcing authorities, the economic impact of iconoclasm, and the political context of its implementation. Compare periods of iconoclasm with periods of political instability or attempts at centralization.',
    'If power consolidation is found to be the primary driver, the constraint''s coordination function (preventing idolatry) would be re-evaluated as a cover story, strengthening its classification as a ''snare'' and increasing its effective extractiveness. If genuine theological concern predominates, the coordination function would be seen as more legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_consolidation_vs_idolatry_prevention, empirical, 'Distinguishing genuine theological motivation from political power consolidation in the enforcement of iconoclasm.').


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
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(deca_tr_t30, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(deca_tr_t50, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(deca_be_t10, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 20, 0.85).
narrative_ontology:measurement(deca_be_t30, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 40, 0.86).
narrative_ontology:measurement(deca_be_t50, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(deca_su_t10, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement(deca_su_t30, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 30, 0.93).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 40, 0.91).
narrative_ontology:measurement(deca_su_t50, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 50, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconoclast_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
