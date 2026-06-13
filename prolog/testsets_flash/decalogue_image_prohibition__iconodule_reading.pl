% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconodule_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__iconodule_reading, []).

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
 *   constraint_id: decalogue_image_prohibition__iconodule_reading
 *   human_readable: Decalogue Image Prohibition (Iconodule Reading)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This constraint represents the 'iconodule' reading of the Decalogue's
 *   image prohibition, which distinguishes between latria (worship,
 *   forbidden) and dulia (veneration, permitted) of images. It asserts that
 *   the Incarnation sanctifies matter, making images valid conduits to the
 *   divine. This reading, formalized at the Second Council of Nicaea (787
 *   AD), enables a rich visual culture within Christian worship. It is a
 *   Rope-type constraint, coordinating spiritual practice and artistic
 *   expression within theological boundaries. The metrics reflect its low
 *   extractiveness and suppression once established, though initial
 *   suppression was high during the Iconoclast controversies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.15).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.2).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Decalogue Image Prohibition (Iconodule Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theology/religious_authority/visual_culture").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, 'db8631d5-407b-4503-bdfd-1f5f5e26ffe0').
narrative_ontology:cs_kernel_codification('db8631d5-407b-4503-bdfd-1f5f5e26ffe0', fixed_text).
narrative_ontology:cs_authority_grounding('db8631d5-407b-4503-bdfd-1f5f5e26ffe0', lineage).
narrative_ontology:cs_interpretation_layer_present('db8631d5-407b-4503-bdfd-1f5f5e26ffe0').
narrative_ontology:cs_reading_relation('db8631d5-407b-4503-bdfd-1f5f5e26ffe0', decalogue_image_prohibition__iconoclast_reading, forecloses).
narrative_ontology:cs_reading_relation('db8631d5-407b-4503-bdfd-1f5f5e26ffe0', decalogue_image_prohibition__moderate_iconoclast_reading, coexists_with).
narrative_ontology:cs_axiom('db8631d5-407b-4503-bdfd-1f5f5e26ffe0', foundational, incarnation_sanctifies_matter).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_matter, holdable).
narrative_ontology:cs_axiom_grounding('db8631d5-407b-4503-bdfd-1f5f5e26ffe0', incarnation_sanctifies_matter, theological).
narrative_ontology:cs_axiom('db8631d5-407b-4503-bdfd-1f5f5e26ffe0', foundational, dulia_is_not_latria).
narrative_ontology:cs_axiom_status(dulia_is_not_latria, holdable).
narrative_ontology:cs_axiom_grounding('db8631d5-407b-4503-bdfd-1f5f5e26ffe0', dulia_is_not_latria, deontological).
narrative_ontology:cs_reference_frame('db8631d5-407b-4503-bdfd-1f5f5e26ffe0', second_council_of_nicaea_787).
narrative_ontology:cs_drift_state('db8631d5-407b-4503-bdfd-1f5f5e26ffe0', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('db8631d5-407b-4503-bdfd-1f5f5e26ffe0', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, laity).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_creators).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, church_hierarchy).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, incarnational_theology).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, sacramental_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces the distinction between latria (worship) and dulia (veneration), sanctioning the creation and use of icons according to theological principles. Benefits from a unified visual culture that reinforces doctrine.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, church_hierarchy, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from a rich visual culture that aids in prayer, contemplation, and understanding of divine mysteries. The constraint provides a permissible and guided way to interact with the divine through material forms, avoiding the spiritual vacuum of iconoclasm.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, laity, beneficiary,
    moderate, biographical, constrained, local).

% Are empowered to create sacred art within established theological and aesthetic guidelines. Their work serves a spiritual function and provides a livelihood, contributing to the visual tradition of the church.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, icon_creators, beneficiary,
    moderate, biographical, mobile, regional).

% Are doctrinally opposed to any religious imagery in worship, viewing it as idolatry. Their arguments are rejected by this reading, and their practices are suppressed within the iconodule tradition. They would advocate for the destruction of all icons.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconoclast_factions, excluded,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the spiritual practice of the laity and the artistic expression of creators with theological doctrine, ensuring that visual aids to worship are orthodox and do not lead to idolatry. It provides a common framework for understanding the role of matter in spiritual life.
% TRANSFER_FUNCTION: Transfers spiritual access and doctrinal understanding through visual means to the laity, and cultural legitimacy to icon creators, in exchange for adherence to theological distinctions and artistic conventions.
% ABSENT_VOICES: Iconoclast factions are absent from the conversation within this reading's framework; they would argue that any use of images in worship is a violation of the Decalogue and a form of idolatry, regardless of intent or theological distinction.
% DISAPPEARANCE_RATIONALE: If this distinction vanished, the entire visual culture of the church would collapse into either idolatry or iconoclasm. Laity would lose a primary mode of spiritual engagement, icon creators would lose their vocation, and the church hierarchy would lose a key tool for doctrinal transmission and communal identity.
% FOUNDING_PROBLEM: The problem of how to reconcile the Old Testament prohibition against images with the Christian belief in the Incarnation, which posits God taking on material form, and the desire of the faithful for visual aids in worship.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live as the theological justification for a central aspect of Christian worship and art. The ongoing need for catechesis and defense against both literalist interpretations of the Decalogue and potential abuses of veneration corroborates its continued relevance. The church hierarchy and theologians outside the immediate beneficiaries attest to its foundational importance.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconodule_reading_tests).
:- end_tests(decalogue_image_prohibition__iconodule_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) as the constraint primarily facilitates spiritual practice rather than extracting material rents. Suppression is low (0.20) in its established state, as adherence is largely voluntary and doctrinally integrated, though it was high during the historical Iconoclast controversies (reflected in early suppression_requirement). Theater ratio is negligible (0.05) as the distinction between latria and dulia is a core functional element, not a performance. Accessibility collapse is moderate (0.30) because while it provides a sanctioned path, other forms of spiritual engagement exist.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the iconodule tradition, this constraint is a necessary and beneficial theological distinction. From an iconoclast perspective, it is a dangerous compromise that leads to idolatry. The engine's classification of 'Rope' reflects the internal coherence and coordination function within the iconodule framework, while the 'excluded' stakeholder captures the external dissent.
 *
 * DIRECTIONALITY LOGIC:
 *   The church hierarchy (agenda_setter) benefits from a coherent theological and visual system. The laity and icon creators are direct beneficiaries, gaining spiritual access and vocational purpose, respectively. Iconoclast factions are structurally excluded, as their core premise is rejected by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latria_dulia_distinction_clarity,
    'Is the theological distinction between latria (worship) and dulia (veneration) sufficiently clear and consistently applied in practice to prevent actual idolatry among the laity?',
    'Empirical studies of popular piety and theological education effectiveness; analysis of historical and contemporary instances of perceived abuse of icon veneration.',
    'If the distinction is found to be consistently blurred or misunderstood, the constraint''s effective extractiveness (spiritual harm) could be higher, potentially shifting its classification towards a Tangled Rope or Snare from the perspective of the laity, as it would be extracting genuine spiritual misdirection under the guise of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latria_dulia_distinction_clarity, empirical, 'Ambiguity in the practical application of the latria/dulia distinction.').

omega_variable(
    incarnation_sanctifies_matter_scope,
    'To what extent does the Incarnation''s sanctification of matter extend to justify all forms of material mediation in worship, or are there inherent limits that this reading might overstep?',
    'Further theological development and inter-denominational dialogue on the implications of Christology for material culture in worship.',
    'If the scope is found to be overextended, the foundational axiom of this reading could be challenged, potentially weakening its theological grounding and opening it to reclassification as a more constructed or even extractive constraint if it persists without strong theological warrant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incarnation_sanctifies_matter_scope, conceptual, 'Conceptual limits of the Incarnation''s justification for material mediation.').

omega_variable(
    iconoclast_suppression_legitimacy,
    'Was the historical suppression of iconoclast factions a legitimate defense of orthodox doctrine (coordination), or an act of coercive power to enforce a particular theological interpretation (extraction)?',
    'Re-evaluation of historical sources, focusing on the agency and motivations of suppressed groups, and the material consequences of iconoclast persecution.',
    'If the suppression is re-evaluated as primarily coercive, the historical trajectory of this constraint would show higher initial extractiveness and suppression, potentially reclassifying its early phase as a Snare or Tangled Rope for the iconoclast seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iconoclast_suppression_legitimacy, empirical, 'Legitimacy of historical suppression of iconoclasm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t787, decalogue_image_prohibition__iconodule_reading, theater_ratio, 787, 0.05).
narrative_ontology:measurement(deca_tr_t1000, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(deca_tr_t1500, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(deca_tr_t2024, decalogue_image_prohibition__iconodule_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(deca_be_t787, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 787, 0.1).
narrative_ontology:measurement(deca_be_t1000, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1000, 0.12).
narrative_ontology:measurement(deca_be_t1500, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1500, 0.15).
narrative_ontology:measurement(deca_be_t2024, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t787, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 787, 0.5).
narrative_ontology:measurement(deca_su_t1000, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1000, 0.3).
narrative_ontology:measurement(deca_su_t1500, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1500, 0.2).
narrative_ontology:measurement(deca_su_t2024, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconodule_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Decalogue's image prohibition, focusing on the iconodule theological distinction. It is linked to sibling readings (iconoclast and moderate iconoclast) which offer different interpretations of the same kernel, leading to different constraint structures and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
