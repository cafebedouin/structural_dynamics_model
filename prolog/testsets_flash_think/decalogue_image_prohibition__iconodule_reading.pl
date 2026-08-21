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
 *   constraint_id: decalogue_image_prohibition__iconodule_reading
 *   human_readable: Iconodule Doctrine on Religious Imagery
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This constraint story instantiates the 'iconodule_reading' of the
 *   Decalogue's prohibition on images. This reading, primarily articulated in
 *   Eastern Orthodox and Roman Catholic traditions, interprets the
 *   prohibition as forbidding the worship of images (latria) but permitting
 *   their veneration (dulia) as a means of honoring the prototype (the person
 *   or event depicted). The Incarnation is central to this view, as it
 *   sanctifies matter, making it a valid conduit to the divine. This reading
 *   functions as a Rope, coordinating religious practice and artistic
 *   expression within a theologically sound framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.15).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.1).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Iconodule Doctrine on Religious Imagery").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconodule_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, 'ce4a421e-cf4f-400f-a720-3b966afde12b').
narrative_ontology:cs_kernel_codification('ce4a421e-cf4f-400f-a720-3b966afde12b', fixed_text).
narrative_ontology:cs_authority_grounding('ce4a421e-cf4f-400f-a720-3b966afde12b', lineage).
narrative_ontology:cs_interpretation_layer_present('ce4a421e-cf4f-400f-a720-3b966afde12b').
narrative_ontology:cs_reading_relation('ce4a421e-cf4f-400f-a720-3b966afde12b', decalogue_image_prohibition__iconoclast_reading, forecloses).
narrative_ontology:cs_reading_relation('ce4a421e-cf4f-400f-a720-3b966afde12b', decalogue_image_prohibition__moderate_iconoclast_reading, forecloses).
narrative_ontology:cs_axiom('ce4a421e-cf4f-400f-a720-3b966afde12b', foundational, incarnation_sanctifies_matter).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_matter, holdable).
narrative_ontology:cs_axiom_grounding('ce4a421e-cf4f-400f-a720-3b966afde12b', incarnation_sanctifies_matter, theological).
narrative_ontology:cs_axiom('ce4a421e-cf4f-400f-a720-3b966afde12b', foundational, dulia_distinct_from_latria).
narrative_ontology:cs_axiom_status(dulia_distinct_from_latria, holdable).
narrative_ontology:cs_axiom_grounding('ce4a421e-cf4f-400f-a720-3b966afde12b', dulia_distinct_from_latria, deontological).
narrative_ontology:cs_reference_frame('ce4a421e-cf4f-400f-a720-3b966afde12b', second_council_of_nicaea_787).
narrative_ontology:cs_drift_state('ce4a421e-cf4f-400f-a720-3b966afde12b', contemporary_theological_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ce4a421e-cf4f-400f-a720-3b966afde12b', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, laity).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, artists_theologians).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, church_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines, interprets, and enforces the orthodox doctrine regarding religious images, ensuring proper veneration and preventing idolatry. Benefits from maintaining theological coherence and authority over visual culture.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, church_hierarchy, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from having sanctioned visual aids for worship and devotional practice, facilitating a connection to the divine and saints. Their options are to follow orthodox practice or risk theological error.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, laity, beneficiary,
    moderate, biographical, constrained, local).

% Creates and interprets religious images within the bounds of orthodox doctrine, contributing to the visual culture of the church. Benefits from a clear theological framework for their work and patronage.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, artists_theologians, beneficiary,
    moderate, biographical, constrained, regional).

% Opposes all religious imagery as idolatrous, based on a strict interpretation of the Decalogue. They are structurally excluded from the iconodule framework and would face persecution if they attempted to enforce their views within it.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconoclast_factions, excluded,
    organized, biographical, identity_locked, global).

% Analyzes the historical, cultural, and sociological impact of the iconodule doctrine and its role in shaping religious practice and art, without participating in its theological claims.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, secular_observers, observer,
    analytical, immediate, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent theological and practical framework for the use of religious images in worship and devotion, allowing the faithful to venerate saints and divine figures without falling into idolatry, and guiding artists in their creation.
% TRANSFER_FUNCTION: Theologically, it facilitates the transfer of spiritual meaning and devotional focus from the image to its prototype (the person or event depicted), enabling a connection between the earthly and divine. Institutionally, it transfers authority over visual culture and its interpretation to the church hierarchy.
% ABSENT_VOICES: Iconoclast factions are structurally absent from the conversation within this framework; they would argue that any religious image is idolatrous and that the Incarnation does not sanctify matter in a way that permits such veneration. Their views are considered heterodox within this reading.
% DISAPPEARANCE_RATIONALE: If this doctrine vanished, the entire visual culture of many Christian traditions (e.g., Orthodox, Catholic) would collapse into either unbridled idolatry or strict iconoclasm, fundamentally altering worship practices, religious art, and theological understanding. The coordination function for visual piety would be lost.
% FOUNDING_PROBLEM: To reconcile the biblical prohibition against graven images (Decalogue) with the desire for visual aids in worship and devotional practice, especially in light of the theological implications of the Incarnation (God becoming flesh, sanctifying matter).
% FOUNDING_PROBLEM_CORROBORATION: Theological arguments from Church Fathers (e.g., John of Damascus), decrees from Ecumenical Councils (e.g., Second Council of Nicaea in 787 CE), and ongoing theological discourse within the traditions that uphold icon veneration.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconodule_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint's extractiveness (0.15) and suppression (0.10) are low because its primary function is to enable and coordinate, rather than extract or coerce. It provides a clear, widely accepted framework for a desired religious practice. Theater ratio (0.05) is minimal, as the theological justification and practical application are genuine. Active enforcement is required to maintain doctrinal purity and prevent actual idolatry, but this is seen as a necessary cost of coordination, not extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of an 'iconoclast_reading' (a sibling constraint), this iconodule doctrine would be perceived as a Snare or Tangled Rope, actively promoting idolatry and extracting spiritual purity. However, within the iconodule framework, it is a necessary and beneficial coordination mechanism. The engine's classification will highlight this divergence based on the structural data of each reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The church hierarchy acts as the agenda-setter, defining and enforcing the doctrine. The laity and artists/theologians are beneficiaries, gaining a legitimate means for visual piety and artistic expression. Iconoclast factions are excluded, as their core premise is incompatible with this reading. The constraint subsidizes the practice of icon veneration and the production of religious art.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of this constraint remains live. The theological problem it addresses (reconciling biblical prohibition with visual piety post-Incarnation) is still relevant for the traditions that uphold it. There is no evidence of its function atrophying or becoming merely theatrical; its continued operation is central to the identity and practice of its adherents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    iconoclast_reading_validity,
    'Is the ''iconoclast_reading'' a valid interpretation of the Decalogue''s prohibition on images, or a theological error from the perspective of the broader Christian tradition?',
    'Further historical-theological analysis of early Christian practice and scriptural interpretation, or a future ecumenical council definitively settling the matter.',
    'If the iconoclast reading were deemed valid, the iconodule doctrine would be reclassified as a Snare, as it would be seen as actively promoting idolatry and extracting spiritual purity from its adherents. If deemed an error, the iconodule reading''s Rope classification would be further solidified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(iconoclast_reading_validity, conceptual, 'Theological validity of the iconoclast interpretation.').

omega_variable(
    material_sanctification_scope,
    'How far does the Incarnation''s sanctification of matter extend? Does it permit all forms of religious art (e.g., 2D icons, 3D statues), or only specific ones, as suggested by the ''moderate_iconoclast_reading''?',
    'Deepened theological reflection on the metaphysics of the Incarnation and its implications for material representation, or a definitive statement from a recognized ecclesiastical authority.',
    'If the Incarnation''s sanctification were found to have limits that exclude certain forms of imagery (e.g., 3D statues), the iconodule reading would need to be re-evaluated, potentially shifting towards a Tangled Rope if it continued to permit such forms while extracting spiritual risk from the laity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_sanctification_scope, conceptual, 'Scope of material sanctification by the Incarnation.').

omega_variable(
    dulia_latria_distinction_clarity,
    'Is the theological distinction between dulia (veneration) and latria (worship) sufficiently clear and consistently applied in practice to prevent actual idolatry among the laity?',
    'Empirical studies of lay devotional practices, theological education effectiveness, and historical analysis of instances where the distinction blurred or failed.',
    'If the distinction is found to be consistently unclear or prone to failure in practice, the constraint''s effective extractiveness (spiritual risk) would increase, potentially reclassifying it as a Tangled Rope or Snare, as it would be extracting genuine idolatry from its adherents despite its stated intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dulia_latria_distinction_clarity, empirical, 'Practical clarity and effectiveness of dulia/latria distinction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 700, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t700, decalogue_image_prohibition__iconodule_reading, theater_ratio, 700, 0.05).
narrative_ontology:measurement(deca_tr_t960, decalogue_image_prohibition__iconodule_reading, theater_ratio, 960, 0.05).
narrative_ontology:measurement(deca_tr_t1220, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1220, 0.05).
narrative_ontology:measurement(deca_tr_t1480, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1480, 0.05).
narrative_ontology:measurement(deca_tr_t1740, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1740, 0.05).
narrative_ontology:measurement(deca_tr_t2020, decalogue_image_prohibition__iconodule_reading, theater_ratio, 2020, 0.05).

% Extraction over time
narrative_ontology:measurement(deca_be_t700, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 700, 0.15).
narrative_ontology:measurement(deca_be_t960, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 960, 0.15).
narrative_ontology:measurement(deca_be_t1220, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1220, 0.15).
narrative_ontology:measurement(deca_be_t1480, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1480, 0.15).
narrative_ontology:measurement(deca_be_t1740, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1740, 0.15).
narrative_ontology:measurement(deca_be_t2020, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t700, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 700, 0.1).
narrative_ontology:measurement(deca_su_t960, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 960, 0.1).
narrative_ontology:measurement(deca_su_t1220, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1220, 0.1).
narrative_ontology:measurement(deca_su_t1480, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1480, 0.1).
narrative_ontology:measurement(deca_su_t1740, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1740, 0.1).
narrative_ontology:measurement(deca_su_t2020, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 2020, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconodule_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, christian_art_production_norms).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, liturgical_practice_standards).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
