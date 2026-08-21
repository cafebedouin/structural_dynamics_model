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
    narrative_ontology:epsilon_provenance/5,
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
 *   prohibition on images, which distinguishes between worship (latria) due
 *   only to God, and veneration/honor (dulia) permissible for images as
 *   conduits to their prototypes. It is grounded in the theological
 *   understanding that the Incarnation sanctified matter, making it a valid
 *   medium for divine communication. This reading enables a rich visual
 *   culture within the church, coordinating the spiritual practices of the
 *   laity and clergy. It is presented as a Rope because it solves a genuine
 *   coordination problem with low extraction, primarily benefiting those who
 *   engage in visual devotion and the artists who create the images.
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
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Decalogue Image Prohibition (Iconodule Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theology/religious_authority/visual_culture").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, '00e425f7-7286-41f3-bca9-4b23526f6e33').
narrative_ontology:cs_kernel_codification('00e425f7-7286-41f3-bca9-4b23526f6e33', fixed_text).
narrative_ontology:cs_authority_grounding('00e425f7-7286-41f3-bca9-4b23526f6e33', lineage).
narrative_ontology:cs_interpretation_layer_present('00e425f7-7286-41f3-bca9-4b23526f6e33').
narrative_ontology:cs_reading_relation('00e425f7-7286-41f3-bca9-4b23526f6e33', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('00e425f7-7286-41f3-bca9-4b23526f6e33', decalogue_image_prohibition__moderate_iconoclast_reading, coexists_with).
narrative_ontology:cs_axiom('00e425f7-7286-41f3-bca9-4b23526f6e33', foundational, incarnation_sanctifies_matter).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_matter, holdable).
narrative_ontology:cs_axiom_grounding('00e425f7-7286-41f3-bca9-4b23526f6e33', incarnation_sanctifies_matter, theological).
narrative_ontology:cs_axiom('00e425f7-7286-41f3-bca9-4b23526f6e33', foundational, dulia_distinct_from_latria).
narrative_ontology:cs_axiom_status(dulia_distinct_from_latria, holdable).
narrative_ontology:cs_axiom_grounding('00e425f7-7286-41f3-bca9-4b23526f6e33', dulia_distinct_from_latria, theological).
narrative_ontology:cs_reference_frame('00e425f7-7286-41f3-bca9-4b23526f6e33', second_council_of_nicaea_doctrine).
narrative_ontology:cs_drift_state('00e425f7-7286-41f3-bca9-4b23526f6e33', contemporary_secular_context, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('00e425f7-7286-41f3-bca9-4b23526f6e33', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, laity).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, clergy).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_artists).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, incarnational_theology).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, sacramental_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a sanctioned means of visual devotion, allowing for spiritual connection through material objects without fear of idolatry. Provides a tangible focus for prayer and contemplation.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, laity, beneficiary,
    moderate, biographical, constrained, global).

% Administers and interprets the theological distinction between latria and dulia, guiding the faithful in proper veneration. Benefits from a rich visual culture that aids catechesis and worship.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, clergy, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from the theological justification for creating religious art, providing a livelihood and a spiritual vocation. Their work serves as a conduit for the divine within the community.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, icon_artists, beneficiary,
    moderate, biographical, constrained, local).

% Would argue that any use of images in worship is idolatrous and violates the Decalogue. Their perspective is actively suppressed within this reading's framework, leading to historical persecution and destruction of art.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconoclast_theologians, excluded,
    powerful, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the spiritual practice of the laity by providing a clear theological framework for the permissible use of religious images, preventing both idolatry and iconoclastic rejection of material mediation.
% TRANSFER_FUNCTION: Transfers spiritual focus and devotion through material images to their divine prototypes, facilitating a connection between the earthly and the heavenly realms.
% ABSENT_VOICES: Strict iconoclast theologians are excluded; they would argue that this reading fundamentally misinterprets the Decalogue and promotes idolatry, but their views are deemed heretical within this framework.
% DISAPPEARANCE_RATIONALE: If this distinction vanished, the entire visual culture of the church would collapse, leading to either widespread iconoclasm or uncontrolled idolatry. Spiritual practices would be profoundly altered, and the theological understanding of Incarnation would be diminished.
% FOUNDING_PROBLEM: The problem of how to reconcile the biblical prohibition against idolatry with the desire for visual aids in worship, especially after the Incarnation affirmed the goodness of matter.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live as the church continues to navigate the use of images in diverse cultural contexts. Corroboration comes from ongoing theological discourse, historical councils, and the lived experience of believers who seek to honor God without falling into idolatry.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness is low (0.15) as the constraint primarily facilitates spiritual practice rather than extracting material rents. Suppression is also low (0.1) because, within this framework, the distinction is largely accepted, and enforcement is primarily catechetical rather than coercive. Theater ratio is minimal (0.05) as the theological justification is deeply integrated into practice. Accessibility collapse is high (0.8) because, for adherents, alternatives to this nuanced approach (either strict iconoclasm or uncritical idolatry) are seen as spiritually dangerous. Resistance is low (0.05) as this reading is widely adopted within its theological tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the laity and icon artists, this constraint is a clear benefit, enabling their spiritual and professional lives. From the perspective of strict iconoclast theologians (excluded), this constraint is a dangerous compromise that leads to idolatry, representing a fundamental theological error.
 *
 * DIRECTIONALITY LOGIC:
 *   The laity and icon artists are direct beneficiaries, gaining a sanctioned means of devotion and livelihood. The clergy acts as the agenda-setter, interpreting and enforcing the theological distinctions. Iconoclast theologians are structurally excluded, as their position is deemed heretical within this reading's framework, making them targets of suppression if they were to actively resist.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling genuine coordination (facilitating visual devotion) as pure extraction. Its persistence is tied to the ongoing theological need to balance biblical prohibitions with incarnational theology. It avoids mandatrophy by remaining relevant to the spiritual lives of its adherents; the founding problem of reconciling images with worship remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    iconoclast_suppression_impact,
    'What would be the effective suppression if the iconoclast reading gained institutional power and actively enforced its views?',
    'Historical analysis of periods of iconoclasm, measuring the destruction of art, persecution of iconodules, and suppression of visual practices.',
    'If the iconoclast reading were enforced, the suppression metric for this (iconodule) constraint would rise dramatically, and its classification would shift from Rope to Snare, as its practices would be actively targeted and victims would emerge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(iconoclast_suppression_impact, empirical, 'Impact of active iconoclast enforcement on iconodule practices.').

omega_variable(
    theological_interpretation_ambiguity,
    'Is the distinction between latria and dulia a genuine theological truth or a post-hoc rationalization to permit images?',
    'Deep historical-theological analysis of early Christian writings and patristic debates, examining the development of the distinction prior to major iconoclastic controversies.',
    'If it were found to be a post-hoc rationalization, the ''emerges_naturally'' aspect of the theological claim would be undermined, potentially shifting the constraint towards a more constructed (Tangled Rope) classification, as its ''naturalness'' would be contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_interpretation_ambiguity, conceptual, 'Ambiguity of the latria/dulia distinction''s theological grounding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(deca_be_t300, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 300, 0.12).
narrative_ontology:measurement(deca_be_t600, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 600, 0.15).
narrative_ontology:measurement(deca_be_t900, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 900, 0.14).
narrative_ontology:measurement(deca_be_t1200, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1200, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(deca_su_t300, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 300, 0.08).
narrative_ontology:measurement(deca_su_t600, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 600, 0.1).
narrative_ontology:measurement(deca_su_t900, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 900, 0.09).
narrative_ontology:measurement(deca_su_t1200, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1200, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconodule_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'decalogue_image_prohibition' kernel. Its sibling readings, 'iconoclast_reading' and 'moderate_iconoclast_reading', offer alternative interpretations of the same biblical text, leading to different constraint structures and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
