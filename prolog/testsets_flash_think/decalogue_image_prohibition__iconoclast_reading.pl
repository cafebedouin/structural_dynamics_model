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
 *   prohibition on images, specifically as enforced during periods like the
 *   Byzantine Iconoclasm. It asserts that any material representation used in
 *   worship constitutes idolatry, leading to a categorical ban on religious
 *   imagery. This reading is a 'Wall-type' constraint, aiming to completely
 *   block material mediation of the holy. Its enforcement targets icon
 *   producers, monastic communities, and devotional practices, while
 *   benefiting centralizing imperial authority and purist adherents. The high
 *   extractiveness and suppression reflect the severe impact on those whose
 *   spiritual lives and livelihoods were tied to icons.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.85).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.9).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Decalogue Image Prohibition (Iconoclast Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, '5843ed91-34ef-4c8d-8f91-41f2e0475d0b').
narrative_ontology:cs_kernel_codification('5843ed91-34ef-4c8d-8f91-41f2e0475d0b', fixed_text).
narrative_ontology:cs_authority_grounding('5843ed91-34ef-4c8d-8f91-41f2e0475d0b', extraction).
narrative_ontology:cs_interpretation_layer_present('5843ed91-34ef-4c8d-8f91-41f2e0475d0b').
narrative_ontology:cs_reading_relation('5843ed91-34ef-4c8d-8f91-41f2e0475d0b', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_reading_relation('5843ed91-34ef-4c8d-8f91-41f2e0475d0b', decalogue_image_prohibition__moderate_iconoclast_reading, forecloses).
narrative_ontology:cs_axiom('5843ed91-34ef-4c8d-8f91-41f2e0475d0b', foundational, material_mediation_is_idolatry).
narrative_ontology:cs_axiom_status(material_mediation_is_idolatry, holdable).
narrative_ontology:cs_axiom_grounding('5843ed91-34ef-4c8d-8f91-41f2e0475d0b', material_mediation_is_idolatry, deontological).
narrative_ontology:cs_axiom('5843ed91-34ef-4c8d-8f91-41f2e0475d0b', secondary, divine_is_immaterial_and_unrepresentable).
narrative_ontology:cs_axiom_status(divine_is_immaterial_and_unrepresentable, holdable).
narrative_ontology:cs_axiom_grounding('5843ed91-34ef-4c8d-8f91-41f2e0475d0b', divine_is_immaterial_and_unrepresentable, theological).
narrative_ontology:cs_reference_frame('5843ed91-34ef-4c8d-8f91-41f2e0475d0b', pure_aniconic_worship).
narrative_ontology:cs_drift_state('5843ed91-34ef-4c8d-8f91-41f2e0475d0b', post_incarnation_theology, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5843ed91-34ef-4c8d-8f91-41f2e0475d0b', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, purist_adherents).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_producers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces the prohibition on religious imagery, viewing it as a means to consolidate spiritual and political control, eliminate perceived idolatry, and prevent challenges to its singular authority. Benefits from a unified, non-material religious expression.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Artists and artisans whose livelihoods depend on creating religious images. The prohibition directly criminalizes their craft, leading to economic ruin and persecution. Their only 'exit' is abandoning their profession or fleeing.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_producers, payer,
    powerless, biographical, trapped, local).

% Many monastic traditions deeply integrate icons into their devotional practices and spiritual teaching. The prohibition forces them to abandon long-held traditions, destroy sacred objects, and re-educate their members, often under duress.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_communities, payer,
    moderate, biographical, constrained, regional).

% Individual believers who use icons in personal and communal worship. The prohibition severs their connection to a familiar and meaningful form of spiritual expression, forcing them into practices they may find alien or less fulfilling. Their identity is often tied to these practices.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners, payer,
    powerless, immediate, identity_locked, local).

% Those who genuinely believe in the absolute prohibition of images and see their removal as a purification of faith. They benefit from the enforcement of what they perceive as true doctrine and the elimination of practices they consider sinful.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, purist_adherents, beneficiary,
    moderate, biographical, mobile, local).

% Scholars and religious leaders who articulate a theological defense of icons, distinguishing between worship and veneration. Their arguments are suppressed, their writings condemned, and their followers persecuted under the iconoclast regime.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconodule_theologians, excluded,
    powerful, generational, constrained, global).

% Theologians who might permit two-dimensional images under strict regulation but oppose three-dimensional statuary. Their nuanced position is foreclosed by the absolute prohibition of the iconoclast reading, placing them in opposition to the dominant enforcement.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, moderate_iconoclast_theologians, excluded,
    powerful, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform, non-material form of worship across a diverse empire, aiming to prevent perceived syncretism with pagan practices and consolidate religious identity under a central authority.
% TRANSFER_FUNCTION: Transfers spiritual authority and control over religious expression from local communities, artists, and monastic traditions to a centralizing imperial and ecclesiastical authority, along with the material wealth associated with icon production.
% ABSENT_VOICES: Iconodule theologians, artists, and monastic communities whose spiritual practices and theological defenses of images are deemed illicit and actively suppressed. Their voices are silenced through persecution and destruction of their works.
% DISAPPEARANCE_RATIONALE: If the prohibition and its enforcement vanished overnight, there would be an immediate resurgence of visual religious art and devotional practices, fundamentally altering the religious landscape, re-empowering local communities and monastic traditions, and challenging the central authority's monopoly on religious form.
% FOUNDING_PROBLEM: Preventing idolatry and maintaining the purity of monotheistic worship, particularly in contexts where visual representations could be confused with pagan idol worship or challenge the singular authority of God.
% FOUNDING_PROBLEM_CORROBORATION: Proponents cite scriptural commandments and historical instances of idolatry. Opponents (iconodules) cite the Incarnation as sanctifying matter and argue that the prohibition misinterprets the nature of worship. Historical accounts of the Byzantine Iconoclasm and theological debates from that era provide corroboration for both sides, from outside the immediate benefiting parties of the iconoclast regime.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness is high because the prohibition demands a fundamental shift in spiritual practice and destroys a significant cultural and economic sphere. Suppression is very high due to the active, often violent, enforcement against widespread and deeply ingrained practices, including destruction of art and persecution of iconodules. The theater ratio is low because the enforcement is direct and functional, not merely performative; the goal is genuine elimination of images. Resistance is high, as evidenced by the historical iconodule counter-movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the centralizing authority and purist adherents, this is a necessary purification of faith (a 'Rope' or even 'Mountain' of divine law). From the perspective of icon producers and devotional practitioners, it is a deeply extractive and oppressive 'Snare' that denies their spiritual expression and livelihood. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The centralizing imperial authority is a clear beneficiary and agenda-setter, using the prohibition to consolidate power and religious uniformity. Purist adherents also benefit by seeing their theological views enforced. Icon producers, monastic communities, and devotional practitioners are direct targets, bearing the full cost of the prohibition. Iconodule and moderate iconoclast theologians are excluded, their alternative interpretations suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_mediation_legitimacy,
    'Is material mediation of the divine inherently idolatrous, or can it serve as a legitimate conduit for veneration without becoming an object of worship itself?',
    'Theological consensus across major traditions, or a shift in the interpretive framework of the Decalogue''s commandment. Empirical observation of devotional practices in contexts where images are permitted.',
    'If material mediation is deemed legitimate, the iconoclast reading''s foundational premise collapses, reclassifying the constraint from a Snare to a Tangled Rope (if some coordination function remains) or even a Piton (if only inertial enforcement remains).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(material_mediation_legitimacy, conceptual, 'The core theological disagreement on the nature of images in worship.').

omega_variable(
    central_authority_motivation,
    'To what extent was the iconoclast prohibition genuinely driven by theological purity, versus being a tool for centralizing imperial power and wealth?',
    'Historical analysis of imperial decrees, economic records of confiscated monastic wealth, and the political context of the Byzantine Empire during iconoclast periods.',
    'If primarily a tool for power, the constraint''s extractiveness is confirmed as inherent to its design. If primarily theological, the extractiveness might be re-evaluated as an unintended consequence of a genuine (though contested) coordination effort.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(central_authority_motivation, empirical, 'Distinguishing theological vs. political drivers of the prohibition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (imperial decrees, destruction of art) or internalized (fear of divine punishment, social pressure to conform)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., after an iconodule restoration), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 726, 843).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t726, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 726, 0.15).
narrative_ontology:measurement(deca_tr_t740, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 740, 0.12).
narrative_ontology:measurement(deca_tr_t760, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 760, 0.1).
narrative_ontology:measurement(deca_tr_t780, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 780, 0.1).
narrative_ontology:measurement(deca_tr_t800, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 800, 0.1).
narrative_ontology:measurement(deca_tr_t843, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 843, 0.1).

% Extraction over time
narrative_ontology:measurement(deca_be_t726, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 726, 0.6).
narrative_ontology:measurement(deca_be_t740, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 740, 0.75).
narrative_ontology:measurement(deca_be_t760, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 760, 0.8).
narrative_ontology:measurement(deca_be_t780, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 780, 0.82).
narrative_ontology:measurement(deca_be_t800, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 800, 0.84).
narrative_ontology:measurement(deca_be_t843, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 843, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t726, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 726, 0.7).
narrative_ontology:measurement(deca_su_t740, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 740, 0.8).
narrative_ontology:measurement(deca_su_t760, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 760, 0.85).
narrative_ontology:measurement(deca_su_t780, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 780, 0.87).
narrative_ontology:measurement(deca_su_t800, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 800, 0.89).
narrative_ontology:measurement(deca_su_t843, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 843, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconoclast_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__iconodule_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Decalogue's image prohibition, focusing on the iconoclast interpretation. It is structurally distinct from the iconodule and moderate iconoclast readings, which permit or regulate images, and is linked to them as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
