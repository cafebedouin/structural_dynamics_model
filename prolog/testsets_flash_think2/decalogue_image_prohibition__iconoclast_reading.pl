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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   This constraint is the iconoclast reading of the Decalogue's image
 *   prohibition, asserting a categorical ban on all religious imagery. It
 *   stands in opposition to iconodule and moderate iconoclast readings. The
 *   prohibition, enforced by imperial and ecclesiastical authority during the
 *   Byzantine Iconoclast Controversies, aimed to eliminate material mediation
 *   of the holy, viewing it as idolatry. While claimed as a divine,
 *   unchangeable law (Mountain), its active enforcement, identifiable
 *   beneficiaries, and severe impact on victims suggest it functioned as a
 *   highly extractive and suppressive mechanism.
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
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, mountain).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Decalogue Image Prohibition (Iconoclast Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).
domain_priors:emerges_naturally(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, '60ec6541-be0b-4f2e-8665-2a372857a1a2').
narrative_ontology:cs_kernel_codification('60ec6541-be0b-4f2e-8665-2a372857a1a2', fixed_text).
narrative_ontology:cs_authority_grounding('60ec6541-be0b-4f2e-8665-2a372857a1a2', lineage).
narrative_ontology:cs_interpretation_layer_present('60ec6541-be0b-4f2e-8665-2a372857a1a2').
narrative_ontology:cs_reading_relation('60ec6541-be0b-4f2e-8665-2a372857a1a2', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_reading_relation('60ec6541-be0b-4f2e-8665-2a372857a1a2', decalogue_image_prohibition__moderate_iconoclast_reading, forecloses).
narrative_ontology:cs_axiom('60ec6541-be0b-4f2e-8665-2a372857a1a2', foundational, material_mediation_is_idolatry).
narrative_ontology:cs_axiom_status(material_mediation_is_idolatry, holdable).
narrative_ontology:cs_axiom_grounding('60ec6541-be0b-4f2e-8665-2a372857a1a2', material_mediation_is_idolatry, deontological).
narrative_ontology:cs_reference_frame('60ec6541-be0b-4f2e-8665-2a372857a1a2', aniconic_purity_of_worship).
narrative_ontology:cs_drift_state('60ec6541-be0b-4f2e-8665-2a372857a1a2', post_second_council_of_nicaea, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('60ec6541-be0b-4f2e-8665-2a372857a1a2', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, iconoclast_clergy).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_producers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconoclast_reading, divine_transcendence).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconoclast_reading, spiritual_worship_purity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Byzantine imperial power that enforced the iconoclast decrees, benefiting from the centralization of religious authority and the weakening of monastic influence. It claimed divine mandate for its actions.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% The ecclesiastical hierarchy and theologians who articulated and enforced the iconoclast position, gaining authority and control over religious practice by eliminating material forms of worship they deemed idolatrous.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconoclast_clergy, agenda_setter,
    institutional, generational, constrained, global).

% Artists, artisans, and workshops whose livelihoods depended on the creation of religious icons. They faced persecution, destruction of their work, and loss of patronage under the prohibition.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_producers, payer,
    powerless, biographical, trapped, local).

% Many monastic orders were strong proponents and producers of icons, which were central to their devotional life and often a source of income. They faced imperial and ecclesiastical pressure to conform, leading to confiscation of property and persecution.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_communities, payer,
    moderate, biographical, constrained, regional).

% Lay believers whose personal and communal worship practices were deeply intertwined with the veneration of icons. The prohibition forced them to abandon deeply held traditions, leading to spiritual distress and resistance.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners, payer,
    powerless, immediate, identity_locked, local).

% Theologians and church leaders who defended the veneration of icons, arguing for their theological legitimacy. They were often exiled, imprisoned, or silenced by the imperial and iconoclast ecclesiastical authorities.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconodule_theologians, excluded,
    powerful, generational, constrained, global).

% Academics who study the historical, political, and theological dynamics of the Iconoclast Controversies, analyzing the motivations and impacts of the prohibition from a detached perspective.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, secular_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate religious practice around an aniconic form of worship, preventing perceived idolatry and maintaining a specific theological purity, thereby standardizing devotional forms across the empire.
% TRANSFER_FUNCTION: Transfers religious authority and control over devotional practice from local communities, monastic centers, and individual piety to a central imperial and ecclesiastical authority. It also transfers material resources (art production, patronage) away from icon-related industries.
% ABSENT_VOICES: Iconodule theologians, artists, and devotional communities who believed in the sanctity of images and their role in worship were actively suppressed, persecuted, or exiled. Their arguments for the theological legitimacy of icons were systematically excluded from official discourse.
% DISAPPEARANCE_RATIONALE: If the prohibition and its enforcement vanished overnight, there would be an immediate resurgence of icon production and use in worship, a re-decentralization of devotional practices, and a significant challenge to the authority that enforced the aniconic stance. The visual and material culture of the church would be fundamentally transformed.
% FOUNDING_PROBLEM: The perceived problem of idolatry and the corruption of pure worship through material representations, rooted in a literal interpretation of the Decalogue. This was intertwined with a desire to centralize religious authority and counter the growing influence and wealth of monastic communities associated with icon production.
% FOUNDING_PROBLEM_CORROBORATION: Iconoclast sources (imperial decrees, synodal acts) attest to the problem of idolatry and the need for reform. However, iconodule sources (theological treatises, hagiographies) and modern historians (secular_historians) contest this framing, pointing to significant political motivations and the theological arguments for images, suggesting the 'problem' was largely a pretext for power consolidation.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, ExtMetricName, E),
    domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(decalogue_image_prohibition__iconoclast_reading),
    narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(decalogue_image_prohibition__iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the forced abandonment of deeply ingrained devotional practices and the destruction of a significant material culture. Suppression (0.9) is severe due to imperial decrees, persecution, and the systematic elimination of icons. Theater ratio is low (0.1) because the enforcement was direct and often brutal, not merely performative. Resistance (0.7) was substantial, leading to prolonged conflict. The claimed type is 'mountain' because the iconoclast position presented itself as an immutable divine command, but the metrics reflect its operational reality as a highly coercive, constructed constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the imperial and iconoclast ecclesiastical authorities, the prohibition was a necessary act of theological purity and a return to true worship (a Mountain). From the perspective of icon producers, monastics, and devotional practitioners, it was a devastating act of cultural destruction and religious oppression (a Snare). The engine will detect this divergence between the claimed type and the operational metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Centralizing imperial authority and iconoclast clergy are clear beneficiaries, gaining power and control over religious life. Icon producers, monastic communities, and devotional practitioners are direct targets, bearing the costs of persecution, loss of livelihood, and disruption of spiritual practice. Iconodule theologians are excluded, their arguments suppressed to maintain the constraint's legitimacy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_command_vs_political_instrument,
    'Is the iconoclast prohibition a genuine divine commandment (Mountain) or a politically motivated interpretation used to centralize imperial and ecclesiastical power (Snare/Tangled Rope)?',
    'Theological consensus across diverse traditions, or historical analysis demonstrating the primary drivers were political rather than purely theological.',
    'If primarily political, the constraint''s true classification shifts from Mountain to Snare or Tangled Rope, highlighting its extractive nature. If genuinely divine, its Mountain classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_command_vs_political_instrument, conceptual, 'Ambiguity between divine mandate and political instrumentalization.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (imperial decrees, physical destruction) or internalized (theological conviction, fear of divine wrath)?',
    'Post-prohibition trajectory: if aniconic practices persisted widely after imperial enforcement ceased, it suggests internalized suppression. If icon veneration immediately resurged, it points to structural suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the prohibition''s effects would persist even without active enforcement. If structural, its persistence depends entirely on coercive power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in religious practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 726, 843).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t726, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 726, 0.1).
narrative_ontology:measurement(deca_tr_t750, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 750, 0.12).
narrative_ontology:measurement(deca_tr_t780, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 780, 0.1).
narrative_ontology:measurement(deca_tr_t810, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 810, 0.08).
narrative_ontology:measurement(deca_tr_t843, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 843, 0.1).

% Extraction over time
narrative_ontology:measurement(deca_be_t726, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 726, 0.75).
narrative_ontology:measurement(deca_be_t750, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 750, 0.8).
narrative_ontology:measurement(deca_be_t780, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 780, 0.83).
narrative_ontology:measurement(deca_be_t810, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 810, 0.87).
narrative_ontology:measurement(deca_be_t843, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 843, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t726, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 726, 0.8).
narrative_ontology:measurement(deca_su_t750, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 750, 0.85).
narrative_ontology:measurement(deca_su_t780, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 780, 0.88).
narrative_ontology:measurement(deca_su_t810, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 810, 0.92).
narrative_ontology:measurement(deca_su_t843, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 843, 0.9).


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
