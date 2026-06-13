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
 *   prohibition on images, specifically focusing on its enforcement as a
 *   'wall-type' constraint that categorically forbids material mediation of
 *   the holy. This reading views any religious imagery used in worship as
 *   idolatry. The victim set includes those whose livelihoods and spiritual
 *   practices depend on icons, while the beneficiaries are the centralizing
 *   imperial and theological authorities who gain power from this aniconic
 *   stance. This is one reading of the 'decalogue_image_prohibition' kernel,
 *   distinct from iconodule and moderate iconoclast readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.6).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.7).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Decalogue Image Prohibition (Iconoclast Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, 'e92cdeec-5742-4ec1-8f20-97b9b85ce97b').
narrative_ontology:cs_kernel_codification('e92cdeec-5742-4ec1-8f20-97b9b85ce97b', fixed_text).
narrative_ontology:cs_authority_grounding('e92cdeec-5742-4ec1-8f20-97b9b85ce97b', lineage).
narrative_ontology:cs_interpretation_layer_present('e92cdeec-5742-4ec1-8f20-97b9b85ce97b').
narrative_ontology:cs_reading_relation('e92cdeec-5742-4ec1-8f20-97b9b85ce97b', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_reading_relation('e92cdeec-5742-4ec1-8f20-97b9b85ce97b', decalogue_image_prohibition__moderate_iconoclast_reading, forecloses).
narrative_ontology:cs_axiom('e92cdeec-5742-4ec1-8f20-97b9b85ce97b', foundational, material_mediation_is_idolatry).
narrative_ontology:cs_axiom_status(material_mediation_is_idolatry, holdable).
narrative_ontology:cs_axiom_grounding('e92cdeec-5742-4ec1-8f20-97b9b85ce97b', material_mediation_is_idolatry, deontological).
narrative_ontology:cs_axiom('e92cdeec-5742-4ec1-8f20-97b9b85ce97b', foundational, divine_transcendence_forbids_representation).
narrative_ontology:cs_axiom_status(divine_transcendence_forbids_representation, holdable).
narrative_ontology:cs_axiom_grounding('e92cdeec-5742-4ec1-8f20-97b9b85ce97b', divine_transcendence_forbids_representation, theological).
narrative_ontology:cs_reference_frame('e92cdeec-5742-4ec1-8f20-97b9b85ce97b', pure_aniconic_worship).
narrative_ontology:cs_drift_state('e92cdeec-5742-4ec1-8f20-97b9b85ce97b', post_incarnation_theology, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e92cdeec-5742-4ec1-8f20-97b9b85ce97b', '').
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

% Enforces the prohibition on religious imagery, seeing it as a means to consolidate power, prevent popular dissent, and eliminate rival centers of religious authority. Benefits from the removal of material mediation of the holy, which it views as a threat to its monopoly on religious form.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Provide the theological justification for the prohibition, framing it as a return to pure worship and a defense against idolatry. Their intellectual authority and influence are enhanced by the enforcement of this reading.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconoclast_theologians, beneficiary,
    powerful, generational, mobile, global).

% Suffer economic ruin and persecution due to the destruction of their livelihood. Their craft is deemed sacrilegious, and they face severe penalties for continuing their work.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_producers, payer,
    powerless, biographical, trapped, local).

% Many monastic traditions rely heavily on icons for devotional practice and spiritual instruction. The prohibition disrupts their spiritual life, destroys their artistic heritage, and undermines their traditional role as centers of religious art.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_communities, payer,
    moderate, generational, constrained, regional).

% Experience a profound loss of familiar and cherished forms of worship. Their personal piety, often expressed through veneration of images, is criminalized, leading to spiritual distress and alienation. Their identity is deeply intertwined with these practices.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Would argue for the legitimacy of icons based on the Incarnation and the distinction between worship and veneration. Their voices are suppressed, and their arguments are deemed heretical by the dominant iconoclast authority.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconodule_theologians, excluded,
    powerful, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate religious practice around an aniconic ideal, ensuring uniformity of worship and preventing perceived idolatry across the empire.
% TRANSFER_FUNCTION: Transfers religious authority and control over spiritual expression from diverse local practices and monastic centers to a centralized imperial and theological authority. It also transfers material wealth (icons) from private hands to state-sanctioned destruction.
% ABSENT_VOICES: Iconodule theologians and their followers, who believe in the spiritual efficacy and theological legitimacy of religious images, are systematically silenced and persecuted. Their arguments for the Incarnation as sanctifying matter are deemed heretical.
% DISAPPEARANCE_RATIONALE: If this prohibition vanished overnight, there would be an immediate resurgence of icon production and veneration, a re-establishment of monastic artistic traditions, and a significant shift in the balance of religious power away from the centralizing authority. The visual landscape of religious life would be fundamentally transformed.
% FOUNDING_PROBLEM: The perceived problem of idolatry and the fragmentation of religious authority due to diverse local cults and the veneration of images.
% FOUNDING_PROBLEM_CORROBORATION: Iconoclast theologians and the imperial authority attest that the problem of idolatry and fragmented authority remains live. However, iconodule theologians and historical accounts from outside the benefiting parties contest this, arguing that the 'problem' was largely a pretext for political consolidation and that genuine idolatry was rare.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(decalogue_image_prohibition__iconoclast_reading, 'none', 1).

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
 *   The extractiveness (0.6) is substantial due to the destruction of property and livelihoods, and the suppression (0.7) is high because the prohibition is enforced through state power and persecution. The theater ratio (0.2) is relatively low, as the enforcement is genuinely aimed at eradicating images, not merely performing a ritual. Accessibility collapse is high (0.8) because the categorical nature of the prohibition leaves little room for alternatives within the accepted religious framework. Resistance (0.4) is moderate, reflecting both active opposition and passive non-compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the imperial authority and iconoclast theologians, this is a necessary measure to purify worship and consolidate legitimate religious authority. From the perspective of icon producers, monastic communities, and devotional practitioners, it is a destructive and oppressive act that undermines their faith and culture.
 *
 * DIRECTIONALITY LOGIC:
 *   The centralizing imperial authority and iconoclast theologians are clear beneficiaries, as the constraint enhances their power and theological standing. Icon producers, monastic communities, and devotional practitioners are direct victims, bearing the costs of destruction and persecution. Iconodule theologians are excluded, their arguments suppressed, making them targets of the enforcement mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to prevent idolatry and unify religious practice. While the problem of idolatry is framed as 'live' by beneficiaries, the actual function has drifted towards political consolidation and suppression of dissent. The high extractiveness and suppression, coupled with the contested status of the founding problem, suggest a snare-like operation rather than a genuine coordination mechanism, even if claimed as a necessary religious law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine interpretation of the Decalogue, or a politically motivated reading used to consolidate power?',
    'Historical and theological analysis of early Christian and Jewish traditions regarding images, independent of imperial patronage, to determine the historical prevalence and theological grounding of aniconic vs. iconodule positions.',
    'If primarily political, the constraint''s extractiveness and suppression are amplified, reclassifying it more firmly as a snare. If genuinely theological, the coordination function (preventing idolatry) might be more salient, potentially shifting it towards a tangled rope, though still extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between theological interpretation and political instrumentalization of the Decalogue''s image prohibition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (imperial decrees, physical destruction) or internalized (fear of divine wrath, social ostracism)?',
    'Post-persecution trajectory: if aniconic practices persist after imperial enforcement is removed, reclassify as partially internalized. If icon veneration immediately resurfaces, suppression was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making resistance harder. If purely structural, removal of enforcement would lead to rapid change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the image prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(deca_tr_t25, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement(deca_tr_t50, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(deca_tr_t75, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 75, 0.2).
narrative_ontology:measurement(deca_tr_t100, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(deca_be_t25, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(deca_be_t50, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(deca_be_t75, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 75, 0.6).
narrative_ontology:measurement(deca_be_t100, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 100, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(deca_su_t25, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(deca_su_t50, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(deca_su_t75, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 75, 0.7).
narrative_ontology:measurement(deca_su_t100, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconoclast_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__iconodule_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'iconoclast_reading' of the 'decalogue_image_prohibition' kernel. It represents a categorical prohibition on all religious imagery, contrasting with the 'iconodule_reading' which permits veneration, and the 'moderate_iconoclast_reading' which permits two-dimensional images under strict regulation. Each reading constitutes a distinct constraint with different structural properties and victim/beneficiary sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
