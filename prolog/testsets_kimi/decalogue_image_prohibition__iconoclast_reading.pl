% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: decalogue_image_prohibition__iconoclast_reading
 *   human_readable: Absolute Iconoclast Decalogue Reading
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This constraint instantiates the iconoclast reading of the
 *   decalogue_image_prohibition kernel: the absolute claim that any material
 *   representation used in worship constitutes idolatry. Structurally, it
 *   operates as a wall-type constraint categorically forbidding material
 *   mediation of the holy. The reading produces a sharp beneficiary/victim
 *   asymmetry: the centralizing imperial authority monopolizes legitimate
 *   religious form, while icon artisans, monastic communities, and devotional
 *   practitioners bear the costs of suppression. The coordination narrative
 *   (preventing idolatry) functions as cover for the extraction of religious
 *   authority. The sibling readingsâiconodule (permitting honor through
 *   images) and moderate iconoclast (regulating dimensionality)âare both
 *   logically foreclosed by the absolute prohibition's core premise.
 *
 * KEY AGENTS:
 *   - imperial_religious_authority: Primary beneficiary and agenda-setter (institutional/arbitrage) â monopolizes religious form through enforcement
 *   - icon_artisans: Primary target (moderate/trapped) â craft criminalized, livelihood destroyed
 *   - monastic_communities: Secondary target (organized/constrained) â property and practice suppressed
 *   - devotional_practitioners: Diffuse target (powerless/identity_locked) â spiritual practice forcibly restructured
 *   - iconodule_theologians: Excluded voice (moderate/trapped) â structurally silenced dissenters
 *   - historical_liturgical_analyst: Analytical observer â evaluates theological vs. imperial extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.82).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.88).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Absolute Iconoclast Decalogue Reading").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, 'c38a9965-8094-4271-ba9c-371cc9d285da').
narrative_ontology:cs_kernel_codification('c38a9965-8094-4271-ba9c-371cc9d285da', fixed_text).
narrative_ontology:cs_authority_grounding('c38a9965-8094-4271-ba9c-371cc9d285da', extraction).
narrative_ontology:cs_interpretation_layer_present('c38a9965-8094-4271-ba9c-371cc9d285da').
narrative_ontology:cs_reading_relation('c38a9965-8094-4271-ba9c-371cc9d285da', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_reading_relation('c38a9965-8094-4271-ba9c-371cc9d285da', decalogue_image_prohibition__moderate_iconoclast_reading, forecloses).
narrative_ontology:cs_axiom('c38a9965-8094-4271-ba9c-371cc9d285da', foundational, material_worship_representation_is_idolatry).
narrative_ontology:cs_axiom_status(material_worship_representation_is_idolatry, holdable).
narrative_ontology:cs_axiom_grounding('c38a9965-8094-4271-ba9c-371cc9d285da', material_worship_representation_is_idolatry, theological).
narrative_ontology:cs_axiom('c38a9965-8094-4271-ba9c-371cc9d285da', foundational, incarnation_does_not_sanctify_matter_for_veneration).
narrative_ontology:cs_axiom_status(incarnation_does_not_sanctify_matter_for_veneration, holdable).
narrative_ontology:cs_axiom_grounding('c38a9965-8094-4271-ba9c-371cc9d285da', incarnation_does_not_sanctify_matter_for_veneration, theological).
narrative_ontology:cs_reference_frame('c38a9965-8094-4271-ba9c-371cc9d285da', absolute_aniconic_purity).
narrative_ontology:cs_drift_state('c38a9965-8094-4271-ba9c-371cc9d285da', post_iconodule_restoration, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c38a9965-8094-4271-ba9c-371cc9d285da', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, imperial_religious_authority).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_artisans).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims exclusive authority to interpret the Decalogue's prohibition on images, orders the destruction of religious art, persecutes iconodule dissent, confiscates monastic properties, and monopolizes legitimate religious expression under imperial theological jurisdiction. Can reverse policy when politically expedient.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, imperial_religious_authority, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconoclast_reading, imperial_religious_authority, beneficiary).

% Painters, mosaicists, and sculptors whose livelihood depends on producing religious imagery; their work is destroyed, their trade criminalized, and they face impoverishment or exile with no legal market for their primary skill set in a society that has banned sacred art.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_artisans, payer,
    moderate, biographical, trapped, regional).

% Monasteries that maintain icon-centered devotion, theological libraries defending images, and monastic economies dependent on pilgrimage and patronage tied to sacred art; face property confiscation, persecution, and forced compliance with aniconic norms.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_communities, payer,
    organized, generational, constrained, regional).

% Lay believers accustomed to praying before icons, using visual imagery for spiritual instruction, and veneration of sacred portraits; forced to abandon practices central to their spiritual identity or operate in dangerous secrecy.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Theologians who argue that images honor their prototypes and that the Incarnation sanctifies matter; systematically excluded from imperial councils, their writings suppressed, and their theological position structurally erased from public discourse.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconodule_theologians, excluded,
    moderate, generational, trapped, regional).

% Analytical seat observing the structural relationship between the theological claim and the imperial monopoly; evaluates whether the constraint functions as genuine worship regulation or as religious authority extraction.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, historical_liturgical_analyst, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconoclast_reading, imperial_religious_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconoclast_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents what the reading holds to be the grave spiritual danger of idolatry by removing all material mediation from worship, thereby unifying religious practice under a single aniconic norm.
% TRANSFER_FUNCTION: Moves control over legitimate religious expression, devotional access, and monastic wealth from local artisans, monastic houses, and lay practitioners to the centralizing imperial authority.
% ABSENT_VOICES: Iconodule theologians who defend image veneration as theologically legitimate, and monastic leaders who resist the confiscation of sacred images and properties, are systematically excluded from imperial councils and public theological discourse.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished, icon production would resume openly, monastic communities would restore their devotional economies and sacred art programs, lay practitioners would return to image-based worship, and the imperial authority would lose its monopoly on defining legitimate religious form; the religious landscape would reorganize around material mediation.
% FOUNDING_PROBLEM: The perceived corruption of worship through idolatry and the fragmentation of religious authority among competing devotional centers, monastic houses, and local image-cults that threatened imperial religious unity.
% FOUNDING_PROBLEM_CORROBORATION: Iconodule theologians and the Second Council of Nicaea (787) contested the founding problem, arguing that images were not idolatrous. Later historical and art-historical scholarship outside the imperial beneficiary seat attests that the crisis was partly manufactured to justify confiscation of monastic wealth and consolidation of authority. No neutral contemporary corroboration exists; the problem was asserted by the same imperial seat that benefited from its resolution.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconoclast_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82) because the constraint transfers wealth, labor, and spiritual autonomy from devotional communities to imperial coffers and authority. Suppression is very high (0.88) because the constraint persists only through active destruction of images, persecution of dissent, and exclusion of rival theological voices; without enforcement, popular devotion reasserts itself immediately. Theater_ratio at 0.45 reflects significant performative maintenanceâpublic icon destruction, theological councils staged to ratify imperial policy, and spectacle of complianceâthough enforcement is also materially severe. Accessibility_collapse is high (0.78) because legal alternatives to imperial aniconism are structurally eliminated; underground devotion is dangerous and unsustainable. Resistance is substantial (0.72) because monastic communities and popular devotion mount sustained theological and practical opposition, including martyrdom and clandestine preservation of images. The temporal series show extraction and suppression intensifying as the enforcement apparatus matures, with theater rising as the policy requires ever more public justification.
 *
 * PERSPECTIVAL GAP:
 *   The imperial seat experiences the constraint as necessary coordinationâunifying worship, preventing idolatry, and consolidating sacred legitimacy under one authority. The artisan, monastic, and devotional seats experience the identical structure as violent extraction: the criminalization of their craft, the confiscation of their property, and the forced dissolution of their spiritual identity. The engine computes this divergence from the structural data: the imperial seat carries agenda-setter/beneficiary roles with arbitrage exit, while the devotional seats carry payer roles with trapped or identity-locked exit. The analytical seat sees both framings and measures the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial_religious_authority is the structural beneficiary (d near 0.0): it sets the constraint, enforces it, and collects the monopoly on religious form. Icon_artisans, monastic_communities, and devotional_practitioners are structural targets (d near 1.0): they bear the extraction, have trapped or identity-locked exit, and their suppression amplifies effective extraction. Iconodule_theologians are excluded targets (d near 1.0): their exclusion from discourse is itself the suppression mechanism. The historical_liturgical_analyst sits at analytical distance (d near 0.5). No overrides are necessary; the structural derivation chain produces accurate directionality from beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by distinguishing the constraint's founding theological narrative from its operational structure. The mandateâpreventing idolatryâwas asserted as a live problem by the imperial seat. However, the arrangement's persistence depends on coercion rather than consensus, its benefits concentrate in the monopolizing authority, and its victims are identifiable and bear concentrated costs. The mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) signals that the mandate has outlived its functional credibility for non-beneficiaries, even if it was ever genuine. This prevents the error of treating an imperial extraction mechanism as a settled theological mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imperial_extraction_vs_theological_genuine,
    'Does the iconoclast reading represent a genuine theological commitment to aniconism, or primarily an imperial mechanism for monopolizing religious authority and wealth?',
    'Historical analysis comparing enforcement patterns against theological development: if suppression clusters around monastic wealth confiscation and political centralization rather than theological education, the extraction mechanism dominates.',
    'If extraction dominates, the constraint remains snare-class; if genuine theological commitment dominates with incidental imperial benefit, reclassification toward tangled_rope is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_extraction_vs_theological_genuine, conceptual, 'Ambiguity between theological motive and imperial extraction in the iconoclast reading').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of image-veneration externally enforced by imperial power alone, or has it been internalized by devotees who accept the prohibition as spiritually necessary?',
    'Post-enforcement behavioral observation: if devotees immediately resume image-use when imperial enforcement relaxes, suppression was structural; if aniconic practice persists absent enforcement, suppression is partially internalized.',
    'Internalized suppression would increase effective extraction beyond the structural measure, as the target population carries the constraint with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    kernel_reading_contest_boundary,
    'Is the absolute iconoclast reading logically foreclosed by the Incarnation, or does it remain a holdable theological position independent of Christological premises?',
    'Theological analysis of the sibling readings'' axioms: if the iconoclast axiom ''incarnation_does_not_sanctify_matter'' is derivable without contradiction from its own theological framework, the reading remains holdable; if Christological logic necessitates material sanctification, the reading is internally challenged.',
    'Resolution determines whether the engine-computed foreclosure from the iconodule reading is mutual or one-directional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_boundary, conceptual, 'Theological boundary between absolute iconoclast and iconodule readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(deca_tr_t8, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(deca_tr_t16, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(deca_tr_t24, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(deca_tr_t32, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 32, 0.48).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(deca_be_t8, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 8, 0.7).
narrative_ontology:measurement(deca_be_t16, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 16, 0.78).
narrative_ontology:measurement(deca_be_t24, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 24, 0.82).
narrative_ontology:measurement(deca_be_t32, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 32, 0.84).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(deca_su_t8, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 8, 0.75).
narrative_ontology:measurement(deca_su_t16, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 16, 0.82).
narrative_ontology:measurement(deca_su_t24, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 24, 0.86).
narrative_ontology:measurement(deca_su_t32, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 32, 0.89).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
