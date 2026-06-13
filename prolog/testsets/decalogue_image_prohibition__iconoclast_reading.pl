% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Iconoclast Reading: Total Prohibition of Religious Imagery
 *   domain: theological/religious_authority
 *
 * SUMMARY:
 *   The iconoclast reading of the Decalogue prohibition interprets 'Thou
 *   shalt not make unto thee any graven image' as a categorical ban on all
 *   material representation used in worship. Any image—icon, statue,
 *   mosaic—that mediates devotion constitutes idolatry because matter cannot
 *   legitimately channel access to the immaterial divine. This reading was
 *   enforced by centralizing imperial and ecclesiastical authorities,
 *   particularly in the Byzantine Empire during the iconoclastic period
 *   (8th–early 9th century), resulting in the destruction of icons,
 *   prohibition of their production, suppression of image-centered monastic
 *   practices, and economic devastation for icon artisans. The reading
 *   coexists with the iconodule reading (which permits images as aids to
 *   devotion), creating a constraint family where the two readings are held
 *   as contradictory positions by different institutional actors.
 *
 * KEY AGENTS:
 *   - Centralizing imperial religious authority: sets and enforces the prohibition; gains monopoly control over devotional form; justifies rule as theological fidelity.
 *   - Icon producers and artisans: face prohibition of their craft; cannot exit without abandoning livelihood, technical mastery, and community.
 *   - Monastic image communities: identity-locked to image veneration; forced choice between doctrinal compliance and institutional survival.
 *   - Lay devotional practitioners: dependent on image veneration; access to their primary spiritual medium is cut off.
 *   - Rival icon-venerating authorities outside jurisdiction: excluded from enforcement apparatus; their counter-testimony would sustain the sibling reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.81).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.88).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, tangled_rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Iconoclast Reading: Total Prohibition of Religious Imagery").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theological/religious_authority").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, 'd347d988-7a3a-40e7-a8bf-6981371076ed').
narrative_ontology:cs_kernel_codification('d347d988-7a3a-40e7-a8bf-6981371076ed', fixed_text).
narrative_ontology:cs_authority_grounding('d347d988-7a3a-40e7-a8bf-6981371076ed', lineage).
narrative_ontology:cs_interpretation_layer_present('d347d988-7a3a-40e7-a8bf-6981371076ed').
narrative_ontology:cs_reading_relation('d347d988-7a3a-40e7-a8bf-6981371076ed', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_reading_relation('d347d988-7a3a-40e7-a8bf-6981371076ed', decalogue_image_prohibition__moderate_iconoclast_reading, influences).
narrative_ontology:cs_axiom('d347d988-7a3a-40e7-a8bf-6981371076ed', foundational, material_mediation_categorically_impermissible).
narrative_ontology:cs_axiom_status(material_mediation_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('d347d988-7a3a-40e7-a8bf-6981371076ed', material_mediation_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('d347d988-7a3a-40e7-a8bf-6981371076ed', secondary, visual_form_incompatible_with_monotheistic_purity).
narrative_ontology:cs_axiom_status(visual_form_incompatible_with_monotheistic_purity, holdable).
narrative_ontology:cs_axiom_grounding('d347d988-7a3a-40e7-a8bf-6981371076ed', visual_form_incompatible_with_monotheistic_purity, theological).
narrative_ontology:cs_reference_frame('d347d988-7a3a-40e7-a8bf-6981371076ed', invisible_divine_worship_framework).
narrative_ontology:cs_drift_state('d347d988-7a3a-40e7-a8bf-6981371076ed', post_seventh_council_repudiation, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('d347d988-7a3a-40e7-a8bf-6981371076ed', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_religious_authority).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_producers_artisans).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_image_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners_dependent_on_imagery).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
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
 *   Extractiveness reaches 0.81 because the constraint consolidates religious authority by eliminating competing loci of religious legitimacy (monasteries, popular image cults) and redistributes authority to the centralizing apparatus. The constraint is not extractive of material wealth directly—it forbids production of the commodity—but extracts religious authority and spiritual gatekeeping. Suppression is high (0.88) because the constraint's persistence depends on active enforcement: burning images, prohibiting production, monitoring for hidden icons, punishing violators. Without sustained suppression machinery, icon production and veneration would resume. Theater rises over the interval (0.25 to 0.42) as the constraint's original theological justification is preserved in rhetoric while enforcement mechanisms become increasingly bureaucratic and punitive—the theological logic becomes increasingly performative as it must override the actual devotional needs of the population. Accessibility collapse is high (0.79) because once the prohibition is understood, alternatives to image veneration within the same institutional church are very limited; practitioners face the hard choice of accepting a new devotional form or seeking heterodox communities. Resistance is substantial (0.71) because monastic communities, icon artisans, and lay practitioners actively resist through hidden production, private veneration, and theological counter-argument.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (icon producers, monastic communities, lay practitioners) and the agenda-setter seat (imperial authority) compute different types from the same structural data. From the authority's seat, the arrangement appears as rope: genuine coordination around monotheistic purity, eliminating idolatry, creating uniform practice. From the payer seats, especially monastic and lay, the same arrangement computes as tangled_rope or snare: coordination is cover, extraction is the function (consolidating authority), the costs are real (lost livelihood, lost spiritual practice, identity crisis). The engine should compute this divergence from the structural data without requiring the claim to reconcile it.
 *
 * DIRECTIONALITY LOGIC:
 *   The centralizing imperial authority is the structural beneficiary: directionality near the beneficiary end because the constraint consolidates its authority, monopolizes religious legitimacy, and faces no real cost for maintaining the rule (it can enforce it at will). Icon producers and lay practitioners are the structural targets: directionality near the target end because they lose their livelihood, spiritual practice, and access to their primary religious medium, with no real exit (geographic mobility is constrained by the extent of enforcement jurisdiction). Monastic communities occupy a complex seat: they hold the beneficiary role insofar as they gain exclusive claim to spiritual rigor and doctrinal purity, but they hold the payer role insofar as their identity-locked attachment to image production creates irreconcilable conflict. For monastic communities, directionality sits near 0.50–0.60: the constraint damages their economic model and identity but provides them with doctrinal authority (the appearance of rigor over compromise). The theological scholiasts and rival icon-venerating authorities have directionality modulated by their distance from the enforcement apparatus: those providing argumentative cover for the prohibition (scholiasts aligned with the authority) sit near beneficiary; those outside the jurisdiction sit near target or excluded.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mislabeling pure extraction as coordination by declaring both beneficiaries and victims explicitly. The victims (icon producers, monastic communities, devotional practitioners) make clear that the arrangement produces asymmetric costs, not shared benefits. The beneficiary (centralizing imperial authority) declares who gains from the consolidation. The claim of tangled_rope captures both the genuine theological coordination problem (What counts as idolatry?) and the real extraction (Authority consolidates through control of devotional form). If the constraint were misclaimed as rope, the victims would flag the error; if misclaimed as mountain (natural theological law), the declared beneficiaries and the high suppression requirement would flag the fabrication.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_vs_reading_identity,
    'Is this constraint one reading of the Decalogue kernel (iconoclast interpretation), or is the total image prohibition itself the genuine kernel and iconodule the deviant reading?',
    'Historical genealogy: which interpretation appears earlier in the textual tradition? Which appears as gloss or reinterpretation vs. primary claim? Later ecumenical councils provide corroboration: the seventh council (787 CE) affirmed iconodule against iconoclast, treating iconoclasm as a departure from received tradition.',
    'If iconodule is the received reading and iconoclasm is the innovation, the power relation inverts: the centralizing authority is imposing a novel interpretation under the guise of recovering original law. If iconoclasm is primary and iconodule is later accommodation, the authority''s claim to restoration is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_identity, empirical, 'Which reading is the kernel''s received interpretation vs. which is a contestation.').

omega_variable(
    material_mediation_theological_necessity,
    'Is the claim that ''matter cannot mediate the divine'' a theological truth independent of observational context, or does it depend on the theological framework within which it is asserted?',
    'Comparative theology: Do all monotheistic traditions accept the material-mediation prohibition, or do those allowing image veneration have coherent theological frameworks justifying material access to the divine? The Incarnation doctrine (central in Christian theology) offers one such framework: if God became matter in Christ, can matter not be a legitimate medium to the divine?',
    'If the claim is framework-dependent, the iconoclast and iconodule readings are two coherent theological positions within Christianity, neither logically foreclosing the other. If the claim is transtheological, iconoclasm is closer to a natural law (mountain) while iconodule is extraction riding on a theological commitment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(material_mediation_theological_necessity, conceptual, 'Whether material-mediation impermissibility is a theological given or framework-dependent doctrine.').

omega_variable(
    identity_locked_vs_economic_exit,
    'For monastic communities, is the attachment to image production an identity-fused theological commitment that cannot be exited, or is it an economic practice that can be relocated or substituted if the external constraint shifts?',
    'Post-enforcement data: when the prohibition is later repealed (as historically occurred), do monastic communities that complied return immediately to image veneration, or do they maintain the prohibition? Do they reconstruct their identity around images or find alternative devotional forms?',
    'If genuinely identity-locked, the suppression is internalized and will persist after external enforcement ceases. If the lock is economic and institutional contingency, exit becomes mobile once the constraint vanishes. The difference refines the suppression mechanism: structural vs. internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_economic_exit, empirical, 'Whether monastic image-attachment is identity fusion or relocatable economic practice.').

omega_variable(
    theological_cover_vs_political_structure,
    'Is the iconoclast reading a sincere theological interpretation of Decalogue, or is it a cover story for political centralization (consolidating authority by eliminating competing loci of religious legitimacy)?',
    'Structural analysis: If the prohibition targets icon production and monastic autonomy but exempts other uses of materials (gold vessels, architectural ornament, written texts), the pattern suggests selective enforcement protecting certain material practices while forbidding others. If the prohibition is universally enforced regardless of political utility, it appears more theologically sincere.',
    'If primarily a cover story, the constraint is snare rather than tangled_rope: coordination is pretense, extraction is the function. If genuine theology, the constraint balances real coordination (monotheistic purity) against real extraction (authority consolidation), making tangled_rope more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_cover_vs_political_structure, empirical, 'Whether the prohibition serves theological coherence or political consolidation.').

omega_variable(
    sibling_reading_coexistence,
    'Are the iconoclast and iconodule readings mutually foreclosing (one framework cannot hold both), or do they coexist as two live theological positions adopted by different institutional actors?',
    'Historical fact: Both readings were held as live, defensible theological positions by major institutional actors (imperial authorities vs. later ecumenical councils, Eastern vs. Western branches). The seventh ecumenical council explicitly condemned iconoclasm and affirmed iconodule as orthodox, yet iconoclasm resurfaced later. This pattern suggests coexistence rather than foreclosure.',
    'If coexisting, the readings form a genuine constraint family linked by network effects, not a hierarchical dismissal of one by the other. The classification of iconoclast constraint depends on its relation to the broader institutional landscape where iconodule also holds authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, empirical, 'Whether iconoclast and iconodule readings logically foreclose or genuinely coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(deca_tr_t5, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(deca_tr_t10, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(deca_tr_t15, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(deca_tr_t25, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(deca_be_t5, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(deca_be_t10, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement(deca_be_t15, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(deca_be_t25, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 25, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(deca_su_t5, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 5, 0.76).
narrative_ontology:measurement(deca_su_t10, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 10, 0.81).
narrative_ontology:measurement(deca_su_t15, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 15, 0.84).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 20, 0.86).
narrative_ontology:measurement(deca_su_t25, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 25, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconoclast_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__iconodule_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% The decalogue_image_prohibition kernel supports three distinct constraint stories, one per major theological reading: (1) iconoclast_reading (this story)—total image prohibition, high extraction from image producers and devotional practitioners; (2) iconodule_reading—images permitted under regulation, lower extraction, different victim set; (3) moderate_iconoclast_reading—dimensional distinction (statuary forbidden, two-dimensional images regulated), intermediate extraction profile. All three share the same founding kernel (Exodus 20:4 interpretation) but instantiate different ε values and victim/beneficiary structures. The three readings form a constraint family related by network causality: the iconoclast reading's enforcement creates pressure on the iconodule reading's institutional survival (affects_constraints edges point forward from stricter to more permissive readings). The iconodule reading's theological arguments (Incarnation sanctification) directly foreclose the iconoclast reading's core premise within a unified theological framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decalogue_image_prohibition__iconoclast_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
