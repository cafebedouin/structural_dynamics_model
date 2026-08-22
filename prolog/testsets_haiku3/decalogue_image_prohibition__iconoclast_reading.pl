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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Decalogue Image Prohibition (Iconoclast Reading)
 *   domain: theological/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This constraint instantiates the ICONOCLAST reading of the Decalogue
 *   image prohibition: the claim that all material representation used in
 *   worship violates the commandment against idolatry and constitutes a
 *   categorically impermissible mediation of the holy. The iconoclast reading
 *   asserts that the spirit cannot be accessed through material forms—that
 *   images inherently seduce believers into worshiping the representation
 *   rather than the prototype, and that genuine worship requires the
 *   elimination of material intermediaries. This reading is enforced through
 *   imperial and ecclesiastical authority that destroys images, prosecutes
 *   image makers, suppresses monastic scriptoria, and silences competing
 *   theological interpretations. The structural beneficiary is the
 *   centralizing imperial/ecclesiastical authority, which consolidates
 *   religious authority by eliminating the independent devotional networks
 *   that image veneration supported. The constraint's victims are icon
 *   producers (whose craft becomes criminal), monastic communities (whose
 *   theological and economic nexus around image creation is dismantled), and
 *   devotional practitioners (whose worship form is criminalized without
 *   substitution). The constraint is claimed as snare and the metrics
 *   describe substantial extraction (0.78), active enforcement (0.81
 *   suppression), and rising theater as enforcement effort expands (theater
 *   rising from 0.18 to 0.42 over the interval).
 *
 * KEY AGENTS:
 *   - Imperial centralizing authority: Sets and enforces the prohibition via ecclesiastical channels; consolidates religious authority; benefits from elimination of independent devotional networks.
 *   - Icon producers: Artisans and monastic craftspeople whose livelihood and spiritual identity center on image creation; prosecuted and dispossessed.
 *   - Monastic communities: Institutional and intellectual centers of image production, theological learning, and pilgrimage-based economy; face destruction and conscription.
 *   - Devotional practitioners: Ordinary believers whose worship centers on icons; face criminalization of their devotional form.
 *   - Rival theological readings (iconodule, moderate iconoclast): Suppressed interpreters and schools that defend image-mediation as theologically coherent and spiritually valid.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.78).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.81).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Decalogue Image Prohibition (Iconoclast Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theological/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, '18c76741-207d-4a8c-b7b2-75530d3c2531').
narrative_ontology:cs_kernel_codification('18c76741-207d-4a8c-b7b2-75530d3c2531', fixed_text).
narrative_ontology:cs_authority_grounding('18c76741-207d-4a8c-b7b2-75530d3c2531', extraction).
narrative_ontology:cs_interpretation_layer_present('18c76741-207d-4a8c-b7b2-75530d3c2531').
narrative_ontology:cs_reading_relation('18c76741-207d-4a8c-b7b2-75530d3c2531', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_reading_relation('18c76741-207d-4a8c-b7b2-75530d3c2531', decalogue_image_prohibition__moderate_iconoclast_reading, forecloses).
narrative_ontology:cs_axiom('18c76741-207d-4a8c-b7b2-75530d3c2531', foundational, material_mediation_categorically_impermissible).
narrative_ontology:cs_axiom_status(material_mediation_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('18c76741-207d-4a8c-b7b2-75530d3c2531', material_mediation_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('18c76741-207d-4a8c-b7b2-75530d3c2531', foundational, spirit_matter_ontological_separation).
narrative_ontology:cs_axiom_status(spirit_matter_ontological_separation, holdable).
narrative_ontology:cs_axiom_grounding('18c76741-207d-4a8c-b7b2-75530d3c2531', spirit_matter_ontological_separation, deontological).
narrative_ontology:cs_reference_frame('18c76741-207d-4a8c-b7b2-75530d3c2531', pure_spirit_worship_without_material_intermediary).
narrative_ontology:cs_drift_state('18c76741-207d-4a8c-b7b2-75530d3c2531', contemporary_theological_consensus_restoration, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('18c76741-207d-4a8c-b7b2-75530d3c2531', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, imperial_centralizing_authority).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_producers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, devotional_image_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, monastic_communities).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconoclast_reading, material_mediation_categorically_impermissible).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconoclast_reading, spirit_matter_ontological_separation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The emperor (or centralizing state church hierarchy) enforces the prohibition, claiming theological authority grounded in scriptural interpretation. Consolidates religious authority by monopolizing what counts as legitimate worship and eliminating the independent devotional networks that icon production supported. The enforcement machinery—destruction of images, prosecution of icon makers, suppression of monastic scriptoria—directly channels religious authority through state institutions.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, imperial_centralizing_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Artisans, monks, and specialized craftspeople who create religious images for worship and devotion. Their livelihood, professional identity, and spiritual practice are indivisible from image creation. Under the prohibition they face prosecution, confiscation of materials and completed works, and exclusion from legitimate religious economy. Exit means abandoning craft identity and denying the theological premise of their life's work.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_producers, payer,
    moderate, biographical, trapped, regional).

% Monasteries housed the scriptoria, libraries, and artistic communities that produced sacred imagery. Icon creation was integrated into monastic liturgy and theologically understood as prayer through craft. The prohibition dismantles this economic and spiritual nexus. Monasteries face destruction of accumulated libraries and artwork, loss of pilgrimage revenue dependent on image veneration, conscription of their labor into iconoclastic campaigns, and doctrinal coercion. Their institutional identity as custodians of sacred tradition is attacked as idolatrous.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_communities, payer,
    powerful, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconoclast_reading, monastic_communities, beneficiary).

% Ordinary believers—rural and urban—whose religious practice centers on icons: prayer before images, intercession through sacred depictions, household worship using painted saints. The prohibition criminalizes their devotional form and requires them to adopt new practices. No theological alternatives are offered; the constraint eliminates their existing worship modality without substitution.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, devotional_image_practitioners, payer,
    powerless, biographical, trapped, local).

% Theologians, bishops, and monastic intellectuals who maintain alternative scriptural interpretations—that images honor their prototypes without constituting worship of the images themselves, that the Incarnation sanctifies matter as a conduit to the divine. These readings are actively suppressed: their proponents are prosecuted, their written works burned, their ecclesiastical authority denied. The constraint's enforcement consists partly of silencing rival interpretive communities.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, rival_theological_readings, excluded,
    organized, generational, trapped, global).

% Scholars and contemporary theologians analyzing whether the scriptural prohibition genuinely covers all material mediation or whether the distinction between worship of an image and honor paid through an image is coherent. They observe the theological contest without institutional power to enforce either reading.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, theological_interpreters, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconoclast_reading, imperial_centralizing_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconoclast_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, uniform scriptural interpretation as the binding norm for Christian worship, eliminating competing theological readings and replacing devotional plurality with centralized doctrinal authority. Creates a shared understanding of what constitutes pious versus idolatrous practice.
% TRANSFER_FUNCTION: Moves religious authority from distributed communities (monasteries, local believers, independent theological interpreters) to imperial/centralized church hierarchy. Transfers productive capacity (scriptoria, icon workshops, pilgrimage networks) from monastic and artisanal communities into state-controlled institutions. Extracts theological legitimacy from alternative readings and consolidates it under a single enforced interpretation.
% ABSENT_VOICES: Icon-venerating communities, practicing monks and nuns, peripheral theological schools, laity whose worship depends on images—all are structurally excluded from the interpretive process. Their voices would dispute the theological reading itself and attest that images mediate prayer without becoming its object. No forum exists for them to contest the imposed interpretation.
% DISAPPEARANCE_RATIONALE: If this prohibition disappeared, monastic scriptoria would resume image production, pilgrimages to icon shrines would resume, household devotion through sacred imagery would restore itself to legitimacy, and theological schools suppressed for defending image-veneration would reemerge. The economic and spiritual networks dependent on image production would reorganize within weeks to months. The constraint's removal would reopen entire classes of religious practice and reinstitute theological competition.
% FOUNDING_PROBLEM: Concern that devotion to images had become functionally equivalent to worship of the images themselves—that believers were treating material depictions as divine rather than as transparent conduits to the divine. A response to perceived practical idolatry (confusion of image with prototype) and theological corruption.
% FOUNDING_PROBLEM_CORROBORATION: The imperial enforcing authority attests the problem is live and present in observed folk devotion. Iconoclast theologians cite scriptural passages and interpret them to support the claim. However, rival theologians (iconodules, moderate iconoclasts) attest that the distinction between worship of an image and honor through an image is sustainable, that practitioners understand it, and that image veneration observed in the field does NOT constitute worship of the image itself. The monastic and parish clergy who experienced folk devotion firsthand attest the problem is overstated or misidentified.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconoclast_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The extractiveness (0.78) reflects the constraint's dependence on eliminating productive capacity (scriptoria, icon workshops, pilgrimage networks) and transferring authority from distributed communities to centralized institutions. The suppression (0.81) is high because the constraint's persistence depends on continuously preventing image production and prosecuting practitioners—it cannot rely on participant preference or default compliance. The theater_ratio (0.42, rising from 0.18) indicates that enforcement effort is increasingly dedicated to maintaining the prohibition's theatrical legitimacy rather than managing material practice itself. The measurement series shows extractiveness and suppression rising steeply through t0-t24 (the period of aggressive enforcement), then plateauing t24-t50 (a plateau consistent with exhaustion of victims or shift to maintenance theater). The accessibility_collapse (0.72) reflects how completely the constraint forecloses alternatives: image production is not merely illegal, it is defined as spiritually dangerous, leaving no legitimate exit path except apostasy or silence. The resistance (0.68) remains substantial throughout the interval, indicating that the constraint meets persistent opposition from monastic networks, rival theologians, and lay practitioners who experience their devotional form as spiritually necessary—the constraint does not achieve Rope-level acceptance. The coercion grid shows stakes inflation and suppression both rising most sharply at the class and organizational levels (monastic institutions, theological schools) where structural resistance is concentrated, while accessibility collapse and resistance are most pronounced at the individual level (ordinary believers most completely trapped). This leveled pattern indicates the constraint operates through institutional dismantling at the structural and organizational levels (shuttering scriptoria, prosecuting theologians) while compounding individual pressure (criminalizing household devotion) from above.
 *
 * PERSPECTIVAL GAP:
 *   The imperial enforcer (agenda_setter seat) experiences this constraint as theological purification and institutional renewal—restoring true worship by eliminating idolatrous intermediaries. From this seat the constraint solves a real problem (devotional corruption perceived as image-worship) and the beneficiary is religious integrity itself. The icon producer and monastic seats experience the same structure as industrial destruction and expertise confiscation—their craft is not reformed, it is criminalized, and their theological defense of image-mediation is silenced before it can be heard. From these seats the 'founding problem' of devotional corruption is either overstated (lay practitioners insist they distinguish image from prototype) or misidentified (confusion of image with prototype is rare in observed practice). The payer seats experience trapped exits: apostasy, silence, or emigration are the only paths; theological conversion is coerced, not chosen. The engine computes this divergence from the power atoms, exit options, and beneficiary/victim declarations: the powerful institutional enforcer has arbitrage-grade exit options and controls which theological readings count as legitimate; the moderate icon producers have trapped or identity-locked exit (craft and theology are fused); the powerless devotional practitioners have trapped exit (no alternative worship form is offered). These structural asymmetries automatically produce divergent directionalities and per-seat type classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial authority sits at d ≈ 0.0 (full beneficiary): it enforces the constraint, controls the theological interpretation, derives institutional consolidation, and faces no material cost. The icon producers sit at d ≈ 0.95 (full target): they pay through dispossession and prosecution, have no enforcement power, and their exit options are trapped—leaving the craft means abandoning theological commitment and professional identity. Monastic communities sit at d ≈ 0.88 (strong target): they face institutional dismantling, library destruction, and conscription into iconoclastic campaigns, though they retain some organizational power to negotiate or hide materials. Devotional practitioners sit at d ≈ 0.92 (strong target): they are powerless, their worship form is criminalized, and they face social pressure and theological re-education. Rival theologians sit at d ≈ 0.85 (strong target): their readings are suppressed, their writings burned, their ecclesiastical authority denied. No directionality overrides are required—the structural data (imperial power, institutional resources, victim prosecution, victim powerlessness) already yield the appropriate d-values without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy mislabeling by holding its snare classification steady: the founding problem (preventing devotional image-worship) is contested in status (rivals attest the problem is overstated or misidentified), but the constraint's enforcement does not depend on solving the founding problem—it depends on eliminating independent devotional networks. If the founding problem were solved (if believers genuinely ceased image-worship or the risks of it were shown to be theoretical), the constraint would persist because its beneficiary (imperial authority) would continue to benefit from the institutional consolidation it achieves. The constraint would not dissolve; it would merely shift from 'solving a problem' to 'preserving institutional control.' This is exactly the snare signature: persistence independent of functional problem-solving, depending instead on active enforcement and suppression of alternatives. The rising theater_ratio (prosecution intensity, theological literature burning, enforcement theater) while metrics plateau suggests the constraint is entering Piton territory—but the continuing resistance (0.68 at interval end) and active suppression (0.81) indicate it is still a living snare, not yet a degraded piton. The founding_problem_status (contested) × disappearance_verdict (world_rearranges) mismatch does NOT fire a mandatrophy flag because the constraint's termination would indeed cause rearrangement (restoration of image production, re-emergence of devotional practices, reinstitution of rival theological schools), confirming its material persistence does not depend on the founding problem remaining live—it depends on enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the scriptural prohibition genuinely categorical against all material mediation of the holy, or does it forbid only the worshiping of images themselves—leaving honor through images to their prototypes coherent?',
    'Textual hermeneutics (comparing Hebrew ''pesel''/''massekhah'' with Greek and Syriac translations and their usage contexts), historical practice analysis (pre-prohibition devotional patterns in early Christian communities and Judaism), and phenomenological analysis of what it means linguistically to ''worship'' versus ''honor through.''',
    'If the textual prohibition is narrow (forbids image-worship, not image-mediation), the iconoclast reading''s victim set dissolves and the constraint reclassifies from snare toward rope—a genuine theological boundary-setting without systematic extraction. If the prohibition is categorical, the reading holds; if neither reading is textually decisive, the contest is genuinely unresolved at the exegetical level and both readings remain live.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The fundamental hermeneutical dispute: is the prohibition categorical or permitting of distinction?').

omega_variable(
    suppression_mechanism_internalization,
    'When icon veneration practitioners internalize the iconoclast prohibition (experience it as religiously correct rather than externally coerced), is that genuine theological conviction or cognitive capture by the enforcement apparatus?',
    'Post-enforcement lapse analysis: if image veneration returns immediately when external suppression ends (as occurred during the Restoration), internalization was incomplete and suppression was primarily structural. If it does not return despite opportunity, part of the suppression entered as genuine conviction. Textual analysis of theological writings by former practitioners: do they articulate a coherent alternative theology, or do they perform compliance rhetoric?',
    'If suppression is primarily structural, the victim set remains trapped. If significant internalization occurs, victims may transition toward identity-lock (they have internalized the prohibition''s framing of images as spiritually dangerous). The theater_ratio rising through the interval suggests increasing performance—enforcement effort outpacing genuine theological buy-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in theological constraint dynamics').

omega_variable(
    imperial_benefit_capture,
    'Is the constraint''s primary beneficiary the imperial authority, or is the constraint driven by genuine theological concern about devotional corruption that happened to align with imperial centralizing interests?',
    'Institutional analysis: does the imperial authority enforce the prohibition consistently across power disparities (prosecuting powerful monasteries and weak lay practitioners equally, or selectively), or does enforcement track institutional threat rather than theological violation? Historical comparison: are iconoclast policies consistent with other centralizing measures (suppression of independent clergy, confiscation of monastic property, conscription of religious authority into state apparatus), or are they isolated theological choices?',
    'If enforcement is selective and tracks imperial consolidation rather than uniform theological application, the constraint is primarily extractive, serving institutional power consolidation. If enforcement is internally consistent, the theological reading may be genuinely held by the imperial enforcer, making the constraint a case of competing theological readings where one happens to be enforced by institutional power. The measured 0.78 extractiveness reflects the first hypothesis; full theological sincerity would typically show lower extraction and higher resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_benefit_capture, empirical, 'Whether the constraint serves theological purification or institutional consolidation').

omega_variable(
    rival_reading_suppression_as_extraction,
    'Does the prohibition constitute a mechanism for suppressing rival theological readings as much as for suppressing image production itself?',
    'Enforcement records: what proportion of prosecutions target image producers vs. theologians defending image veneration? Which writings are burned—images or theological texts defending images? Are rival theological schools closed or their library holdings destroyed? Does the enforcing authority prosecute the theological claim (defending the reading) or only the material practice?',
    'If the constraint''s enforcement machinery is substantially directed at silencing rival interpretations and their exponents, the constraint functions as a reading-monopoly device, making the imperial authority itself a victim of suppression (forced conformity to a single theological reading they may not hold). The ''excluded'' status of rival_theological_readings would be upgraded to ''payer'' status if this omega resolves toward suppression of reading rather than material practice alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rival_reading_suppression_as_extraction, empirical, 'Whether the constraint suppresses material practices, rival theological readings, or both').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(deca_tr_t0, projected).
narrative_ontology:measurement(deca_tr_t8, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(deca_tr_t8, observed).
narrative_ontology:measurement(deca_tr_t16, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement_basis(deca_tr_t16, observed).
narrative_ontology:measurement(deca_tr_t24, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(deca_tr_t24, observed).
narrative_ontology:measurement(deca_tr_t35, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(deca_tr_t35, observed).
narrative_ontology:measurement(deca_tr_t50, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(deca_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(deca_be_t0, projected).
narrative_ontology:measurement(deca_be_t8, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(deca_be_t8, observed).
narrative_ontology:measurement(deca_be_t16, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement_basis(deca_be_t16, observed).
narrative_ontology:measurement(deca_be_t24, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 24, 0.74).
narrative_ontology:measurement_basis(deca_be_t24, observed).
narrative_ontology:measurement(deca_be_t35, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement_basis(deca_be_t35, observed).
narrative_ontology:measurement(deca_be_t50, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 50, 0.78).
narrative_ontology:measurement_basis(deca_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(deca_su_t0, projected).
narrative_ontology:measurement(deca_su_t8, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 8, 0.71).
narrative_ontology:measurement_basis(deca_su_t8, observed).
narrative_ontology:measurement(deca_su_t16, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 16, 0.76).
narrative_ontology:measurement_basis(deca_su_t16, observed).
narrative_ontology:measurement(deca_su_t24, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 24, 0.79).
narrative_ontology:measurement_basis(deca_su_t24, observed).
narrative_ontology:measurement(deca_su_t35, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 35, 0.81).
narrative_ontology:measurement_basis(deca_su_t35, observed).
narrative_ontology:measurement(deca_su_t50, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 50, 0.81).
narrative_ontology:measurement_basis(deca_su_t50, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(deca_grid_01, decalogue_image_prohibition__iconoclast_reading, accessibility_collapse(class), 0, 0.52).
narrative_ontology:measurement(deca_grid_02, decalogue_image_prohibition__iconoclast_reading, accessibility_collapse(class), 50, 0.68).
narrative_ontology:measurement(deca_grid_03, decalogue_image_prohibition__iconoclast_reading, accessibility_collapse(individual), 0, 0.48).
narrative_ontology:measurement(deca_grid_04, decalogue_image_prohibition__iconoclast_reading, accessibility_collapse(individual), 50, 0.74).
narrative_ontology:measurement(deca_grid_05, decalogue_image_prohibition__iconoclast_reading, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(deca_grid_06, decalogue_image_prohibition__iconoclast_reading, accessibility_collapse(organizational), 50, 0.7).
narrative_ontology:measurement(deca_grid_07, decalogue_image_prohibition__iconoclast_reading, accessibility_collapse(structural), 0, 0.62).
narrative_ontology:measurement(deca_grid_08, decalogue_image_prohibition__iconoclast_reading, accessibility_collapse(structural), 50, 0.72).
narrative_ontology:measurement(deca_grid_09, decalogue_image_prohibition__iconoclast_reading, resistance(class), 0, 0.64).
narrative_ontology:measurement(deca_grid_10, decalogue_image_prohibition__iconoclast_reading, resistance(class), 50, 0.66).
narrative_ontology:measurement(deca_grid_11, decalogue_image_prohibition__iconoclast_reading, resistance(individual), 0, 0.58).
narrative_ontology:measurement(deca_grid_12, decalogue_image_prohibition__iconoclast_reading, resistance(individual), 50, 0.72).
narrative_ontology:measurement(deca_grid_13, decalogue_image_prohibition__iconoclast_reading, resistance(organizational), 0, 0.71).
narrative_ontology:measurement(deca_grid_14, decalogue_image_prohibition__iconoclast_reading, resistance(organizational), 50, 0.68).
narrative_ontology:measurement(deca_grid_15, decalogue_image_prohibition__iconoclast_reading, resistance(structural), 0, 0.72).
narrative_ontology:measurement(deca_grid_16, decalogue_image_prohibition__iconoclast_reading, resistance(structural), 50, 0.64).
narrative_ontology:measurement(deca_grid_17, decalogue_image_prohibition__iconoclast_reading, stakes_inflation(class), 0, 0.61).
narrative_ontology:measurement(deca_grid_18, decalogue_image_prohibition__iconoclast_reading, stakes_inflation(class), 50, 0.78).
narrative_ontology:measurement(deca_grid_19, decalogue_image_prohibition__iconoclast_reading, stakes_inflation(individual), 0, 0.54).
narrative_ontology:measurement(deca_grid_20, decalogue_image_prohibition__iconoclast_reading, stakes_inflation(individual), 50, 0.75).
narrative_ontology:measurement(deca_grid_21, decalogue_image_prohibition__iconoclast_reading, stakes_inflation(organizational), 0, 0.68).
narrative_ontology:measurement(deca_grid_22, decalogue_image_prohibition__iconoclast_reading, stakes_inflation(organizational), 50, 0.82).
narrative_ontology:measurement(deca_grid_23, decalogue_image_prohibition__iconoclast_reading, stakes_inflation(structural), 0, 0.71).
narrative_ontology:measurement(deca_grid_24, decalogue_image_prohibition__iconoclast_reading, stakes_inflation(structural), 50, 0.84).
narrative_ontology:measurement(deca_grid_25, decalogue_image_prohibition__iconoclast_reading, suppression(class), 0, 0.58).
narrative_ontology:measurement(deca_grid_26, decalogue_image_prohibition__iconoclast_reading, suppression(class), 50, 0.78).
narrative_ontology:measurement(deca_grid_27, decalogue_image_prohibition__iconoclast_reading, suppression(individual), 0, 0.62).
narrative_ontology:measurement(deca_grid_28, decalogue_image_prohibition__iconoclast_reading, suppression(individual), 50, 0.82).
narrative_ontology:measurement(deca_grid_29, decalogue_image_prohibition__iconoclast_reading, suppression(organizational), 0, 0.65).
narrative_ontology:measurement(deca_grid_30, decalogue_image_prohibition__iconoclast_reading, suppression(organizational), 50, 0.82).
narrative_ontology:measurement(deca_grid_31, decalogue_image_prohibition__iconoclast_reading, suppression(structural), 0, 0.68).
narrative_ontology:measurement(deca_grid_32, decalogue_image_prohibition__iconoclast_reading, suppression(structural), 50, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconoclast_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__iconoclast_reading, 0.12).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__iconodule_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% The decalogue image prohibition decomposes into three structurally distinct constraints corresponding to three live theological readings of the same scriptural text. The iconoclast reading (this file) asserts categorical prohibition of all material mediation; the iconodule reading permits honor through images to their prototypes; the moderate iconoclast reading permits two-dimensional images under regulation. These readings entail different victim sets (icon producers vs. icon defenders vs. sculptors), different ε values (high extraction vs. low extraction vs. moderate extraction), and different persistence mechanisms. They are linked through network.affects_constraints because they are readings of one kernel and shifts in one reading's fortunes affect the others' legitimacy conditions and resource availability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
