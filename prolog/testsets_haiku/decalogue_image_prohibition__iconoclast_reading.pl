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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Prohibition on Religious Imagery (Iconoclast Reading)
 *   domain: theological/religious_authority
 *
 * SUMMARY:
 *   This constraint instantiates the ICONOCLAST READING of the Decalogue's
 *   prohibition on graven images. The reading interprets the commandment as
 *   categorically forbidding ALL material representation used in worship: the
 *   mediation of the holy through sensory, material form is inherently
 *   idolatrous and spiritually enslaving. This is one of two coherent
 *   theological readings of the same kernel text. The iconoclast reading
 *   claims the prohibition emerges naturally from scriptural law and divine
 *   instruction. It is AUTHORED AS A MOUNTAIN because the iconoclast position
 *   grounds itself in a claim of irreducible spiritual law: matter
 *   categorically cannot mediate the holy without enslaving the perceiver.
 *   However, the constraint also declares identifiable beneficiaries
 *   (imperial authority, ascetic theology faction) and victims (image
 *   communities, devotional practitioners, icon producers). This triggers the
 *   false-summit evaluation: is this a natural law, or a constructed
 *   constraint that benefits identifiable actors? The measurement series
 *   tracks how suppression intensifies as the prohibition is enforced, how
 *   extractiveness accumulates as the authority monopolizes religious
 *   interpretation, and how theater grows as enforcement machinery justifies
 *   itself by referencing the founding theological problem.
 *
 * KEY AGENTS:
 *   - centralizing_imperial_authority: agenda-setter (institutional power); monopolizes interpretation and enforcement
 *   - ascetic_theological_faction: beneficiary (powerful, mobile); gains doctrinal authority and state endorsement
 *   - icon_producers: payer (moderate, constrained); profession criminalized and destroyed
 *   - monastic_image_communities: payer (organized, identity-locked); centuries of practice declared heretical
 *   - devotional_practitioners: payer (powerless, trapped); spiritual tradition criminalized
 *   - iconodule_theologians: excluded (powerful, trapped); voice suppressed by state authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.82).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.89).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, mountain).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Prohibition on Religious Imagery (Iconoclast Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theological/religious_authority").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).
domain_priors:emerges_naturally(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, 'd4f0e51a-986f-44ad-9b62-545d5ae7fc80').
narrative_ontology:cs_kernel_codification('d4f0e51a-986f-44ad-9b62-545d5ae7fc80', fixed_text).
narrative_ontology:cs_authority_grounding('d4f0e51a-986f-44ad-9b62-545d5ae7fc80', lineage).
narrative_ontology:cs_interpretation_layer_present('d4f0e51a-986f-44ad-9b62-545d5ae7fc80').
narrative_ontology:cs_reading_relation('d4f0e51a-986f-44ad-9b62-545d5ae7fc80', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_axiom('d4f0e51a-986f-44ad-9b62-545d5ae7fc80', foundational, material_mediation_categorically_impermissible).
narrative_ontology:cs_axiom_status(material_mediation_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('d4f0e51a-986f-44ad-9b62-545d5ae7fc80', material_mediation_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('d4f0e51a-986f-44ad-9b62-545d5ae7fc80', secondary, sensory_attachment_enslaves_spirit).
narrative_ontology:cs_axiom_status(sensory_attachment_enslaves_spirit, holdable).
narrative_ontology:cs_axiom_grounding('d4f0e51a-986f-44ad-9b62-545d5ae7fc80', sensory_attachment_enslaves_spirit, empirically_contingent).
narrative_ontology:cs_reference_frame('d4f0e51a-986f-44ad-9b62-545d5ae7fc80', immaterial_worship_purity).
narrative_ontology:cs_drift_state('d4f0e51a-986f-44ad-9b62-545d5ae7fc80', late_enforcement_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d4f0e51a-986f-44ad-9b62-545d5ae7fc80', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, ascetic_theological_faction).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_producers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_image_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, imperial_enforcement_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and enforces the prohibition through imperial decree and religious councils. Controls interpretation of sacred law and monopolizes the authority to adjudicate what constitutes idolatry. Collects compliance through enforcement machinery (destruction of images, monastic oversight, heresy tribunals). Justifies the prohibition as literal obedience to divine law; uses enforcement to consolidate state religious authority against parallel centers of ritual power (icon-venerating monasteries, local shrine communities).
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority, agenda_setter,
    institutional, generational, arbitrage, regional).

% Endorses the prohibition on theological grounds: material representation constitutes a barrier to pure spiritual communion and enslaves the mind to sensory illusion. Gains institutional authority and doctrinal victory through the prohibition's adoption. Their theological framework becomes state-endorsed; competing devotional practices are delegitimized. Not subject to enforcement against themselves; can practice immaterial worship freely.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, ascetic_theological_faction, beneficiary,
    powerful, generational, mobile, regional).

% Professional artisans and workshops producing religious imagery for sale to communities and pilgrims. Their entire economic niche is criminalized. Face confiscation of materials, destruction of finished work, prosecution for heresy. Cannot exit to secular artistic production without complete retraining and loss of established clientele and reputation. Their skill (icon painting, sacred sculpture) becomes unmarketable; the constraint destroys an entire profession.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_producers, payer,
    moderate, biographical, constrained, regional).

% Monasteries that incorporated image veneration into their liturgical and contemplative practice. Their forms of prayer, ritual, and spiritual pedagogy are declared idolatrous. Face institutional pressure to destroy libraries of sacred images, strip churches of icons, and alter centuries of practice. Identity is constituted through image-centered devotion; the prohibition forces either doctrinal repudiation of their own spiritual tradition or formal heresy and suppression. They are excluded from the council that authored the prohibition and have no voice in its interpretation.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_image_communities, payer,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconoclast_reading, monastic_image_communities, excluded).

% Lay believers whose prayer practices, pilgrimage habits, and spiritual life center on interaction with sacred images. Possession of icons becomes evidence of heresy. Their devotional life is criminalized; they cannot publicly practice what their faith tradition taught them. Exit means renouncing their understood path to the divine or seeking hidden worship in violation of imperial law. No alternatives are available within their theological tradition—the prohibition is presented as the only correct reading of the commandment.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners, payer,
    powerless, biographical, trapped, regional).

% Theological voices (particularly later Eastern theologians, some church mothers, mystical theologians) who argue for a coherent iconodule position based on the Incarnation: matter is sanctified by God's embodiment; images can mediate without becoming idols; distinction between worship of images (latria) and veneration of icons as windows to their prototypes (dulia). These voices are excluded from the imperial councils that author the prohibition and are suppressed when they attempt to speak. Their theological framework is defined as heretical.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconodule_theologians, excluded,
    powerful, generational, trapped, regional).

% Military and ecclesiastical bureaucracy that carries out the prohibition: identifies and destroys images, prosecutes producers and venerators, maintains surveillance over monastic communities, administers heresy trials. Bears the administrative cost of enforcement while consolidating imperial authority. The prohibition justifies a permanent enforcement apparatus; removal of the constraint would dissolve the apparatus's rationale and function.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, imperial_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconoclast_reading, imperial_enforcement_apparatus, payer).

% The theological proposition that matter is categorically unfit as a medium for the holy; that sensory meditation enslaves rather than elevates the mind; that only immaterial communion with the transcendent is spiritually valid. This framework is non-agential but collects legitimacy through the prohibition's enforcement: every destroyed image, every prosecuted devotional practitioner, reinforces the framework's authority.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, higher_authority_framework, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(decalogue_image_prohibition__iconoclast_reading, higher_authority_framework).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconoclast_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% FOUNDING_PROBLEM: Material representation in worship creates idolatry by attaching spiritual devotion to sensory objects; the mind enslaved to images cannot achieve pure communion with the transcendent; the commandment forbids all graven images to protect against this spiritual entrapment.
% FOUNDING_PROBLEM_CORROBORATION: The ascetic theological faction attests the problem is live and urgent: sensory attachment is a documented spiritual danger; images continue to seduce believers into worship-of-the-image rather than worship-through-the-image. Iconodule theologians (excluded from state councils) attest the problem is a category error: the problem is intention (idolatry), not mediation (imagery); the Incarnation proves God chose matter as a conduit; image veneration can be practiced for centuries without the documented idolatry-collapse the prohibition claims to prevent. Historical evidence from image-venerating Orthodox, Catholic, and Buddhist traditions shows sustained practice without the spiritual catastrophe the prohibition predicts.
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconoclast_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.82 at endpoint) measures how thoroughly the prohibition transfers authority over religious meaning-making from local image-venerating communities to state/ecclesiastical hierarchies. The measurement series shows extraction accumulating as enforcement machinery matures (early 0.68 → plateau 0.82 by interval midpoint). Suppression (0.89) is high and rising because the prohibition's persistence depends on actively preventing image production, destroying extant images, prosecuting venerators, and silencing theological alternatives. The suppression is not merely structural barrier (images are forbidden) but requires continuous enforcement (destruction, prosecution, surveillance). Theater ratio (0.48 at endpoint) captures the rising proportion of enforcement activity devoted to defending the prohibition's legitimacy rather than solving the founding theological problem. Early in the interval, enforcement is pitched as addressing the real spiritual danger (prevention of idolatry); later, enforcement becomes increasingly abstract—destroying images that have not sparked idolatry, prosecuting devotional acts that pose no documented spiritual risk—suggesting enforcement now defends the RULE itself rather than the PROBLEM it addressed. This drives theater upward. Accessibility collapse (0.78) is high because the prohibition is presented as literal divine law with no alternative valid reading; once understood, alternatives appear to collapse into the single option: obey or commit heresy. Resistance (0.71) is substantial because icon-venerating communities resist the prohibition actively (hidden worship, theological counter-arguments, preservation of images in monastic archives) rather than accepting it as natural law.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and ascetic-faction seats should compute as experiencing a fundamentally different type than the victim seats. From the imperial authority's position, the prohibition is a tool of legitimate consolidation (coordinating religious practice under state authority, preventing theological chaos). From the devotional-practitioner position, it is pure extraction: their spiritual tradition is criminalized, their prayer forms are forbidden, their livelihoods or identity are destroyed. From the iconodule-theologian position (excluded), it is a snare: a false interpretation of scriptural law, suppressed and enforced to concentrate authority. The engine computes per-seat types from the structural data; this constraint's story describes the structural asymmetry (beneficiary with enforcement power vs. victims without voice) that drives the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations + exit options. Imperial authority benefits (monopolizes interpretation, collects compliance revenue in authority consolidation) and has high exit optionality (arbitrage: can switch enforcement focus if needed). Ascetic faction benefits (theological victory, doctrinal authority) and is mobile (can migrate between theological schools if needed; face no enforcement). Icon producers are victims (profession destroyed) with constrained exit (retraining required, no alternate skilled market). Monastic communities are victims (practice criminalized) with identity-locked exit (their identity is constituted through image-centered devotion; exit means self-dissolution). Devotional practitioners are victims (spiritual tradition criminalized) with trapped exit (no alternative theology available that satisfies their spiritual needs within the authorized framework). This differentiated exit structure drives differentiated d values: beneficiary seats near d=0, victim seats near d=1, reflecting how the constraint operates asymmetrically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing true worship from idolatry; preventing sensory enslavement to matter) is CONTESTED as to whether it is still live. The iconoclast faction attests it is urgent and permanent. The iconodule faction (excluded from state councils) attests it is a pedagogical risk masquerading as a structural necessity—that the Incarnation itself sanctifies matter, that image veneration can be practiced for centuries without idolatry-collapse, that the problem is intention (what you worship), not mediation (how you worship). The disappearance verdict is WORLD_REARRANGES: if the prohibition vanished, icon production would resume, devotional communities would restore their practices, and theological authority would decentralize. This mismatch—a founding problem whose solution is contested, paired with a disappearance verdict that the world depends on it—is the mandatrophy signature. The constraint persists not because the problem is solved (it is alive, generating ongoing enforcement) but because the authority structure benefits from the constraint's maintenance. When a constraint's founding problem remains live but the constraint persists anyway, and enforcement must be continuous and active, that is mandatrophy: the constraint's manifest function has outlived its founding rationale, and it now persists as a mechanism for consolidating the authority that maintains it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_authority,
    'Is the prohibition on religious imagery a claim about immutable spiritual law (that matter categorically cannot mediate the holy), or a constructed constraint that benefits identifiable beneficiaries (imperial authority consolidation, ascetic faction victory)?',
    'Historical analysis: does the prohibition arise organically from all theological voices in the tradition, or is it imposed by centralized authority against resistance from other theological communities? If imposed, against what resistance? Ethnographic/historical comparison: do image-venerating theological traditions exist without documented idolatry-collapse? (Yes: Eastern Orthodoxy, Roman Catholicism, many Orthodox traditions practice image veneration for centuries without the foundational idolatry problem the prohibition claims to prevent.) If the founding problem can be solved without the prohibition, the prohibition is not natural law.',
    'If the prohibition is natural law, it should be reclassified as Mountain (emerges_naturally valid, beneficiary declaration as false-summit candidate, no victims). If it is constructed authority, it should be reclassified as Snare or Tangled Rope: the beneficiaries benefit from enforcing extraction (authority consolidation); the victims bear costs (criminalized practice, destroyed profession, suppressed theology); the claimed naturalness is the cover story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_authority, empirical, 'Whether the constraint''s naturalness claim is grounded in irreducible spiritual fact or in beneficiary-friendly interpretation of contested doctrine.').

omega_variable(
    iconoclasm_vs_iconodulism_foreclosure,
    'Does the iconoclast reading''s core premise—that material mediation of the holy is CATEGORICALLY impermissible—logically foreclose the iconodule reading''s core premise—that the Incarnation sanctifies matter and permits mediation under proper intention?',
    'Logical analysis: can a single theological framework (a single commitment to both the Incarnation and to the Decalogue) hold BOTH the assertion that matter is sanctified by God''s embodiment AND the assertion that material mediation of the holy is categorically forbidden? If yes, the readings coexist; if no, one reading forecloses the other.',
    'If foreclosure: iconoclasm and iconodulism cannot coexist in a single coherent theology—one reading''s adoption requires the other''s rejection. This is a rare structural relation (forecloses) in kernel readings. If coexistence: the readings occupy different theological traditions/parties and are both live interpretations of the kernel—neither rules out the other, they simply disagree. This is the typical case (coexists_with).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iconoclasm_vs_iconodulism_foreclosure, conceptual, 'Whether the two readings are logically opposed or held simultaneously by different parties.').

omega_variable(
    ascetic_theology_identity_fusion,
    'Is the ascetic faction''s endorsement of the prohibition grounded in genuine theological conviction (the immaterial-worship philosophy is internally coherent and spiritually compelling), or is it grounded in institutional capture by centralizing authority (the faction endorses the prohibition because it consolidates their institutional power)?',
    'Genealogical analysis: do ascetic theological voices endorse image prohibition BEFORE state authority enforces it, or AFTER centralized power incentivizes the endorsement? Do ascetic communities benefit materially (land grants, resources, official status) from endorsing the prohibition? If endorsement precedes institutional capture and ascetics do not materially benefit, conviction is likely primary; if endorsement is recent and accompanied by state resources, institutional capture is likely primary.',
    'If conviction-primary: the ascetic faction is a genuine beneficiary whose theology aligns with the prohibition''s structure; they are not captured, merely aligned. If capture-primary: the ascetic faction is identity-locked to the constraint; their opposition to it would dissolve their institutional status and resources. This shifts their directionality from beneficiary (d near 0) toward captive-payer (d near 0.5–0.7), and the constraint becomes less rope-like (coordination with genuine theological agreement) and more snare-like (forced alignment maintained by institutional incentive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ascetic_theology_identity_fusion, empirical, 'Whether the ascetic faction''s support for the prohibition is authentic theological conviction or institutional capture.').

omega_variable(
    alternative_iconodule_readings_suppressed,
    'Are iconodule theological arguments (the Incarnation sanctifies matter, image veneration can be practiced without idolatry, distinction between latria and dulia) authentically alternative interpretations of the same scriptural tradition, or are they post-hoc rationalizations constructed to justify resistance to state authority?',
    'Textual archaeology: do iconodule arguments appear in the theological tradition BEFORE the iconoclast prohibition is enforced, or only after suppression creates resistance? If pre-prohibition, iconodulism is an original alternative interpretation; if post-prohibition, it may be reactive. Either way, the question is whether iconodulism is a coherent theological position that could have been (and should have been) included in the state councils that authored the prohibition, or whether it is a weaker counter-claim.',
    'If authentic pre-existing alternative: the absence of iconodule voices from the imperial councils is structural injustice (absent_voices, excluded stakeholders); the prohibition is not a theological consensus but an imposed orthodoxy. If post-hoc rationalization: the prohibition represents a genuine theological consensus that iconodulism has since challenged. This affects whether the constraint is a snare (one view imposed on dissenters) or a rope (genuine coordination around shared theology).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_iconodule_readings_suppressed, empirical, 'Whether iconodulism is an authentic sibling reading or a secondary response to suppression.').

omega_variable(
    structural_vs_internalized_suppression,
    'Is the measured suppression of image veneration primarily STRUCTURAL (external barriers: legal prohibition, destruction, enforcement) or INTERNALIZED (the affected communities have absorbed the prohibition''s framing as legitimate, even when external enforcement were removed)?',
    'Longitudinal evidence: in periods when enforcement is relaxed or impossible (persecution, institutional collapse, displacement to communities beyond state reach), do image-venerating communities spontaneously return to image veneration, or do they continue to suppress it? If spontaneous return occurs, suppression is primarily structural; if suppression persists despite removal of enforcement, it is partially internalized.',
    'If primarily structural: removing the prohibition would rapidly restore image veneration; the constraint''s power is external. If partially internalized: even if the prohibition were formally rescinded, some communities would need re-socialization to recover image-venerating practice; the constraint''s power has been partially internalized. This affects the estimate of how extractive the constraint actually is: internalized suppression makes the effective extraction higher (the target has incorporated the constraint''s framing) than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Whether suppression is maintained by external enforcement or partly by internalized acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(deca_tr_t5, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(deca_tr_t10, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(deca_tr_t15, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(deca_tr_t20, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement(deca_tr_t25, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 25, 0.46).
narrative_ontology:measurement(deca_tr_t30, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 30, 0.47).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(deca_be_t5, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(deca_be_t10, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement(deca_be_t15, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement(deca_be_t20, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(deca_be_t25, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement(deca_be_t30, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0, 0.71).
narrative_ontology:measurement(deca_su_t5, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 5, 0.76).
narrative_ontology:measurement(deca_su_t10, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 10, 0.81).
narrative_ontology:measurement(deca_su_t15, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 15, 0.84).
narrative_ontology:measurement(deca_su_t20, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 20, 0.86).
narrative_ontology:measurement(deca_su_t25, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 25, 0.88).
narrative_ontology:measurement(deca_su_t30, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 40, 0.89).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconoclast_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__iconoclast_reading, 0.12).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__iconodule_reading).

% DUAL FORMULATION NOTE:
% The kernel 'decalogue_image_prohibition' instantiates as two distinct constraints with opposed ε profiles and beneficiary structures. The iconoclast reading (this story) declares material representation categorically forbidden; ε=0.82 (high extraction, enforced through destruction and prosecution). The iconodule reading (sibling constraint) declares the Incarnation sanctifies matter; images are permissible when distinguished from idolatry; ε much lower (less extractive, less enforced). These are NOT the same constraint viewed from different seats—they are structurally distinct claims with different referents (what the commandment prohibits), different beneficiary structures (iconoclasm concentrates authority; iconodulism decentralizes it), and different ε values. The constraint family exists because a single natural-language label (the Decalogue's image prohibition) covers two structurally incommensurable readings. Each must be modeled separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decalogue_image_prohibition__iconoclast_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
