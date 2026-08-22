% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconodule_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: decalogue_image_prohibition__iconodule_reading
 *   human_readable: Iconodule Image Permission (Dulia Distinction)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   The iconodule reading of the Decalogue image prohibition permits honor
 *   and veneration of religious images (dulia) directed toward their
 *   prototypes and the divine, distinct from worship (latria) reserved for
 *   God alone. The Incarnation is the theological anchor: God became matter,
 *   sanctifying material mediation as a valid conduit to the divine. This
 *   reading generated the theological framework that sustained Orthodox
 *   image-veneration practice from late antiquity through the Byzantine
 *   Iconoclasm (8th–9th centuries) and into the present. The constraint
 *   operates as Rope at the institutional level: it solves a genuine
 *   coordination problem (How can the laity venerate images without violating
 *   the commandment?) while enabling market access for icon-makers and
 *   liturgical authority for the Church. However, it requires active
 *   enforcement to suppress the rival iconoclast reading and to constrain
 *   image-veneration within orthodox boundaries (proper intent, regulated
 *   depiction). The measurement series tracks the suppression requirement
 *   declining over the interval as the iconodule position consolidated
 *   ecumenical authority and iconoclast enforcement receded, with theater
 *   rising slightly mid-interval (periods of heightened doctrinal definition)
 *   before moderating.
 *
 * KEY AGENTS:
 *   - iconodule_theologians: agenda-setter, institutional power, identity-locked to the latria/dulia distinction; their professional authority and theological project depend entirely on this framing
 *   - icon_venerators_laity: beneficiary, organized power, constrained exit; gain liturgical sanction and coherent devotional structure
 *   - icon_artists: beneficiary, moderate power, constrained exit; receive ecclesiastical patronage and market access conditioned on orthodox depiction
 *   - iconoclast_enforcers: institutional power, trapped exit; bear enforcement costs during iconoclast periods; payer in the sense of dedicating authority to suppression; excluded from the coordination framework logically
 *   - faithful_image_destroyers: powerless, trapped exit; forced to participate in destruction or witness loss of devotional objects during enforcement phases; bear emotional and spiritual costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.38).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.52).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Iconodule Image Permission (Dulia Distinction)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconodule_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, 'd80a5fe5-9687-4281-88e4-6e498d127662').
narrative_ontology:cs_kernel_codification('d80a5fe5-9687-4281-88e4-6e498d127662', fixed_text).
narrative_ontology:cs_authority_grounding('d80a5fe5-9687-4281-88e4-6e498d127662', lineage).
narrative_ontology:cs_interpretation_layer_present('d80a5fe5-9687-4281-88e4-6e498d127662').
narrative_ontology:cs_reading_relation('d80a5fe5-9687-4281-88e4-6e498d127662', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('d80a5fe5-9687-4281-88e4-6e498d127662', decalogue_image_prohibition__moderate_iconoclast_reading, influences).
narrative_ontology:cs_axiom('d80a5fe5-9687-4281-88e4-6e498d127662', foundational, latria_dulia_distinction_valid).
narrative_ontology:cs_axiom_status(latria_dulia_distinction_valid, holdable).
narrative_ontology:cs_axiom_grounding('d80a5fe5-9687-4281-88e4-6e498d127662', latria_dulia_distinction_valid, deontological).
narrative_ontology:cs_axiom('d80a5fe5-9687-4281-88e4-6e498d127662', foundational, incarnation_sanctifies_matter).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_matter, holdable).
narrative_ontology:cs_axiom_grounding('d80a5fe5-9687-4281-88e4-6e498d127662', incarnation_sanctifies_matter, deontological).
narrative_ontology:cs_reference_frame('d80a5fe5-9687-4281-88e4-6e498d127662', scriptural_prohibition_coherent_with_incarnation).
narrative_ontology:cs_drift_state('d80a5fe5-9687-4281-88e4-6e498d127662', post_iconoclasm_consolidation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d80a5fe5-9687-4281-88e4-6e498d127662', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, orthodox_icon_veneration_tradition).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, liturgical_visual_practice).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_venerators_laity).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_artists).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconodule_reading, iconoclast_enforcers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconodule_reading, faithful_image_destroyers).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, incarnational_theology).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, material_sanctification_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and defend the latria/dulia distinction: worship directed to God alone (latria), honor directed through images to their prototypes (dulia). They administer the orthodox teaching that the Incarnation sanctifies matter and establishes images as valid conduits to the divine. Their entire theological project depends on this framing; professional identity and institutional authority are fused to it.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconodule_theologians, agenda_setter,
    institutional, generational, identity_locked, regional).

% Receive permission and liturgical sanction to venerate icons as a path to the divine. The dulia distinction legitimizes their lived practice of kissing icons, bowing before them, and praying through them as intermediaries. They gain a coordinated devotional structure and theological justification against heresy accusations.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, icon_venerators_laity, beneficiary,
    organized, biographical, constrained, regional).

% Are permitted to create and sell religious images under the dulia framework. Their craft gains ecclesiastical sanction and market access through churches, monasteries, and private devotion. The constraint defines which images are licit (orthodox depiction) and which are not.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, icon_artists, beneficiary,
    moderate, biographical, constrained, regional).

% Bear the cost of active enforcement against icon veneration during periods when iconoclast doctrine prevails (e.g., the Byzantine Iconoclasm, 8th–9th centuries). They must destroy existing icons, suppress liturgical image practices, persecute icon-venerators, and maintain doctrinal boundaries. They are 'payers' in the sense that enforcement machinery and political authority are devoted to suppressing the iconodule position; 'excluded' because their position (strict iconoclasm) is logically opposed to this reading and cannot participate in its coordination framework.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconoclast_enforcers, payer,
    institutional, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconodule_reading, iconoclast_enforcers, excluded).

% In periods of iconoclast enforcement, ordinary believers are forced to participate in or witness the destruction of sacred images they venerated. They bear the emotional, spiritual, and community cost of losing devotional objects. Their exit options collapse: refusing to destroy icons risks persecution; performing the destruction violates their conscience.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, faithful_image_destroyers, payer,
    powerless, immediate, trapped, local).

% Seek a middle path between total iconoclasm and unrestricted image veneration. They observe the dispute from positions advocating gradual reform, dimensional restrictions (two-dimensional only), or heightened regulatory oversight. They do not fully belong to either the iconodule or iconoclast camps and can shift their support across theological and political cycles.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, theological_moderates, observer,
    moderate, generational, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconodule_reading, iconodule_theologians).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconodule_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified framework for visual devotion within orthodoxy: the latria/dulia distinction coordinates the laity's image-veneration practice with institutional teaching, preventing both blasphemous idolatry (prohibited) and iconoclast destruction of sacred art. Creates a shared vocabulary (distinction of worship-kinds) that allows both icon-makers, venerators, and theologians to operate within a coherent system.
% TRANSFER_FUNCTION: Transfers authority and legitimacy from ecclesiastical hierarchy to icon-venerators and artists. Moves devotional resources (time, attention, financial support for icon production) from laity toward both the icons themselves and the institutional framework that validates them. Moves artistic patronage and market access toward icon-makers whose work conforms to orthodox standards.
% ABSENT_VOICES: Strict iconoclasts (who would prohibit all images) are structurally excluded from the coordination — their position logically contradicts the dulia permission and cannot coexist within this framework. They would argue the Incarnation does not sanctify matter in the way claimed, and that any image veneration violates Scripture. Victims of forced icon destruction during iconoclast persecutions have no seat in the dulia-framework conversation, though their absence is visible in the historical record of suppressed practice.
% DISAPPEARANCE_RATIONALE: If the dulia permission and latria/dulia distinction disappeared, icon-veneration practice would cease to have institutional sanction; icon production would lose ecclesiastical market support; believers would revert to non-visual devotion or face accusation of idolatry. Theological authority would consolidate around either strict iconoclasm or unregulated image-veneration, fragmenting the unified devotional structure. Communities organized around icon-veneration would experience disruption, persecution, or schism.
% FOUNDING_PROBLEM: How can the faithful venerate and pray through images without violating the Second Commandment's prohibition of idolatry? How can matter be made sacred through the Incarnation without collapsing the distinction between worship of God and honor toward lesser beings and things?
% FOUNDING_PROBLEM_CORROBORATION: Iconodule theologians from John of Damascus onward attest the problem is constitutive to Orthodox Christian theology and remains unsolved without the dulia framework. The Second Ecumenical Council of Nicaea (787 CE) formally endorsed the latria/dulia distinction as a binding ecumenical verdict. Independent theological historians and Orthodox dogmatic tradition corroborate that image-veneration was a widespread lived practice among the laity that needed legitimization against both heresy accusations and the rival iconoclast reading.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconodule_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.38 at interval end) because the constraint operates primarily as coordination: it solves a real theological problem and enables laity participation without high coercive overhead. It begins lower (0.25 at t0) when the reading is emergent and contested, rising to 0.41 at t24 when enforcement against rival readings peaks, then moderating as ecumenical consensus solidifies and enforcement becomes less necessary. Suppression is elevated (0.52 at interval end, declining from 0.75) because active enforcement is required to suppress the iconoclast reading and maintain orthodoxy boundaries — this is not passive but institutional. Theater is low-moderate (0.22) because the dulia distinction is structurally functional (coordinates real practice) rather than performative. Accessibility_collapse is high (0.68) because once the theological distinction between latria and dulia is understood, alternatives collapse: one cannot both accept the Incarnation and reject all material mediation to the divine without internal logical tension. Resistance is high (0.71) because the iconoclast position remains a live alternative theology with institutional support in some periods, and icon-venerators resist suppression efforts.
 *
 * PERSPECTIVAL GAP:
 *   The iconodule theologian seat and the iconoclast-enforcer seat should compute as fundamentally opposed types. From the theologian's position, the constraint is genuine coordination solving a coherence problem; from the enforcer's position (when enforcement is imposed), it is an extractive constraint being defended through suppression of the rival reading. The engine computes this divergence: the theologian benefits from the framework and has identity-locked exit; the enforcer bears the cost of maintaining boundaries and can only exit by adopting the rival reading. The beneficiary-payer distinction is structural, not observer-relative.
 *
 * DIRECTIONALITY LOGIC:
 *   Iconodule theologians are near-beneficiary (d ≈ 0.15): they benefit from the institutional authority the framework grants them and have identity-locked exit. Icon-venerators and artists are moderate-beneficiary (d ≈ 0.2–0.35): they benefit from sanction and market access but remain constrained by orthodoxy regulations. Iconoclast enforcers are near-target (d ≈ 0.8) when enforcement is active: they bear the burden of suppressing the rival reading and have trapped exit. Faithful image-destroyers are full target (d ≈ 1.0): they bear the emotional cost of destruction and have no exit. The theological moderates sit near symmetric (d ≈ 0.5): they observe the dispute and could move toward either reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not exhibit mandatrophy. The founding problem (how to permit image-veneration without violating the commandment) remains live across the interval; ecumenical councils (Nicaea II, 787) endorse the solution; the problem is not obsolete or superseded. The suppression requirement declines over time not because the coordination function atrophies but because the rival iconoclast reading loses institutional power after the Iconoclasm is formally condemned (843 CE in the interval modeled). This is consolidation of the winning reading, not decay of the original function. Theater remains low, indicating the constraint retains its functional coordination role rather than becoming performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latria_dulia_distinction_epistemology,
    'Is the latria/dulia distinction (worship of God vs. honor to images) a coherent theological boundary, or is it a linguistic hedge obscuring practical idolatry?',
    'Phenomenological study of actual devotional practice: do icon-venerators functionally treat dulia as distinct from latria in their lived piety, or does the distinction collapse in practice? Theological analysis of whether the distinction is stable under pressure from both laity simplification (conflation) and strict iconoclast critique (insufficiency).',
    'If the distinction is incoherent or unstable in practice, the constraint shifts from Rope (coordination solving a real problem) toward Snare (doctrinal cover for image-veneration that violates the commandment). If stable and functional, the Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latria_dulia_distinction_epistemology, conceptual, 'Whether the foundational theological distinction of this reading is epistemically sound or covers over a logical gap').

omega_variable(
    incarnational_sanctification_scope,
    'Does the Incarnation sanctify matter broadly (all matter can mediate the divine) or narrowly (only matter depicting Christ or related to the incarnate Christ)?',
    'Theological exegesis of patristic sources, particularly John of Damascus and the councils, on whether sanctification extends universally or is restricted to specific images. Historical analysis of iconodule practice: which objects were actually venerated, and were non-representational sacred objects (relics, the cross) treated as equivalent to images?',
    'Broad interpretation supports unrestricted image-veneration and weakens the dulia boundary; narrow interpretation strengthens the constraint by limiting what counts as licit material mediation. The resolution affects which acts fall within the permission and which count as violations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incarnational_sanctification_scope, empirical, 'The theological scope of Incarnational material sanctification').

omega_variable(
    enforcement_mechanism_asymmetry,
    'During periods of iconoclast dominance, what proportion of the measured suppression reflects active persecution of icon-venerators versus passive regulatory barriers to image production?',
    'Historical accounting: during the Byzantine Iconoclasm, quantify forced destruction events, official persecutions, and executions versus regulatory prohibitions on commissioning and distribution. Separate structural barriers (no ecclesiastical patronage) from coercive suppression (punishment for disobedience).',
    'If suppression is primarily coercive, the constraint is Snare-adjacent during iconoclast periods (extraction riding on enforcement). If primarily regulatory, it remains Rope even under the rival reading''s enforcement regime. This affects whether iconoclast enforcers are payers (bearing enforcement costs) or beneficiaries (extracting through suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_asymmetry, empirical, 'Composition of suppression: coercive vs. regulatory mechanisms').

omega_variable(
    reading_identity_iconodule_versus_moderate,
    'Is the moderate_iconoclast_reading (dimensional restriction: 2D only) a coherent third reading, or a degraded form of iconodule theology that has abandoned the latria/dulia distinction?',
    'Textual analysis of moderate defenders: do they endorse the Incarnational sanctification axiom and latria/dulia distinction, or reject them? Do they appeal to different foundations (e.g., idolatry-risk mitigation) for their restrictions? Historical tracking of whether moderates shift toward iconodule or iconoclast positions under pressure.',
    'If moderate is a coherent third reading, the kernel contest is tripolar with different foreclosure patterns. If moderate is a degraded iconodule, then the binary (iconodule vs. strict iconoclast) structures the core contest. This affects the reading_relations classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_identity_iconodule_versus_moderate, conceptual, 'Whether the moderate position is a coherent third reading or a weakened form of iconodule theology').

omega_variable(
    suppression_internalization_cycles,
    'In periods of high suppression (iconoclast dominance), to what extent does suppression remain structural (external legal barriers) versus becoming internalized (believers internalize iconoclast theology and suppress their own devotional impulses)?',
    'Post-suppression trajectory analysis: when suppression ended (after 843 CE), did icon-veneration immediately resume at pre-suppression levels, or did it require re-cultivation and doctrinal revival? Textual analysis of how believers described the re-emergence of practice.',
    'High internalization suggests suppression is more effective and harder to reverse; the constraint exhibits stronger behavioral lock. Low internalization suggests the constraint''s suppressive effect is primarily structural and reversible, consistent with Rope-level coordination being temporarily overridden by rival enforcement, not replaced by capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_cycles, empirical, 'Degree of internalization of suppression during periods of rival-reading dominance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconodule_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(deca_tr_t0, projected).
narrative_ontology:measurement(deca_tr_t8, decalogue_image_prohibition__iconodule_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement_basis(deca_tr_t8, observed).
narrative_ontology:measurement(deca_tr_t16, decalogue_image_prohibition__iconodule_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement_basis(deca_tr_t16, observed).
narrative_ontology:measurement(deca_tr_t24, decalogue_image_prohibition__iconodule_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement_basis(deca_tr_t24, observed).
narrative_ontology:measurement(deca_tr_t32, decalogue_image_prohibition__iconodule_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement_basis(deca_tr_t32, observed).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__iconodule_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(deca_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(deca_be_t0, projected).
narrative_ontology:measurement(deca_be_t8, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement_basis(deca_be_t8, observed).
narrative_ontology:measurement(deca_be_t16, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement_basis(deca_be_t16, observed).
narrative_ontology:measurement(deca_be_t24, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement_basis(deca_be_t24, observed).
narrative_ontology:measurement(deca_be_t32, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 32, 0.39).
narrative_ontology:measurement_basis(deca_be_t32, observed).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(deca_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement_basis(deca_su_t0, projected).
narrative_ontology:measurement(deca_su_t8, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 8, 0.72).
narrative_ontology:measurement_basis(deca_su_t8, observed).
narrative_ontology:measurement(deca_su_t16, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(deca_su_t16, observed).
narrative_ontology:measurement(deca_su_t24, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(deca_su_t24, observed).
narrative_ontology:measurement(deca_su_t32, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 32, 0.54).
narrative_ontology:measurement_basis(deca_su_t32, observed).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(deca_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconodule_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__iconodule_reading, 0.1).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (the Decalogue image prohibition). The iconodule reading establishes a permission structure (dulia: honor through images to prototypes) grounded in Incarnational theology and the latria/dulia distinction. The iconoclast reading in the same family prohibits all religious imagery as idolatry. The moderate reading restricts three-dimensional statuary. Each reading has distinct structural properties, beneficiary sets, and ε values. They are linked by network dependency: the success or failure of the iconodule reading (e.g., councils endorsing it, enforcement against rivals) structurally affects the viability of competing readings. The iconodule constraint is authored at ε≈0.38 (Rope-level coordination with moderate enforcement); the iconoclast reading would be authored at higher ε (extraction through prohibition). Each reading is a complete constraint story with its own metrics, stakeholders, and six-questions answers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decalogue_image_prohibition__iconodule_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
