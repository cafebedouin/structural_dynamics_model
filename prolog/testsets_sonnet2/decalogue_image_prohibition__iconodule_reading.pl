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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: decalogue_image_prohibition__iconodule_reading
 *   human_readable: Iconodule Reading of the Second Commandment (Dulia/Latria Distinction)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This story instantiates the iconodule reading of the decalogue
 *   image-prohibition kernel: the prohibition bars worship (latria) of images
 *   but permits honor rendered to images that passes through to their
 *   prototypes (dulia), and the Incarnation — God taking material form —
 *   sanctifies matter as a legitimate conduit for the sacred. Under this
 *   reading the constraint functions as coordination: it gives laity,
 *   artisans, and clergy a shared criterion for licit devotional image use,
 *   letting a rich visual culture of icon veneration proceed without
 *   individual believers having to adjudicate idolatry themselves. This is a
 *   distinct constraint from the iconoclast reading (which holds all
 *   religious imagery constitutes idolatry regardless of stated intent) and
 *   the moderate-iconoclast reading (which permits two-dimensional images
 *   under strict regulation but bars statuary). The three readings have
 *   structurally different beneficiary/victim sets and different epsilon
 *   values by design — the iconoclast reading of the same kernel would show
 *   high extraction and an explicit victim class (icon-venerators suppressed
 *   by enforcement); this reading, evaluated on its own terms, shows
 *   low-to-moderate extraction because the coordination function (a workable
 *   criterion for devotional practice) dominates and no party is structurally
 *   extracted from under the iconodule reading's own operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.28).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.22).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Iconodule Reading of the Second Commandment (Dulia/Latria Distinction)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theology/religious_authority/visual_culture").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, '08be374c-d438-4656-9c3a-51a9bbd07d05').
narrative_ontology:cs_kernel_codification('08be374c-d438-4656-9c3a-51a9bbd07d05', fixed_text).
narrative_ontology:cs_authority_grounding('08be374c-d438-4656-9c3a-51a9bbd07d05', lineage).
narrative_ontology:cs_interpretation_layer_present('08be374c-d438-4656-9c3a-51a9bbd07d05').
narrative_ontology:cs_reading_relation('08be374c-d438-4656-9c3a-51a9bbd07d05', decalogue_image_prohibition__iconoclast_reading, forecloses).
narrative_ontology:cs_reading_relation('08be374c-d438-4656-9c3a-51a9bbd07d05', decalogue_image_prohibition__moderate_iconoclast_reading, coexists_with).
narrative_ontology:cs_axiom('08be374c-d438-4656-9c3a-51a9bbd07d05', foundational, incarnation_sanctifies_matter_as_divine_conduit).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_matter_as_divine_conduit, holdable).
narrative_ontology:cs_axiom_grounding('08be374c-d438-4656-9c3a-51a9bbd07d05', incarnation_sanctifies_matter_as_divine_conduit, theological).
narrative_ontology:cs_axiom('08be374c-d438-4656-9c3a-51a9bbd07d05', foundational, honor_to_image_transfers_to_prototype_not_substance).
narrative_ontology:cs_axiom_status(honor_to_image_transfers_to_prototype_not_substance, holdable).
narrative_ontology:cs_axiom_grounding('08be374c-d438-4656-9c3a-51a9bbd07d05', honor_to_image_transfers_to_prototype_not_substance, theological).
narrative_ontology:cs_reference_frame('08be374c-d438-4656-9c3a-51a9bbd07d05', apostolic_aniconic_ambiguity).
narrative_ontology:cs_drift_state('08be374c-d438-4656-9c3a-51a9bbd07d05', post_nicaea_ii_ratification, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('08be374c-d438-4656-9c3a-51a9bbd07d05', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_venerating_laity).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_painters_and_workshops).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, monastic_iconodule_communities).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, ecclesiastical_hierarchy_of_icon_sanctioning_councils).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, incarnational_theology_of_matter).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, prototype_honor_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordinary believers use icons as a devotional access point to the divine — kissing, bowing before, and praying in front of images while (per this reading) directing honor to the prototype (Christ, Mary, the saint) rather than the wood and pigment itself. The distinction lets them practice embodied devotion without confessing idolatry. Exit from the practice is available in principle but would mean forfeiting a central mode of communal and personal worship they were raised inside.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, icon_venerating_laity, beneficiary,
    powerless, generational, constrained, regional).

% Craft producers whose livelihood and vocational identity depend on the theological legitimacy of religious image-making. Under this reading their work is sanctified labor rather than idol manufacture; under the sibling iconoclast reading their trade is criminalized and their products destroyed. They can in principle switch to secular craft, but the workshop tradition and guild identity is built entirely around sanctioned image production.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, icon_painters_and_workshops, beneficiary,
    moderate, biographical, mobile, regional).

% Monastic orders (historically centered on figures like John of Damascus and the defenders at the Second Council of Nicaea) articulate and transmit the latria/dulia distinction, write the theological defenses, and organize resistance to iconoclast enforcement. Their institutional and personal identity is fused with the doctrine's survival — abandoning it would mean abandoning the community's founding theological project.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, monastic_iconodule_communities, agenda_setter,
    organized, civilizational, identity_locked, continental).

% Church councils (Nicaea II, later Orthodox synods) formally ratify the distinction and set the conditions under which images are permitted — orthodox depiction, proper liturgical use, correct intent. This ratification also consolidates the hierarchy's authority to police what counts as sanctioned versus idolatrous imagery, giving it durable interpretive control over visual culture.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, ecclesiastical_hierarchy_of_icon_sanctioning_councils, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconodule_reading, ecclesiastical_hierarchy_of_icon_sanctioning_councils, beneficiary).

% Imperial and ecclesiastical factions holding the sibling readings (full iconoclasm or the two-dimensional-only compromise) are the excluded voice inside THIS reading's frame — from the iconodule seat their objection is treated as a resolved heresy, not a live theological alternative, even though historically they held state power during iconoclast periods and enforced destruction of images.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, moderate_iconoclast_and_iconoclast_authorities, excluded,
    institutional, civilizational, trapped, continental).

% Scholars of Byzantine and Orthodox theology trace how the latria/dulia distinction was constructed, contested across the eighth and ninth centuries, and eventually ratified as orthodoxy in the reading's own tradition. They can describe the reading's coherence and its contest without being bound by it.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, theological_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconodule_reading, diffuse).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconodule_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how embodied, image-using worship can coexist with a strict prohibition on idol-worship: it gives laity, artisans, and clergy a shared, teachable criterion (intent directed to prototype, not substance) that lets devotional visual culture proceed without each participant individually re-litigating whether their practice is idolatry.
% TRANSFER_FUNCTION: Moves theological legitimacy and creative/economic opportunity toward image-producing and image-venerating communities, and moves interpretive authority over what counts as 'proper' veneration toward the councils and monastic theologians who articulate and police the distinction.
% ABSENT_VOICES: The iconoclast and moderate-iconoclast factions are structurally absent from this reading's own frame — their theological objection (that any material image used devotionally collapses into latria in practice, regardless of stated intent) is treated as settled error rather than a live counter-argument, even though it held imperial enforcement power during historical iconoclast periods.
% DISAPPEARANCE_RATIONALE: If this reading's distinction vanished, sanctioned devotional visual culture (icon veneration, icon production, the liturgical use of images) would lose its doctrinal cover; laity would either be pushed toward the iconoclast reading's wholesale prohibition or toward an unsanctioned practice the hierarchy could no longer defend, and the icon-workshop economy and monastic theological tradition built around it would lose their institutional footing.
% FOUNDING_PROBLEM: Early Christian communities needed to explain how devotional use of images (already widespread in popular practice) could be reconciled with the Decalogue's explicit prohibition on graven images, especially once iconoclast emperors and councils began enforcing destruction of icons as idolatrous.
% FOUNDING_PROBLEM_CORROBORATION: Monastic iconodule writers (John of Damascus, Theodore the Studite) and the ratifying councils attest the distinction resolves a genuine problem and remains doctrinally live. Historians of the iconoclast controversy, working from outside the iconodule tradition's own self-description, corroborate that the distinction was a contested theological innovation forged under political and enforcement pressure, not a straightforward reading of the original text — supporting a 'contested' rather than 'live' or 'dead' status.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconodule_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness stays low-to-moderate (0.28 by the interval's end) because within this reading's own frame the doctrine primarily enables a coordination good — shared devotional practice, artisanal livelihood, and a stable liturgical culture — rather than transferring value from an identifiable victim class. Suppression is modest (0.22): the reading does not itself require coercive enforcement against dissenters to function (the coercive enforcement machinery belongs to the historical periods when iconoclast, not iconodule, factions held state power). Theater ratio is low and rises only slowly (0.10 to 0.18) reflecting the gradual routinization of council-sanctioned iconographic canons into somewhat formulaic liturgical production over centuries, without the function hollowing out. Accessibility collapse is moderate (0.35) — alternatives to icon veneration (aniconic worship) remain conceivable and are held by real rival factions, so the collapse is not mountain-grade.
 *
 * PERSPECTIVAL GAP:
 *   From the monastic and hierarchical agenda-setter seats, the distinction is the doctrine's stable, settled truth. From the analytical historian seat, the same distinction is a contingent, historically contested construction whose stabilization required conciliar authority and, in its era, real political contest against iconoclast state power. The engine computes these as structurally different seat experiences from the same authored data; this story does not average them into one score.
 *
 * DIRECTIONALITY LOGIC:
 *   Icon-venerating laity, painters, monastic communities, and the sanctioning hierarchy are declared beneficiaries because the reading's operation gives each a legitimate mode of practice, livelihood, or institutional authority they would lose under the sibling iconoclast reading. No victim group is declared for THIS reading because, evaluated by its own lights, no party is extracted from through its operation — the historical victims of the image controversy (destroyed icons, persecuted venerators) are victims of the iconoclast reading's ENFORCEMENT, which is a different constraint story entirely, not of this reading's coordination function. This is a deliberate structural choice consistent with the ε-referent rule: this story's ε describes the iconodule arrangement as the iconodule reading's own lights assess it, not the contest between readings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling devotional image-use with the Decalogue's prohibition) is marked contested rather than dead: the iconodule tradition treats it as permanently, doctrinally live (a settled truth to be transmitted, not a historical problem that has been solved and left behind), while outside historians read the distinction as a period-specific theological innovation whose original controversy has receded but whose institutional apparatus (councils, canons, sanctioned iconography) persists. This keeps the story from being mislabeled as pure inertial theater — the coordination function (a working criterion enabling devotional visual culture) remains actively exercised, not merely performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latria_dulia_boundary_stability,
    'Is the latria/dulia distinction a stable, principled theological line, or does it collapse in ordinary devotional practice into functionally identical behavior toward the image itself?',
    'Ethnographic and historical study of actual devotional practice (rather than doctrinal statement) to assess whether venerators'' behavior and self-reported intent track the distinction, or whether the distinction functions mainly as a post-hoc justification indistinguishable in practice from the venerated behavior the iconoclast reading condemns.',
    'If the distinction collapses in practice, this reading''s low-extraction profile becomes harder to sustain and the constraint would look more like a Tangled Rope (coordination cover for what iconoclasts would call unchanged idolatrous practice); if the distinction holds in practice, the Rope classification is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latria_dulia_boundary_stability, conceptual, 'Whether latria/dulia is a real behavioral distinction or a doctrinal gloss on unchanged practice.').

omega_variable(
    council_ratification_vs_theological_necessity,
    'Did the Second Council of Nicaea''s ratification of the iconodule position reflect a theologically necessary conclusion from the Incarnation, or a politically contingent outcome of an eighth/ninth-century imperial power struggle that could have gone the other way?',
    'Comparative historical analysis of the political conditions surrounding the councils (787, 843) versus the periods of iconoclast imperial dominance, assessing whether theological argument or political alignment was the decisive variable.',
    'If primarily political-contingent, the iconodule reading''s claim to being the ''natural'' outworking of incarnational theology is weakened, and this story''s low accessibility_collapse (0.35) should perhaps be read even lower, since the alternative reading was a live, near-equal contender rather than a marginal heresy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(council_ratification_vs_theological_necessity, empirical, 'Whether the reading''s dominance reflects theological necessity or historical-political contingency.').

omega_variable(
    kernel_framing_alternative,
    'Is the correct unit of analysis ''the second commandment as such'' (a single kernel with three readings) or should the Incarnation doctrine be treated as a separate, upstream kernel that this reading merely applies to the image question?',
    'Compare theological literatures: if Incarnation doctrine is argued and contested independently of the image controversy (which it is, e.g. in Christological disputes unrelated to icons), it may warrant its own kernel/reading structure with this constraint''s iconodule reading as a downstream application rather than a direct sibling of iconoclast/moderate-iconoclast.',
    'If Incarnation doctrine is decomposed as its own upstream kernel, this story''s cs_structure and network links would need an additional edge to an incarnation_doctrine constraint family; the current single-kernel framing (all three image readings as direct siblings) treats the Incarnation as a load-bearing axiom internal to this reading rather than an external kernel it draws on.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether Incarnation doctrine should be a separate upstream kernel rather than an axiom internal to this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__iconodule_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__iconodule_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(deca_tr_t80, decalogue_image_prohibition__iconodule_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(deca_tr_t120, decalogue_image_prohibition__iconodule_reading, theater_ratio, 120, 0.16).
narrative_ontology:measurement(deca_tr_t160, decalogue_image_prohibition__iconodule_reading, theater_ratio, 160, 0.17).
narrative_ontology:measurement(deca_tr_t200, decalogue_image_prohibition__iconodule_reading, theater_ratio, 200, 0.18).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 40, 0.2).
narrative_ontology:measurement(deca_be_t80, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 80, 0.24).
narrative_ontology:measurement(deca_be_t120, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 120, 0.26).
narrative_ontology:measurement(deca_be_t160, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 160, 0.27).
narrative_ontology:measurement(deca_be_t200, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 200, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(decalogue_image_prohibition__iconodule_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconodule_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__iconodule_reading, 0.1).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the decalogue_image_prohibition kernel (iconodule_reading, iconoclast_reading, moderate_iconoclast_reading). The iconoclast_reading and moderate_iconoclast_reading are separate constraint files, not present in this network object per the generation instructions for this batch; when authored, this file's network.affects_constraints should be updated to include their constraint_ids, since the iconodule reading's historical dominance directly shaped (and was shaped by) enforcement periods under the rival readings — an influences-type edge in the historical record, distinct from the coexists_with relation declared at the cs_structure.reading_relations level for the synchronic doctrinal contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
