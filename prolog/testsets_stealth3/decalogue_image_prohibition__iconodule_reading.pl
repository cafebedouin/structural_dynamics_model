% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconodule_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Iconodule Settlement: Honor Through Images, Worship Reserved for God Alone
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   After two waves of imperial iconoclasm, the iconodule settlement became
 *   the standing arrangement of the Byzantine oikoumene: worship (latria) is
 *   reserved for God alone, honor (dulia) may pass through images to their
 *   prototypes, and the Incarnation is cited as warrant that matter can
 *   mediate the divine. The arrangement coordinates a whole visual-religious
 *   economy — certified subjects, taught intent, patriarchal oversight of
 *   depiction, workshop production, household practice — while policing its
 *   own boundary against two failure modes: adoration sliding onto the panel
 *   itself, and refusal of mediation altogether. Epsilon's referent is this
 *   standing settlement, assessed in the reading's own terms, over an
 *   interval mapped to roughly 786-846 CE (restoration under the regency
 *   through post-Triumph consolidation). The claimed type (rope) and the
 *   metrics are authored independently: the claim states the structure as I
 *   judge it; the metrics state the operation as the record shows it,
 *   enforcement edge included. KEY AGENTS (by structural relationship): See
 *   commentary.key_agents for the enumerated list; the agents named there
 *   reappear verbatim in base_properties.beneficiaries and
 *   base_properties.victims and in the structured stakeholder surface.
 *
 * KEY AGENTS:
 *   - - episcopal_hierarchy: agenda-setter and principal authority seat (institutional / identity_locked) — defines and polices the honor/worship boundary; collects deference and, after the settlement prevailed, redistributed offices
 *   - - imperial_court: co-agenda-setter (institutional / arbitrage) — supplies or withdraws the armed enforcement behind the settlement; its allegiance has flipped repeatedly across reigns
 *   - - orthodox_laity: primary beneficiary (moderate / identity_locked) — receives sanctioned material access to the divine; devotional identity is constituted through icons
 *   - - monastic_communities: dual-positioned beneficiary and historical payer (organized / identity_locked) — produces and defends icons; paid in exile, mutilation, and death when imperial policy turned
 *   - - icon_painter_workshops: beneficiary with bounded freedom (moderate / constrained) — lives from cult demand under canonical control of subject matter
 *   - - theological_defenders: analytical observers (analytical / arbitrage) — articulate the warrant from beyond enforcement reach; their writing supplied the councils' argumentative infrastructure
 *   - - recalcitrant_iconoclast_clergy: primary payer among insiders (organized / trapped) — refuse the practice and lose office, community, or both
 *   - - unsanctioned_imagery_practitioners: payer at the folk margin (powerless / constrained) — disciplined when village practice crosses the honor line
 *   - - neighboring_abrahamic_communities: excluded voices (organized / mobile) — press the idolatry charge from outside the conciliar room
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.26).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.48).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Iconodule Settlement: Honor Through Images, Worship Reserved for God Alone").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconodule_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, '59383351-df95-43c4-b2a3-4c1c34abdd72').
narrative_ontology:cs_kernel_codification('59383351-df95-43c4-b2a3-4c1c34abdd72', fixed_text).
narrative_ontology:cs_authority_grounding('59383351-df95-43c4-b2a3-4c1c34abdd72', lineage).
narrative_ontology:cs_interpretation_layer_present('59383351-df95-43c4-b2a3-4c1c34abdd72').
narrative_ontology:cs_reading_relation('59383351-df95-43c4-b2a3-4c1c34abdd72', decalogue_image_prohibition__iconoclast_reading, forecloses).
narrative_ontology:cs_reading_relation('59383351-df95-43c4-b2a3-4c1c34abdd72', decalogue_image_prohibition__moderate_iconoclast_reading, forecloses).
narrative_ontology:cs_axiom('59383351-df95-43c4-b2a3-4c1c34abdd72', foundational, incarnation_sanctifies_material_mediation).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_material_mediation, holdable).
narrative_ontology:cs_axiom_grounding('59383351-df95-43c4-b2a3-4c1c34abdd72', incarnation_sanctifies_material_mediation, theological).
narrative_ontology:cs_axiom('59383351-df95-43c4-b2a3-4c1c34abdd72', foundational, latria_dulia_distinction_is_decisive).
narrative_ontology:cs_axiom_status(latria_dulia_distinction_is_decisive, holdable).
narrative_ontology:cs_axiom_grounding('59383351-df95-43c4-b2a3-4c1c34abdd72', latria_dulia_distinction_is_decisive, deontological).
narrative_ontology:cs_axiom('59383351-df95-43c4-b2a3-4c1c34abdd72', secondary, honor_passes_to_the_prototype).
narrative_ontology:cs_axiom_status(honor_passes_to_the_prototype, holdable).
narrative_ontology:cs_axiom_grounding('59383351-df95-43c4-b2a3-4c1c34abdd72', honor_passes_to_the_prototype, theological).
narrative_ontology:cs_reference_frame('59383351-df95-43c4-b2a3-4c1c34abdd72', patristic_incarnational_consensus).
narrative_ontology:cs_drift_state('59383351-df95-43c4-b2a3-4c1c34abdd72', contemporary_devotional_practice, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('59383351-df95-43c4-b2a3-4c1c34abdd72', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, orthodox_laity).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, monastic_communities).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, icon_painter_workshops).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, episcopal_hierarchy).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconodule_reading, recalcitrant_iconoclast_clergy).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconodule_reading, unsanctioned_imagery_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconodule_reading, monastic_communities).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, incarnational_matter_sanctification).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, latria_dulia_distinction).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, seventh_ecumenical_council_definition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes councils, defines the boundary between honor (dulia) and worship (latria), certifies orthodox depiction, and disciplines excess. Collects deference and doctrinal authority, and after the settlement prevailed presided over the removal of clergy who refused the practice. Leaving the seat would mean dissolving the office itself: custodianship of this boundary constitutes the See's authority.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, episcopal_hierarchy, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconodule_reading, episcopal_hierarchy, beneficiary).

% Supplies or withdraws the armed enforcement behind the settlement: convoking and financing councils, restoring images, installing compliant patriarchs. Its allegiance has switched more than once across reigns, making its commitment the arrangement's most volatile input. Repudiating the practice outright would mean repudiating consecration rites that anchor imperial legitimacy, so outright abandonment is costly even for a willing crown.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, imperial_court, agenda_setter,
    institutional, generational, arbitrage, continental).

% Receive a sanctioned material conduit to the divine: household icons, church decoration programs, feast-day veneration; catechesis reaches the non-literate through images. Devotional identity is constituted through these objects — the home icon corner, the named festival, the kiss before the panel. Setting them aside would mean surrendering inherited practice itself, not merely one devotion among many.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, orthodox_laity, beneficiary,
    moderate, biographical, identity_locked, regional).

% Paint, house, and defend icons; icon-theology anchors monastic spirituality and the patronage economy of the houses. When imperial policy turned hostile they paid with exile, mutilation, and death — Theodore the Studite's correspondence network coordinated fidelity at exactly that price. Benefit and cost arrive through the same objects, and neither can be declined separately.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, monastic_communities, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconodule_reading, monastic_communities, payer).

% Live from commission and cult demand; the canonical prototype system guarantees a market and dictates its subject matter. Innovation outside the canon finds no buyer and invites censure; leaving the trade forfeits the craft entirely. Prosperity and confinement travel together.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, icon_painter_workshops, beneficiary,
    moderate, biographical, constrained, regional).

% Articulate the reading's warrant in writing — John of Damascus composing from Damascus beyond the emperor's reach, Theodore the Studite writing from Studion under sanction — supplying the argumentative infrastructure the councils later ratified. Their seat sees the whole structure of the dispute; extraterritoriality lets them publish what imperial editors would strike.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, theological_defenders, observer,
    analytical, generational, arbitrage, continental).

% Bishops and abbots who maintained refusal after the settlement prevailed; deposition, exile, and replacement followed. Ordination, office, and community leave no exit that preserves either conscience or station — recant and betray conviction, or refuse and lose everything built over a career. Their counter-case survives mainly in confiscated and copied papers.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, recalcitrant_iconoclast_clergy, payer,
    organized, generational, trapped, regional).

% Village and household devotees whose practice blurs honor into something closer to adoration — offerings, vows, attributions of power to the panel itself. Pastoral discipline falls on them when the boundary is crossed: public correction, and sometimes destruction of the beloved objects. The practice is woven into local custom they cannot easily relocate or renegotiate.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, unsanctioned_imagery_practitioners, payer,
    powerless, immediate, constrained, local).

% Jewish and Muslim neighbors read Christian image veneration as precisely the practice the commandment forbids, and press the charge in disputation and polemic. They stand outside the conciliar process that defined the settlement; their objection never enters the room where the boundary is drawn, though it circulates endlessly around it.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, neighboring_abrahamic_communities, excluded,
    organized, civilizational, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconodule_reading, episcopal_hierarchy).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconodule_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective problem of incarnational devotion: how embodied worshippers honor a God who took visible flesh without collapsing honor into the worship reserved for God alone. Standardizes a shared material grammar — certified subjects, sanctioned gestures, taught intent — so that non-literate believers across the oikoumene receive the same doctrine through the same image programs, and so local practice stays inside a boundary everyone can recognize.
% TRANSFER_FUNCTION: Moves deference and disciplinary obedience from laity, workshops, and folk practice upward to the episcopate; moves honor through painted matter sideways toward the depicted prototype; and, after the settlement prevailed, moved offices, sees, and legitimacy away from clergy who refused the practice toward clergy who conformed.
% ABSENT_VOICES: Jewish and Muslim neighbors press the charge that image veneration is the forbidden thing itself; they stand outside the conciliar process and their objection never enters the room where the boundary is drawn. Inside, the recalcitrant clergy after 843 speak only through confiscated and secretly copied writings; their counter-case survives on paper but holds no seat.
% DISAPPEARANCE_RATIONALE: Church decoration programs, household icon corners, feast-day veneration, painter guild economies, and the catechesis of the non-literate all organize around the sanctioned image. Overnight removal would strand devotion without its material conduit, collapse workshop economies, sever monastic identity, and force a choice between imageless piety and improvised substitutes — the oikoumene's entire visual-religious economy would reorganize.
% FOUNDING_PROBLEM: Hold two truths at once: the commandment forbids image-worship, and the Incarnation made God visible in matter, seemingly demanding visible honor. Raw iconoclasm severed incarnational logic; uncritical image-use violated the commandment. The settlement was built to keep both — forbid the worship, license the honor, and teach the difference.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: the iconoclast party's own conciliar arguments (preserved in the acts of 787) pressed the tension as real; contemporaneous Jewish and Muslim polemic attacked Christian practice on the same ground from wholly outside the polity; and historians of doctrine with no confessional stake in either seat treat the latria/dulia negotiation as a genuine problem that every generation of icon-venerating Christianity renegotiates in practice.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconodule_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon (0.26, end-state) prices what the settlement itself demands of those it governs: conformity of intent and depiction, deference to episcopal adjudication, and the discipline that falls on practice crossing the honor line — real but secondary beside the devotional good the arrangement delivers, and priced low because the reading assesses its own prohibitions as protective rather than predatory. Suppression (0.48) is authored as a raw structural quantity, unscaled by power or scope: the boundary needs standing catechetical and patriarchal machinery, and after 843 that machinery was actively applied to clergy who refused. Theater (0.15) is low because the practice is substantively functional; the rise at interval end reflects the newly instituted annual Feast of Orthodoxy, whose anathema ceremony contributes a performative layer without displacing function. Accessibility collapse is 0.30 — imageless prayer and rival readings remain reachable, and the settlement disputes rather than erases its alternatives. Resistance 0.65 records two waves of armed imperial opposition, martyrdoms, and decades in which adherence cost office, limb, or life. The three measurement series share one time grid (points every ten years across the 786-846 mapping). Epsilon and suppression_requirement oscillate with the two enforcement shocks (second iconoclasm's return around t=30, its harshest phase around t=50): the cycle is driven by imperial policy alternating, with the settlement's internal fidelity-demands ratcheting during underground phases — during persecution, the demand for costly open fidelity functioned as a commitment filter, which I note honestly rather than disguise as noise. Suppression_requirement is tracked deliberately: enforcement machinery was built, collapsed, and rebuilt across the interval, exactly the dynamic the scalar rule reserves for this series. All scalars are end-state (post-Triumph consolidation) readings.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute divergent seats from one structure: from the hierarchy's chair the arrangement is custody it exercises; from the laity's chair it is enablement their identity is fused to; from the recalcitrant clergy's chair it is coercion with no exit that saves either conscience or station; from the workshop's chair it is a market guaranteed and dictated at once. The excluded neighbor seat computes nowhere — it is a commentary-grade absence, recorded so that unanimity in the conciliar record is not mistaken for consent.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: laity, monasteries, workshops, and the episcopate sit near the beneficiary end (low d), with the episcopate highest among them because it also runs the boundary it blesses. Payers — recalcitrant clergy (trapped) and folk practitioners (constrained, powerless) — derive high d, amplified by exit-lock for the clergy seat. The court derives mid-range: it supplies enforcement and absorbs its political costs, and its demonstrated capacity to flip allegiances keeps it from locking to either pole. No directionality_overrides are used: the atoms separate the seats cleanly, and the one ambiguity — court versus episcopate sharing the institutional atom — cannot be resolved by an atom-keyed override without distorting the hierarchy's beneficiary-side position, so both are left to structural derivation. Receipt is not benefit: gain_flow names episcopal_hierarchy because the settlement's coercive gains (adjudication rents, redistribution of rival offices) demonstrably land there, while the laity's collected good is devotional rather than extracted.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and the function is not atrophied, so neither scaffold nor piton signatures apply: there is no sunset to declare, and no piton-shaped cost asymmetry — the administrator could not cheaply relax the boundary even had it wished, because army and household attachment made relaxation itself the expensive move (fixing_cost: prohibitive). Theater stays well below the drift threshold throughout the interval. The classification guards against both mislabels: against reading the settlement as a snare (its enforcement edge is real but narrow, aimed at insiders who refuse and folk practice that crosses the line — not at the coordinated population), and against reading its ritual surface as inert performance. The R5 mismatch check is consistent: founding_problem_status=live with disappearance_verdict=world_rearranges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the decalogue_image_prohibition kernel — the iconodule_reading. Which structural features would change under the sibling readings?',
    'Compare the compiled sibling stories: under decalogue_image_prohibition__iconoclast_reading the victim set expands to icon-venerators, monastic communities, and the artworks themselves, and epsilon rises sharply; under decalogue_image_prohibition__moderate_iconoclast_reading the victim set splits along medium (statuary trades pay) while flat-image practice is retained.',
    'Classification is reading-relative: the iconoclast instantiation computes as enforced extraction with identifiable victims; this instantiation computes near-rope with a narrow payer set. Cross-reading comparison must join on kernel_id, never on the colloquial label ''image prohibition''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Kernel membership and sibling structural deltas for the image-prohibition contest.').

omega_variable(
    disagreement_location_object_of_prohibition,
    'Where in the structure do the readings disagree — is the prohibition''s object the act of worship performed toward an image (latria), the existence of a material representation in worship, or the representational form?',
    'No textual or archaeological find resolves this; it is fixed by each reading''s hermeneutic of the commandment. Track which criterion each sibling''s compiled axioms encode.',
    'The criterion determines the victim set: an act-criterion yields almost no victims beyond latria-offenders; an existence-criterion yields venerators and artworks; a form-criterion yields statuary trades. Mis-specifying the location would merge distinct constraints carrying distinct epsilon values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_object_of_prohibition, conceptual, 'Definitional-gate location of the readings'' disagreement.').

omega_variable(
    post_restoration_enforcement_character,
    'Was post-843 enforcement against recalcitrant iconoclast clergy boundary-defense of a coordination settlement, or authority-consolidation by the hierarchy?',
    'Compare the scale of enforcement gains (redistributed sees and offices, numbering in the dozens) against the settlement''s coordination output; test whether enforcement terminated once rival clergy were absorbed or instead scaled with the hierarchy''s discretionary power.',
    'If enforcement tracks authority-consolidation beyond orthodoxy maintenance, the payer seat''s effective extraction amplifies and the computed type drifts toward tangled_rope; if it terminates with absorption, the rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_restoration_enforcement_character, empirical, 'Whether the settlement''s enforcement edge was guard-rail maintenance or rent collection.').

omega_variable(
    dulia_latria_borderline_population,
    'How large is the population whose practice sits irreducibly between honor and worship — large enough that the latria-prohibition operates as a recurring disciplinary surface on folk devotion rather than an occasional guard-rail?',
    'Comparative liturgical anthropology across icon-venerating regions and centuries: frequency of disciplinary intervention against folk practice, and whether interventions cluster where clerical revenue or control is at stake.',
    'A wide borderline widens the victim set and raises effective extraction on powerless local practitioners; a narrow one confirms the guard-rail reading and keeps epsilon low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dulia_latria_borderline_population, empirical, 'Size and character of the population straddling the honor/worship line.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(decalogue_iconodule_reading_tr_t0, decalogue_image_prohibition__iconodule_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_tr_t0, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_tr_t10, decalogue_image_prohibition__iconodule_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_tr_t10, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_tr_t20, decalogue_image_prohibition__iconodule_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_tr_t20, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_tr_t30, decalogue_image_prohibition__iconodule_reading, theater_ratio, 30, 0.06).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_tr_t30, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_tr_t40, decalogue_image_prohibition__iconodule_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_tr_t40, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_tr_t50, decalogue_image_prohibition__iconodule_reading, theater_ratio, 50, 0.07).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_tr_t50, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_tr_t60, decalogue_image_prohibition__iconodule_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(decalogue_iconodule_reading_be_t0, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 0, 0.27).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_be_t0, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_be_t10, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_be_t10, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_be_t20, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_be_t20, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_be_t30, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 30, 0.34).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_be_t30, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_be_t40, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_be_t40, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_be_t50, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 50, 0.33).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_be_t50, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_be_t60, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 60, 0.26).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(decalogue_iconodule_reading_su_t0, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_su_t0, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_su_t10, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_su_t10, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_su_t20, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_su_t20, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_su_t30, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_su_t30, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_su_t40, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_su_t40, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_su_t50, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 50, 0.64).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_su_t50, observed).
narrative_ontology:measurement(decalogue_iconodule_reading_su_t60, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement_basis(decalogue_iconodule_reading_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconodule_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Decalogue's image prohibition' covers three structurally distinct regimes, decomposed per the epsilon-invariance principle. The iconoclast reading compiles with a high-extraction profile — identifiable victims in destroyed artworks, suppressed venerating communities, and persecuted defenders. This iconodule reading compiles low-moderate, with a broad beneficiary set and a narrow payer set. The moderate reading splits the victim set along medium: statuary trades pay while flat-image practice rides. The iconodule act-criterion is upstream of the moderate reading's flat-image permission, which presupposes the honor/worship distinction it regulates. Family members link through affects_constraints; each file carries its own epsilon, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
