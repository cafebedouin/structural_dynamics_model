% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__orthodox_christological
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_john_1_1_logos__orthodox_christological, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: john_1_1_logos__orthodox_christological
 *   human_readable: Orthodox Christological Logos Doctrine (John 1:1-14)
 *   domain: theology/biblical_hermeneutics
 *
 * SUMMARY:
 *   The Orthodox Christological reading of John 1:1-14 establishes that Logos
 *   is ontologically divine, coeternal with the Father, and incarnate as
 *   Jesus Christ. This constraint was forged in the ecumenical councils
 *   (particularly Nicaea 325 and Chalcedon 451) as the binding interpretation
 *   of the biblical passage and the ground of sacramental authority,
 *   trinitarian theology, and ecclesiastical hierarchy. The reading is
 *   **claimed** as the faithful recovery of apostolic truth; however, it is
 *   **authored** here with high extractiveness and suppression metrics
 *   because institutional power is used to enforce it against rival
 *   interpretations (subordinationism, non-incarnational monotheism) that
 *   have alternative exegetical grounds. The constraint coordinates genuine
 *   theological goods (coherent Christology, unified sacramental practice)
 *   while simultaneously extracting institutional authority from the reading
 *   and suppressing alternatives. Non-trinitarian communities are active
 *   victims, bearing the costs of anathema and exclusion; subordinationist
 *   interpreters face suppression of their readings; even mainstream
 *   patristic theology is constrained by the boundary of orthodoxy. The
 *   constraint's extractiveness increased over the interval (0.31→0.72) as
 *   institutional enforcement machinery developed and theater increased
 *   (0.15→0.41) as the constraint's performance aspect grew relative to its
 *   functional content — e.g., elaborate liturgical dramatization of
 *   incarnational theology as a way to reinforce the reading's
 *   unquestionability.
 *
 * KEY AGENTS:
 *   - Orthodox episcopal hierarchy (agenda-setter, institutional power): sets and enforces the binding Logos reading through councils and canon law; derives sacramental authority from incarnational theology
 *   - Non-trinitarian communities (victims, powerless): hold alternative readings (Arian subordinationism, Unitarian monotheism); are actively suppressed through anathema and exclusion
 *   - Subordinationist interpreters (victims, moderate): historically held positions that Logos is created or subordinate; are declared heretical and excluded from legitimate discourse
 *   - Ecumenical councils (agenda-setters, institutional): codify the reading into binding creeds; enforcement machinery that determines orthodoxy boundaries
 *   - Patristic theologians (beneficiaries, moderate): develop and systematize the incarnational reading; gain authority and standing by defending orthodoxy
 *   - Lived believers (observers, powerless identity-locked): inherit the reading as faith tradition; face social/spiritual cost if they question it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__orthodox_christological, 0.72).
domain_priors:suppression_score(john_1_1_logos__orthodox_christological, 0.68).
domain_priors:theater_ratio(john_1_1_logos__orthodox_christological, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, extractiveness, 0.72).
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__orthodox_christological, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__orthodox_christological, "Orthodox Christological Logos Doctrine (John 1:1-14)").
narrative_ontology:topic_domain(john_1_1_logos__orthodox_christological, "theology/biblical_hermeneutics").

domain_priors:requires_active_enforcement(john_1_1_logos__orthodox_christological).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__orthodox_christological, '7a629d26-0bd7-4c37-bcc2-4d1fc738be0c').
narrative_ontology:cs_kernel_codification('7a629d26-0bd7-4c37-bcc2-4d1fc738be0c', fixed_text).
narrative_ontology:cs_authority_grounding('7a629d26-0bd7-4c37-bcc2-4d1fc738be0c', lineage).
narrative_ontology:cs_interpretation_layer_present('7a629d26-0bd7-4c37-bcc2-4d1fc738be0c').
narrative_ontology:cs_reading_relation('7a629d26-0bd7-4c37-bcc2-4d1fc738be0c', john_1_1_logos__subordinationist, forecloses).
narrative_ontology:cs_reading_relation('7a629d26-0bd7-4c37-bcc2-4d1fc738be0c', john_1_1_logos__non_incarnational_monotheist, coexists_with).
narrative_ontology:cs_axiom('7a629d26-0bd7-4c37-bcc2-4d1fc738be0c', foundational, logos_ontologically_divine_homoousios).
narrative_ontology:cs_axiom_status(logos_ontologically_divine_homoousios, holdable).
narrative_ontology:cs_axiom_grounding('7a629d26-0bd7-4c37-bcc2-4d1fc738be0c', logos_ontologically_divine_homoousios, deontological).
narrative_ontology:cs_axiom('7a629d26-0bd7-4c37-bcc2-4d1fc738be0c', foundational, incarnation_real_not_apparent).
narrative_ontology:cs_axiom_status(incarnation_real_not_apparent, holdable).
narrative_ontology:cs_axiom_grounding('7a629d26-0bd7-4c37-bcc2-4d1fc738be0c', incarnation_real_not_apparent, empirically_contingent).
narrative_ontology:cs_reference_frame('7a629d26-0bd7-4c37-bcc2-4d1fc738be0c', apostolic_incarnational_christology).
narrative_ontology:cs_drift_state('7a629d26-0bd7-4c37-bcc2-4d1fc738be0c', contemporary_historical_scholarship_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7a629d26-0bd7-4c37-bcc2-4d1fc738be0c', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(john_1_1_logos__orthodox_christological, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__orthodox_christological, orthodox_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(john_1_1_logos__orthodox_christological, trinitarian_sacramental_authority).
narrative_ontology:constraint_victim(john_1_1_logos__orthodox_christological, non_trinitarian_communities).
narrative_ontology:constraint_victim(john_1_1_logos__orthodox_christological, subordinationist_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(john_1_1_logos__orthodox_christological, patristic_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes the binding interpretation of John 1:1-14 through ecumenical councils, liturgical practice, and canonical discipline. Derives sacramental authority (valid eucharist, valid ordination, valid absolution) from the incarnational reading — if Logos is not God-made-flesh, the entire sacramental system loses its grounding. Guards the orthodoxy boundary through anathema, exclusion from communion, and institutional suppression of rival interpretations.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, orthodox_episcopal_hierarchy, agenda_setter,
    institutional, civilizational, identity_locked, global).

% The sacramental authority structure (priest as conduit of divine grace, eucharist as real presence of Christ, absolution as valid only through apostolic succession) rests entirely on the incarnational reading. The doctrine is the justified condition for clerical authority and institutional control over salvation goods. Without the reading, the authority has no transcendent warrant.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, trinitarian_sacramental_authority, beneficiary,
    institutional, civilizational, arbitrage, global).

% Hold alternative readings (subordinationist, non-incarnational monotheist) that deny the incarnational Logos doctrine. Are actively suppressed through institutional anathema, exclusion from communion, property confiscation in some periods and contexts, and denial of sacramental validity. Cannot exit the doctrinal constraint without leaving the faith tradition itself — the reading is embedded in core ritual and creedal identity. Face civilizational-scale pressure to conform.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, non_trinitarian_communities, payer,
    powerless, biographical, trapped, regional).

% Historically held positions (Arius, some early Syriac traditions) that Logos is a created or subordinate divine being. Are declared heretical and excluded from legitimate theological discourse within the orthodox institutional framework. Can sustain their reading only in parallel communities or heterodox traditions; within orthodoxy, exit means either conforming the reading or leaving the institutional structure.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, subordinationist_interpreters, payer,
    moderate, biographical, constrained, regional).

% Develop and systematize the incarnational reading through exegetical and philosophical work (Origen, Athanasius, Gregory of Nyssa, John of Damascus). Gain intellectual authority and institutional recognition precisely by defending and refining the doctrine. Their career and reputation depend on being recognized as authoritative within orthodoxy; dissenting would cost them institutional standing.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, patristic_theologians, beneficiary,
    moderate, generational, constrained, regional).

% Formal institutional bodies (Nicaea 325, Constantinople 381, Ephesus 431, Chalcedon 451) that codify the incarnational reading into binding creeds and exclude rival interpretations by formal anathema. The councils are the enforcement machinery: their decisions become the standard by which orthodoxy is measured and heterodoxy is condemned. They are both the expression of the constraint and the mechanism that maintains it.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, ecumenical_councils, agenda_setter,
    institutional, civilizational, analytical, global).

% Inherit the incarnational reading as part of their faith tradition (liturgy, catechesis, icons, prayer). Most do not engage in theological contestation; they live inside the reading as a taken-for-granted aspect of Christian identity. Those who question it face social isolation, spiritual disorientation, and exclusion from community participation (eucharist, confession, marriage blessing).
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, lived_believers, observer,
    powerless, biographical, identity_locked, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(john_1_1_logos__orthodox_christological, orthodox_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(john_1_1_logos__orthodox_christological, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified christological boundary: what counts as orthodox belief about Christ's nature, identity, and work. Coordinates the eucharistic liturgy, sacramental validity, clerical authority, and soteriological claims around the doctrine that God became incarnate as the Logos. Creates a singular framework within which Christian salvation, worship, and community are intelligible.
% TRANSFER_FUNCTION: Transfers interpretive authority from the text (John 1:1-14) to the institutional hierarchy. Believers surrender the right to read the passage in ways that contradict the conciliar reading in exchange for access to sacraments, spiritual authority, and institutional validation of their faith. The hierarchy collects (1) interpretive monopoly, (2) ecclesiastical authority grounded in incarnational theology, and (3) the power to determine who is orthodox and who is heretical. Non-trinitarian and subordinationist communities pay through exclusion, anathema, and loss of sacramental access.
% ABSENT_VOICES: Gnostic interpreters (who read Logos as an emanation distinct from the supreme God), Ebionite communities (who denied pre-incarnate divinity), Docetic groups (who read incarnation as apparent, not ontological), and ordinary believers without institutional training who might hold contradictory or hybrid beliefs but are socialized into silence about them. The constraint actively works to keep these readings out of legitimate theological discourse.
% DISAPPEARANCE_RATIONALE: If the orthodox incarnational Logos doctrine disappeared (replaced by subordinationist or non-incarnational reading as authoritative), the entire sacramental system would require reconstruction, clerical authority would lose its theological warrant, and Christian communities would reorganize around rival interpretations. Centuries of liturgical practice, theological tradition, and institutional hierarchy rest on this constraint being in place.
% FOUNDING_PROBLEM: John 1:1-14 uses the term 'Logos' in a way that could mean multiple things: divine wisdom (Proverbs 8), functional creative speech (Genesis 1), a distinct hypostasis, or poetic language. Early Christian theology faced the task of deciding how to read the passage in a way that was faithful to Christ's divinity, coherent with monotheism, and defensible philosophically. The constraint emerged as a resolution: Logos IS God, preexistent, incarnate.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox theology attests the problem is live: the foundational claim that Christ is God incarnate must be defended against each generation of heterodox alternatives. However, modern scholarship (Schnelle, Keener, Maloney) and comparative religious studies note that John 1:1-14 can be coherently read in subordinationist or non-incarnational ways; the problem is not textually resolved but institutionally determined. The founding problem persists as a live contestation, not a settled fact.
narrative_ontology:disappearance_verdict(john_1_1_logos__orthodox_christological, world_rearranges).
narrative_ontology:founding_problem_status(john_1_1_logos__orthodox_christological, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__orthodox_christological, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(john_1_1_logos__orthodox_christological, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__orthodox_christological, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(john_1_1_logos__orthodox_christological_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(john_1_1_logos__orthodox_christological, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(john_1_1_logos__orthodox_christological_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint transfers interpretive monopoly to the hierarchy and makes access to sacraments conditional on assent to the incarnational reading. Suppression is high (0.68) because rival readings are actively excluded through anathema, doctrinal censure, property confiscation in some periods, and institutional denial of legitimacy — these are structural coercive mechanisms, not merely disagreement. Theater is moderate (0.41) because a significant portion of enforcement is performative: the constraint's persistence relies on liturgical repetition, iconic imagery, and ritualized recitation of creeds to make the reading feel natural and inevitable. Accessibility collapse is high (0.79) because once the hierarchy is established as authoritative, alternatives collapse — believers lack the institutional resources, theological training, or community support to sustain rival readings. Resistance is moderate (0.61) because non-trinitarian communities did resist and some persist, but they are marginalized and do not challenge the dominant hierarchy's power to enforce the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the hierarchy's seat: the incarnational Logos doctrine is the faithful defense of apostolic truth against heresy; enforcement is spiritual correction of error; the constraint is a mountain (natural boundary of truth). From the non-trinitarian seat: the incarnational reading is one interpretive choice among others, bound by philosophical frameworks the hierarchy imposed; enforcement is institutional violence; the constraint is a snare (pure extraction of authority and exclusion power). From the patristic theologian's seat: the constraint is a genuine coordination mechanism that resolves a real ambiguity in John 1:1-14, but it also provides career incentives to defend it; the constraint is rope with modest extraction (tangled rope). From the believer's seat: the constraint is inherited tradition that feels natural, but questioning it brings isolation; the constraint shifts from rope (provided coherence) to snare (suppressed doubt) if the believer begins to resist. These divergences are computationally derived from the power/exit/beneficiary structure; the engine will produce different types for different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The hierarchy sits near d=0.0 (full beneficiary) — it collects interpretive authority, sacramental control, and institutional power from the reading's dominance. Non-trinitarian communities sit near d=1.0 (full targets) — they bear costs through suppression, exclusion, and anathema. Subordinationist interpreters sit around d=0.8 (high target) — they are suppressed but not completely eradicated because some traditions preserved their readings. Patristic theologians sit around d=0.2 (moderate beneficiary) — they develop the reading and gain standing, but they are also constrained by it; they must work within the boundary it establishes. Lived believers sit around d=0.5 (symmetric) — they benefit from the coordination (unified sacramental system, coherent Christian identity) but also carry the cost of potential spiritual dissonance if they harbor doubts. The measurement series track suppression intensifying (0.42→0.68) as institutional machinery hardened across the centuries, and extractiveness accumulating (0.31→0.72) as the hierarchy layered institutional benefits onto the theological reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to read John 1:1-14 in a way that honors Christ's divinity and maintains monotheistic coherence — remains **live** (not solved; the scholarly consensus includes subordinationist and non-incarnational readings as legitimate exegetical options). Yet the constraint persists with high institutional force (0.72 extractiveness, 0.68 suppression). This is a mandatrophy signal: the constraint's founding justification has not been discharged, but the constraint is maintained for institutional reasons rather than because the problem requires it. The modern scholarly consensus (documented in Schnelle, Keener, Brown) acknowledges that John 1:1-14 can be read in multiple ways; the problem is not solved by the incarnational reading, it is institutionally foreclosed against alternatives. This prevents the constraint from being classified as pure rope (where the founding problem is solved) and establishes it as tangled rope or snare: coordination goods (unified theology, sacramental practice) are genuine, but extraction (authority monopoly, victim suppression) is equally real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_textual_grounding,
    'Is the incarnational Logos reading textually inevitable from John 1:1-14, or is it a theological interpretation imposed by conciliar authority?',
    'Linguistic and exegetical analysis; comparison with other Johannine incarnational language and with non-Christian Logos texts; assessment of whether the grammar and semantics of John 1:1-14 uniquely support incarnationalism or permit plural readings.',
    'If textually grounded, the constraint''s authority is epistemic — enforcing the truth. If imposed by conciliar power, the constraint''s authority is institutional — enforcing a politically determined reading. This shifts the classification from rope (truth-maintenance) toward snare (authority-hoarding).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_textual_grounding, empirical, 'Textual ground vs. institutional imposition of incarnational reading.').

omega_variable(
    sibling_coexistence_or_foreclosure,
    'Do subordinationist and non-incarnational readings remain live theological options (held by serious Christian communities) after the constraint is established, or are they completely foreclosed?',
    'Historical study: Which Christian communities (Arian, Nestorian, Unitarian) persisted in reading John 1:1-14 differently? Are there contemporary Christian theologies that hold subordinationist or non-incarnational readings? If they exist, the readings coexist; if extinct or consigned to heresy-only status, they are foreclosed.',
    'Coexistence means the constraint functions as a boundary maintainer (I am orthodox, you are not) but doesn''t eliminate alternatives. Foreclosure means the constraint has attempted monopoly — one reading is truth, others are error to be suppressed. The classification depends on this: true coexistence points to `coexists_with` relations; foreclosure points to `forecloses`.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_coexistence_or_foreclosure, empirical, 'Whether sibling readings are coexistent or foreclosed/extinct.').

omega_variable(
    extraction_vs_coordination_ambiguity,
    'Is the constraint''s persistence due to (a) the genuine coordination benefits of a unified Christology and sacramental system, or (b) the institutional benefits the hierarchy extracts from the reading, or (c) both equally?',
    'Counterfactual: if the hierarchy had no power to extract institutional benefits from the incarnational reading, would they still enforce it? Or: if a subordinationist or non-incarnational reading provided the same coordination benefits, would the hierarchy enforce that instead? Historical analysis of when enforcement intensity correlates with theological threats vs. institutional competition.',
    'Pure coordination (a) → rope. Pure extraction (b) → snare. Both (c) → tangled rope. The proportionality determines the terminal type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_ambiguity, empirical, 'Whether the constraint persists for coordination or extraction or both.').

omega_variable(
    identity_lock_mechanism_in_suppression,
    'For non-trinitarian communities and subordinationist interpreters marked as victims: is the suppression working through external coercive mechanisms (institutional exclusion, property confiscation, legal penalty), or through internalized identity fusion (believers believe they ARE the reading, so questioning it feels like self-dissolution)?',
    'Post-exit trajectory: among those who leave orthodox Christianity for non-trinitarian communities, do they report that suppression was external (institutional barriers) or internalized (they had internalized the reading as core identity)? Do ex-members report decompression or continued identity dissonance?',
    'External suppression is a structural constraint the binding forces the interpreter into. Internalized suppression persists after exit and is more deeply extractive. If both, the constraint''s effective suppression is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_suppression, empirical, 'Whether suppression is structural/external or internalized/identity-fused.').

omega_variable(
    kernel_codification_location,
    'Is the Logos kernel''s primary codification the text (John 1:1-14 itself) or the conciliar creeds (Nicene, Constantinopolitan)?',
    'Authority structure analysis: When the hierarchy cites the incarnational doctrine, do they cite John 1:1-14 or the creeds? When believers recite the Logos doctrine, is it creedal language or biblical language? Which source is treated as having final authority in dispute?',
    'If textual: the authority depends on successful exegesis, and rival exegetical readings are threats. If creedal: the authority is institutional (councils have power), and textual exegesis is secondary. This shifts understanding of the constraint from epistemic (truth-recovery) to institutional (authority-maintenance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_codification_location, conceptual, 'Whether the kernel is John 1:1-14 or the conciliar creeds.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__orthodox_christological, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(john_tr_t0, john_1_1_logos__orthodox_christological, theater_ratio, 0, 0.15).
narrative_ontology:measurement(john_tr_t325, john_1_1_logos__orthodox_christological, theater_ratio, 325, 0.22).
narrative_ontology:measurement(john_tr_t431, john_1_1_logos__orthodox_christological, theater_ratio, 431, 0.28).
narrative_ontology:measurement(john_tr_t800, john_1_1_logos__orthodox_christological, theater_ratio, 800, 0.35).
narrative_ontology:measurement(john_tr_t1200, john_1_1_logos__orthodox_christological, theater_ratio, 1200, 0.38).
narrative_ontology:measurement(john_tr_t1700, john_1_1_logos__orthodox_christological, theater_ratio, 1700, 0.41).

% Extraction over time
narrative_ontology:measurement(john_be_t0, john_1_1_logos__orthodox_christological, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(john_be_t325, john_1_1_logos__orthodox_christological, base_extractiveness, 325, 0.52).
narrative_ontology:measurement(john_be_t431, john_1_1_logos__orthodox_christological, base_extractiveness, 431, 0.61).
narrative_ontology:measurement(john_be_t800, john_1_1_logos__orthodox_christological, base_extractiveness, 800, 0.64).
narrative_ontology:measurement(john_be_t1200, john_1_1_logos__orthodox_christological, base_extractiveness, 1200, 0.68).
narrative_ontology:measurement(john_be_t1700, john_1_1_logos__orthodox_christological, base_extractiveness, 1700, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(john_su_t0, john_1_1_logos__orthodox_christological, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(john_su_t325, john_1_1_logos__orthodox_christological, suppression_requirement, 325, 0.58).
narrative_ontology:measurement(john_su_t431, john_1_1_logos__orthodox_christological, suppression_requirement, 431, 0.62).
narrative_ontology:measurement(john_su_t800, john_1_1_logos__orthodox_christological, suppression_requirement, 800, 0.65).
narrative_ontology:measurement(john_su_t1200, john_1_1_logos__orthodox_christological, suppression_requirement, 1200, 0.67).
narrative_ontology:measurement(john_su_t1700, john_1_1_logos__orthodox_christological, suppression_requirement, 1700, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__orthodox_christological, identity_coordination).
narrative_ontology:boltzmann_floor_override(john_1_1_logos__orthodox_christological, 0.12).
narrative_ontology:affects_constraint(john_1_1_logos__orthodox_christological, john_1_1_logos__subordinationist).
narrative_ontology:affects_constraint(john_1_1_logos__orthodox_christological, john_1_1_logos__non_incarnational_monotheist).
narrative_ontology:affects_constraint(john_1_1_logos__orthodox_christological, sacramental_authority_structure).
narrative_ontology:affects_constraint(john_1_1_logos__orthodox_christological, apostolic_succession_doctrine).

% DUAL FORMULATION NOTE:
% The John 1:1 Logos kernel admits three structurally distinct readings: Orthodox Christological (this constraint), Subordinationist, and Non-incarnational Monotheist. Each reading emits different constraints because they make different ε claims about what the passage means, different beneficiary/victim structures (who benefits from enforcing the reading), and different extraction costs (how heavily institutional authority rides on the reading). The three stories form a constraint family linked via network.affects_constraints. The Orthodox reading is upstream in legitimacy authority but arguably downstream in the empirical vulnerability of its ε claim (more contested, more requires institutional suppression to maintain). The Non-incarnational reading is the empirical competitor (most readily supported by scholarly consensus). The Subordinationist reading is historically the first near-miss before orthodoxy solidified at Nicaea.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
