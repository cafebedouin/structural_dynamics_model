% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__trinitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__trinitarian_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: biblical_divine_nature__trinitarian_reading
 *   human_readable: Trinitarian Reading of the Divine Nature (Three Hypostases, One Ousia)
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   This story instantiates the Trinitarian reading of the biblical divine
 *   nature kernel: the claim that Father, Son, and Spirit are three
 *   hypostases sharing one ousia (essence), formalized at Nicaea (325) and
 *   Constantinople (381) via the homoousios formula. This reading emerged
 *   from a genuine coordination problem — reconciling scriptural texts naming
 *   three divine agents with inherited monotheism — but its historical
 *   operation required active imperial and ecclesial enforcement:
 *   depositions, exiles, and anathemas against subordinationist (Arian),
 *   modalist (Sabellian), and later Unitarian and Oneness Pentecostal
 *   alternatives. Extraction here is measured as the transfer of
 *   institutional legitimacy, ordination access, and communion standing away
 *   from non-Trinitarian readings toward the conciliar formula and its
 *   administering hierarchy — not as a claim about the formula's theological
 *   truth-value, which this framework does not adjudicate.
 *
 * KEY AGENTS:
 *   - nicene_ecclesial_hierarchy: agenda_setter (institutional/arbitrage) — administers and enforces the formula
 *   - arian_clergy: primary target (organized/trapped) — deposed and exiled for subordinationist Christology
 *   - unitarian_congregations: primary target (powerless/trapped) — excluded from historic communion structures
 *   - oneness_pentecostals: primary target (powerless/constrained) — excluded from modern Trinitarian denominational fellowship
 *   - comparative_theologians: analytical observer — traces the contested genealogy without confessional stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, 0.62).
domain_priors:suppression_score(biblical_divine_nature__trinitarian_reading, 0.71).
domain_priors:theater_ratio(biblical_divine_nature__trinitarian_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__trinitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__trinitarian_reading, "Trinitarian Reading of the Divine Nature (Three Hypostases, One Ousia)").
narrative_ontology:topic_domain(biblical_divine_nature__trinitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__trinitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__trinitarian_reading, '5ab01411-684b-482f-968d-7af5cc822fc7').
narrative_ontology:cs_kernel_codification('5ab01411-684b-482f-968d-7af5cc822fc7', formalized).
narrative_ontology:cs_authority_grounding('5ab01411-684b-482f-968d-7af5cc822fc7', lineage).
narrative_ontology:cs_interpretation_layer_present('5ab01411-684b-482f-968d-7af5cc822fc7').
narrative_ontology:cs_reading_relation('5ab01411-684b-482f-968d-7af5cc822fc7', biblical_divine_nature__unitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('5ab01411-684b-482f-968d-7af5cc822fc7', biblical_divine_nature__modalist_reading, forecloses).
narrative_ontology:cs_axiom('5ab01411-684b-482f-968d-7af5cc822fc7', foundational, three_hypostases_one_ousia_is_coherent_monotheism).
narrative_ontology:cs_axiom_status(three_hypostases_one_ousia_is_coherent_monotheism, holdable).
narrative_ontology:cs_axiom_grounding('5ab01411-684b-482f-968d-7af5cc822fc7', three_hypostases_one_ousia_is_coherent_monotheism, deontological).
narrative_ontology:cs_axiom('5ab01411-684b-482f-968d-7af5cc822fc7', secondary, conciliar_anathema_binds_universal_communion).
narrative_ontology:cs_axiom_status(conciliar_anathema_binds_universal_communion, holdable).
narrative_ontology:cs_axiom_grounding('5ab01411-684b-482f-968d-7af5cc822fc7', conciliar_anathema_binds_universal_communion, conventional).
narrative_ontology:cs_reference_frame('5ab01411-684b-482f-968d-7af5cc822fc7', nicene_constantinopolitan_conciliar_settlement).
narrative_ontology:cs_drift_state('5ab01411-684b-482f-968d-7af5cc822fc7', contemporary_ecumenical_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('5ab01411-684b-482f-968d-7af5cc822fc7', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__trinitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, nicene_ecclesial_hierarchy).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, conciliar_theologians).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, trinitarian_laity).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, arian_clergy).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, unitarian_congregations).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, oneness_pentecostals).
narrative_ontology:constraint_vindicates(biblical_divine_nature__trinitarian_reading, monotheistic_coherence_of_triune_godhead).
narrative_ontology:constraint_vindicates(biblical_divine_nature__trinitarian_reading, conciliar_authority_of_nicaea_constantinople).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and councils (Nicaea 325, Constantinople 381) formulate and enforce the homoousios formula as the boundary of orthodox communion. They convene synods, issue anathemas against dissenting formulations, and control ordination, so their authority is coextensive with the doctrine's enforcement. Their institutional standing and the doctrine's survival are mutually constitutive.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, nicene_ecclesial_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Cappadocian and Latin theologians develop the hypostasis/ousia distinction that resolves the coordination problem of confessing one God in three named persons scripturally attested. They gain intellectual authority, patronage, and doctrinal primacy from being the accepted articulators of the formula; their careers and legacies are built on its acceptance.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, conciliar_theologians, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__trinitarian_reading, conciliar_theologians, agenda_setter).

% Ordinary believers receive a stabilized liturgy, creed, and communal identity that resolves an otherwise fracturing theological dispute into shared worship practice. They benefit from doctrinal peace and clear catechesis but bear the cost of enforced conformity if they privately doubt the formula.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, trinitarian_laity, beneficiary,
    moderate, biographical, constrained, regional).

% Clergy holding that the Son is created and subordinate to the Father are deposed, exiled (as with Arius and his sympathizers after Nicaea), and excommunicated. Their scriptural readings emphasizing subordinationist texts are declared heretical; continued advocacy costs office, communion, and often safety under imperial enforcement of conciliar decisions.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, arian_clergy, payer,
    organized, biographical, trapped, continental).

% Communities affirming numerical divine singularity (Father alone as God) are formally anathematized, excluded from historic Trinitarian communion structures, and in various periods faced civil penalties tied to religious establishment. Their reading of monotheistic texts is treated as a departure from settled orthodoxy rather than a live alternative.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, unitarian_congregations, payer,
    powerless, generational, trapped, regional).

% Modern Oneness Pentecostal communities baptizing in Jesus' name alone and rejecting tripersonal formulation are excluded from most Trinitarian denominational fellowships and para-church bodies, denied credentialing in Trinitarian seminaries, and characterized as doctrinally unsound despite continuity of Pentecostal practice otherwise.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, oneness_pentecostals, payer,
    powerless, biographical, constrained, national).

% The convened councils function as the formal adjudicating mechanism, drawing on imperial backing (Constantine, Theodosius) to enforce conclusions empire-wide. They set the anathema formulas that subsequent hierarchy administers and that later councils reaffirm or refine.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, ecumenical_councils, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__trinitarian_reading, ecumenical_councils, observer).

% Historians of doctrine and comparative religionists trace how the homoousios formula emerged from a specific fourth-century controversy, was contested at every stage, and was stabilized as much by imperial political interest in ecclesial unity as by exegetical necessity. They document the formula's contested genealogy without holding a confessional stake in its truth.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, comparative_theologians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single formula (three hypostases, one ousia) that lets diverse congregations across a vast empire confess a coherent doctrine of God from shared scriptural texts that name Father, Son, and Spirit distinctly while affirming monotheism, avoiding the practical fragmentation of every congregation developing an idiosyncratic Christology.
% TRANSFER_FUNCTION: Moves doctrinal authority, ordination access, and communion standing from clergy and congregations holding alternative Christological readings (subordinationist, modalist, unitarian) to those affirming the conciliar formula; moves imperial and later denominational institutional recognition toward Trinitarian bodies exclusively.
% ABSENT_VOICES: Arian bishops after their depositions, Jewish and Islamic monotheistic critiques of the coherence of the formula, and modern Unitarian/Oneness communities are structurally outside the councils that define orthodoxy for them; they participated in earlier stages of the debate (Arius himself was a council participant before condemnation) but lost standing to object once the formula was ratified and enforced.
% DISAPPEARANCE_RATIONALE: If conciliar Trinitarian orthodoxy and its enforcement mechanisms vanished, the historic institutional boundary between 'orthodox' and 'heretical' Christian bodies would dissolve; denominational credentialing, ecumenical fellowship structures, and centuries of excommunication history built on this boundary would need to be renegotiated from scratch, and communities currently excluded (Unitarian, Oneness Pentecostal, historically Arian-descended groups) would have standing claims to inclusion they currently lack.
% FOUNDING_PROBLEM: Early Christian communities needed to reconcile scriptural texts that speak of Father, Son, and Spirit as distinct agents (baptismal formula, Johannine prologue, Pauline benedictions) with an inherited Jewish monotheism that forbade multiple gods, especially under pressure from competing formulations (Arian subordinationism, Sabellian modalism) that each claimed textual support.
% FOUNDING_PROBLEM_CORROBORATION: Historians of late antiquity (outside any confessional stake) corroborate that the fourth-century controversy was a genuine, unresolved textual and philosophical problem requiring some settlement mechanism. However, the SAME historians document that the specific resolution reached (homoousios) was contested by substantial portions of the church for decades after Nicaea and was enforced through imperial coercion (exile, property confiscation) rather than settled by exegetical consensus alone — corroboration for 'a problem existed' is independent of corroboration for 'this resolution was the only coherent one,' and no source outside the beneficiary hierarchy attests the latter.
narrative_ontology:disappearance_verdict(biblical_divine_nature__trinitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__trinitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__trinitarian_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_divine_nature__trinitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__trinitarian_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__trinitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__trinitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 at interval end) reflects the durable exclusion of non-Trinitarian bodies from communion, credentialing, and historically from civil standing, sustained across 1700 years even as enforcement mechanisms shifted from imperial coercion to denominational and seminary gatekeeping. Suppression peaks sharply after Nicaea (0.85 at t=60, tracking the Constantinian and Theodosian enforcement era of exile and property confiscation) then declines as state-church fusion weakens, settling near 0.35 by the modern era where exclusion is doctrinal/institutional rather than coercive-legal. Theater ratio rises modestly (0.10 to 0.28) as the anathema apparatus persists in creedal recitation and confessional boundary-marking long after the original political stakes (imperial unity) receded — a genuine but partial theatrical residue, not the dominant mode. Accessibility collapse (0.58) is moderate: alternative readings persisted underground and resurfaced repeatedly (medieval antitrinitarians, Socinians, modern Unitarian Universalism, Oneness Pentecostalism), so collapse is real but not total.
 *
 * PERSPECTIVAL GAP:
 *   From the hierarchy's seat, the formula is coordination: a real philosophical resolution to a real textual tension, defended because it is true and because false alternatives threaten communal unity. From the excluded seats, the identical structure operates as enforced boundary-maintenance: a particular resolution among several textually defensible ones was selected under imperial political pressure and then defended by anathema rather than continued argument. The engine computes both seats' types from the same structural data — the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The ecclesial hierarchy and conciliar theologians sit near the beneficiary end: they administer the formula, derive institutional and intellectual authority from its acceptance, and control the mechanisms (ordination, credentialing, communion) through which it is enforced. Trinitarian laity sit closer to symmetric — genuine doctrinal coherence and communal stability benefit them, though conformity is not optional. Arian clergy, Unitarian congregations, and Oneness Pentecostals sit at the target end: each holds a textually-grounded alternative reading that is formally anathematized, and each bears concrete costs (deposition, exclusion, denied credentialing) for maintaining it. Their trapped/constrained exit options reflect that the doctrinal boundary is enforced at the level of communion and institutional recognition, not merely private belief — leaving the tradition costs ecclesial standing itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling triadic scriptural language with monotheism) remains live in the sense that the underlying texts still require some interpretive resolution — this is not a fully dead mandate. But the specific enforcement mechanism (anathema against Arian, Unitarian, and Oneness readings) persists in denominational credentialing and fellowship exclusion long after imperial coercion ended, which is a scaffold-like transitional justification (settling a fourth-century political crisis) calcified into permanent boundary-maintenance. Classifying this as tangled_rope rather than pure snare (or pure rope) preserves both halves: real coordination function (shared worship, doctrinal coherence for the majority) coexists with asymmetric extraction (permanent exclusion of textually-grounded minority readings) — collapsing to either pole would mislabel the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trinitarian_reading_theological_vs_political_genealogy,
    'Was the homoousios formula''s victory over Arian, modalist, and unitarian alternatives determined primarily by exegetical/philosophical merit, or by imperial political interest in ecclesial unification under Constantine and Theodosius?',
    'This is not fully resolvable by historical data alone since it depends partly on theological premises about whether truth-tracking and politically-expedient processes can coincide; however comparative study of council proceedings, voting patterns under imperial pressure, and the fate of dissenting bishops (exile timing correlated with imperial favor shifts) provides partial empirical evidence.',
    'If primarily political, the constraint''s coordination framing is substantially cover for an extraction/consolidation function, pushing the classification toward snare; if primarily exegetical with enforcement as a secondary hardening mechanism, tangled_rope with a larger genuine-coordination component is the more accurate reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trinitarian_reading_theological_vs_political_genealogy, conceptual, 'Whether Nicene formula''s victory reflects theological merit or imperial political consolidation.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that the biblical_divine_nature kernel is genuinely ambiguous at the textual level (the sources support subordinationist, modalist, and tripersonal readings without unambiguous resolution), is the trinitarian_reading''s institutional dominance evidence of correct disambiguation of an underdetermined text, or evidence that institutional power selects among equally defensible readings and then retroactively naturalizes the selection?',
    'Compare textual-critical scholarship on the disputed passages (John 1:1, Philippians 2, the Comma Johanneum) across confessional and non-confessional scholarship traditions to assess whether the ambiguity is genuine or an artifact of confessional reading.',
    'If the kernel is genuinely underdetermined, no reading (including this one) can claim the natural-law-like certainty that would exempt it from the extraction analysis; this bears directly on why this constraint is authored as tangled_rope rather than as a settled, low-extraction rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether textual underdetermination at the kernel level undermines any single reading''s claim to natural resolution.').

omega_variable(
    modern_exclusion_mechanism_ambiguity,
    'Is the continued exclusion of Oneness Pentecostals and Unitarian congregations from Trinitarian denominational structures today driven by genuine doctrinal concern about coherent worship practice, or by inertial boundary-maintenance inherited from a fourth-century political settlement with no contemporary functional justification?',
    'Survey exclusion rationale given by contemporary denominational bodies and seminaries; assess whether stated grounds reference live theological concerns or cite historical conciliar authority as self-sufficient justification.',
    'If largely inertial, the modern operation of this constraint drifts toward piton-like characteristics (theater over function) even though the historical founding was substantially functional; this would support raising the theater_ratio trajectory further in future measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_exclusion_mechanism_ambiguity, empirical, 'Whether modern exclusionary enforcement is functionally motivated or inertial.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__trinitarian_reading, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__trinitarian_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bibl_tr_t60, biblical_divine_nature__trinitarian_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(bibl_tr_t300, biblical_divine_nature__trinitarian_reading, theater_ratio, 300, 0.2).
narrative_ontology:measurement(bibl_tr_t800, biblical_divine_nature__trinitarian_reading, theater_ratio, 800, 0.25).
narrative_ontology:measurement(bibl_tr_t1500, biblical_divine_nature__trinitarian_reading, theater_ratio, 1500, 0.27).
narrative_ontology:measurement(bibl_tr_t1700, biblical_divine_nature__trinitarian_reading, theater_ratio, 1700, 0.28).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__trinitarian_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bibl_be_t60, biblical_divine_nature__trinitarian_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(bibl_be_t300, biblical_divine_nature__trinitarian_reading, base_extractiveness, 300, 0.6).
narrative_ontology:measurement(bibl_be_t800, biblical_divine_nature__trinitarian_reading, base_extractiveness, 800, 0.58).
narrative_ontology:measurement(bibl_be_t1500, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1500, 0.55).
narrative_ontology:measurement(bibl_be_t1700, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1700, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__trinitarian_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(bibl_su_t60, biblical_divine_nature__trinitarian_reading, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(bibl_su_t300, biblical_divine_nature__trinitarian_reading, suppression_requirement, 300, 0.75).
narrative_ontology:measurement(bibl_su_t800, biblical_divine_nature__trinitarian_reading, suppression_requirement, 800, 0.6).
narrative_ontology:measurement(bibl_su_t1500, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1500, 0.45).
narrative_ontology:measurement(bibl_su_t1700, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1700, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__trinitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__trinitarian_reading, 0.1).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, unitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, modalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language 'biblical divine nature' / Trinity debate: trinitarian_reading (this story, tangled_rope — institutional coordination plus enforced exclusion), unitarian_reading (Father alone is God; different ε, different victim set — Trinitarians would be the excluded party under this reading's own historical instantiations), and modalist_reading (sequential modes; a minority position anathematized by the SAME councils this story's agenda_setter administers, making it both a sibling reading and, in this story's history, a victim of the enforcement apparatus). Each reading's ε reflects its own historically-specific institutional position, not a shared average.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
