% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__trinitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Trinitarian Homoousios Doctrine (Nicene-Constantinopolitan Reading)
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   The trinitarian reading of biblical divine nature asserts that God exists
 *   as three hypostases (persons: Father, Son, Spirit) sharing one ousia
 *   (essence). This reading was formalized at Nicaea (325 CE) and
 *   Constantinople (381 CE) through the homoousios formula and the Filioque
 *   clause. The doctrine is claimed to preserve biblical monotheism while
 *   affirming Christ's full divinity and the Spirit's personality. It is
 *   enforced institutionally through creedal affirmation requirements,
 *   anathema of non-trinitarian theology, and exclusion from sacraments and
 *   ordained ministry for those who reject it. Non-trinitarian communities
 *   (Arians, later Unitarians, Oneness Pentecostals, Socinians) bear the
 *   costs of this enforcement: they are branded heretical, excluded from
 *   ecclesiastical community, and their theology is systematically suppressed
 *   in mainstream Christian education and institutional structures. This
 *   story instantiates ONE reading of the contested kernel
 *   'biblical_divine_nature'. The sibling readings are unitarian_reading
 *   (strict numerical monotheism; Son/Spirit created or subordinate) and
 *   modalist_reading (Father/Son/Spirit are sequential modes, not
 *   simultaneous persons). Each reading generates a different constraint with
 *   different beneficiary/victim structures and different institutional
 *   authority configurations. This story does NOT describe all three
 *   readings; it describes only the trinitarian reading as a clean
 *   ε-invariant constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, 0.68).
domain_priors:suppression_score(biblical_divine_nature__trinitarian_reading, 0.76).
domain_priors:theater_ratio(biblical_divine_nature__trinitarian_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__trinitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__trinitarian_reading, "Trinitarian Homoousios Doctrine (Nicene-Constantinopolitan Reading)").
narrative_ontology:topic_domain(biblical_divine_nature__trinitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__trinitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__trinitarian_reading, '6851cc0b-1534-4ae6-9b93-7940f42bcaf4').
narrative_ontology:cs_kernel_codification('6851cc0b-1534-4ae6-9b93-7940f42bcaf4', fixed_text).
narrative_ontology:cs_authority_grounding('6851cc0b-1534-4ae6-9b93-7940f42bcaf4', lineage).
narrative_ontology:cs_interpretation_layer_present('6851cc0b-1534-4ae6-9b93-7940f42bcaf4').
narrative_ontology:cs_reading_relation('6851cc0b-1534-4ae6-9b93-7940f42bcaf4', biblical_divine_nature__unitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('6851cc0b-1534-4ae6-9b93-7940f42bcaf4', biblical_divine_nature__modalist_reading, coexists_with).
narrative_ontology:cs_axiom('6851cc0b-1534-4ae6-9b93-7940f42bcaf4', foundational, three_simultaneous_hypostases).
narrative_ontology:cs_axiom_status(three_simultaneous_hypostases, holdable).
narrative_ontology:cs_axiom_grounding('6851cc0b-1534-4ae6-9b93-7940f42bcaf4', three_simultaneous_hypostases, theological).
narrative_ontology:cs_axiom('6851cc0b-1534-4ae6-9b93-7940f42bcaf4', foundational, essence_unity_homoousios).
narrative_ontology:cs_axiom_status(essence_unity_homoousios, holdable).
narrative_ontology:cs_axiom_grounding('6851cc0b-1534-4ae6-9b93-7940f42bcaf4', essence_unity_homoousios, theological).
narrative_ontology:cs_reference_frame('6851cc0b-1534-4ae6-9b93-7940f42bcaf4', apostolic_divine_revelation).
narrative_ontology:cs_drift_state('6851cc0b-1534-4ae6-9b93-7940f42bcaf4', late_modern_christianity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6851cc0b-1534-4ae6-9b93-7940f42bcaf4', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__trinitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, trinitarian_institutional_authority).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, arian_communities).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, unitarian_believers).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, modalist_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, trinitarian_believer_communities).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, early_ecumenical_councils).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, non_trinitarians_in_later_eras).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, oneness_pentecostals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ecumenical councils (Nicaea 325, Constantinople 381) and successor magisterial bodies codify the trinitarian reading as orthodox doctrine. They define the legitimate interpretation of Scripture and the identity boundaries of the Church itself. They enforce the doctrine through anathema, excommunication, and institutional exclusion. The coordination function they offer is unified ecclesial identity and doctrinal coherence; the extraction function is the power to declare who belongs and who does not, and to exclude those who do not affirm the hypostasis-ousia distinction.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, trinitarian_institutional_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Accept the trinitarian reading and thus secure full standing in the institutional Church, access to sacraments, burial rites, social legitimacy, and inclusion in the theological consensus. The reading aligns with their faith commitments; they do not experience the doctrine as imposed. Exit is possible (they could convert to unitarianism or modalism) but is costly in social terms — requires renouncing prior faith commitments and ecclesiastical standing.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, trinitarian_believer_communities, beneficiary,
    organized, civilizational, mobile, global).

% Affirm Christ's subordination to the Father and deny the homoousios (numerical identity of essence). They are branded heretical, excommunicated, barred from the sacraments and institutional Church life. Their theology is systematically erased from theological education and councils. They cannot exit without renouncing their reading of Scripture and their theological tradition; yet staying means bearing anathema status, exclusion, and the slow institutional destruction of their communities (particularly after imperial adoption of Nicene orthodoxy in the 4th century).
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, arian_communities, payer,
    moderate, civilizational, constrained, regional).

% Maintain strict numerical monotheism: the Father alone is God; Son and Spirit are created beings or functional subordinates. They are declared heretical by the trinitarian institutional authority. They bear anathema, exclusion from sacraments, and institutional marginalization. Their communities are often geographically distant (Poland, Transylvania, parts of the Middle East) and survive by separating from trinitarian institutional structures; exit means accepting trinitarian doctrine, which they experience as a violation of radical monotheism.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, unitarian_believers, payer,
    moderate, civilizational, constrained, regional).

% Teach that Father, Son, and Spirit are sequential roles or manifestations of one divine person, not three simultaneous persons. They are anathematized as heretical by trinitarian authority. Their theological interpretation is treated as incoherent (neither truly monotheist nor truly trinitarian). They experience identity fusion with their reading: to exit modalism is to renounce not just doctrine but the lived experience of God's self-revelation as they understand it. Modern Oneness Pentecostalism carries modalist commitments and faces institutional exclusion and doctrinal anathema from trinitarian denominationalism.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, modalist_communities, payer,
    moderate, civilizational, identity_locked, regional).

% Nicaea (325) and Constantinople (381) codify the doctrine through formal definition (homoousios, perichoresis) and imperial legitimation. They are the primary institutional seats where the reading becomes authoritative. Bishops affirm the creed or face deposition. The councils solve a coordination problem: unified doctrine enables a coherent Church across dispersed communities. They also consolidate institutional authority by gatekeeping what counts as legitimate biblical interpretation.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, early_ecumenical_councils, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__trinitarian_reading, early_ecumenical_councils, beneficiary).

% Jewish biblical scholars and the Jewish scriptural tradition never encounter or affirm the trinitarian doctrine. They are excluded from the Christian theological conversation that produces the constraint. Their readings of YHWH, Torah, and monotheism are orthogonal to the trinitarian-unitarian debate. They would reject all three readings (trinitarian, unitarian, modalist) as post-biblical Christian theology, not biblical interpretation. Their absence from the councils is structural: the constraint is internal to Christian tradition-formation and does not apply to Jewish interpretation.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, hebrew_scriptures_interpreters, excluded,
    analytical, civilizational, analytical, global).

% Later non-trinitarian believers (Reformation-era Socinians, colonial-period Unitarians, 19th-century liberal Protestants) face institutional marginalization from mainline Protestant and Catholic churches. They are excluded from pulpits, seminaries, and ecclesiastical offices. By the modern period, their exclusion is less violent (no longer burning at the stake) but still systematic: they are barred from ordained ministry in most Christian denominations, their churches are treated as schismatic or heretical, and their children often face social pressure to conform to trinitarian churches. The constraint persists but enforcement has shifted from anathema to institutional gatekeeping and social stigma.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, non_trinitarians_in_later_eras, payer,
    powerless, biographical, trapped, local).

% Modern Oneness Pentecostal communities (approximately 5 million believers) maintain modalist Christology and are excluded from Trinitarian ecumenical bodies (WCC, most mainline denominations). They experience systematic denunciation as heretical by trinitarian Christianity. Their churches are barred from many cooperative ministries and ecumenical forums. Exit from modalism would require renouncing not just doctrine but lived Pentecostal experience and community belonging. The constraint operates through soft institutional exclusion rather than direct anathema (which is reserved for historical authority), but its effect is similar: non-trinitarians bear costs that trinitarians do not.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, oneness_pentecostals, payer,
    powerless, biographical, identity_locked, local).

% Religious historians, comparative theologians, and ecumenical scholars examine the doctrinal contest from outside the institutional authority of any particular reading. They analyze why the trinitarian reading was institutionally victorious, what alternatives existed, and what the costs of exclusion have been for non-trinitarian communities. They do not hold a stake in the doctrine's victory; they observe how institutional power shaped doctrinal victory.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, theological_observer_seat, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unified Christian ecclesiology and doctrinal coherence: a single, authoritative reading of Scripture's witness to God's nature enables a coherent, communicable faith across dispersed communities. The homoousios formula (essence-unity despite hypostatic distinction) solves the theological problem of monotheism preservation while affirming Christ's full divinity and personhood. This coordination enables sacramental practice, liturgical unity, and common creedal confession.
% TRANSFER_FUNCTION: Authority over doctrinal boundaries and membership identity: the trinitarian institutional authority transfers ecclesial standing, sacramental access, and legitimacy to those who affirm the reading, while extracting status and community belonging from those who do not. Non-trinitarians lose ordination eligibility, sacramental validity, burial rites in the institutional Church, and social standing as legitimate Christians. The constraint moves legitimacy from the institutional authority (which defines orthodoxy) to the trinitarian believers (who are affirmed as orthodox) and away from non-trinitarians (who are anathematized).
% ABSENT_VOICES: Jewish biblical scholars and the Jewish exegetical tradition are structurally absent — the trinitarian-unitarian debate is internal to Christian theology and does not admit or consult Jewish monotheism arguments, though the constraint's justification rests partly on claims about the coherence of Jewish-inherited monotheism with trinitarian doctrine. Pre-Nicene non-trinitarian communities (like some 2nd-century modalists) cannot speak for themselves; their writings are preserved only through trinitarian refutations. Non-Christian recipients of Scripture (Muslims) are absent but would argue the constraint is a corrupt addition to biblical monotheism. Later rationalist philosophers (Enlightenment critiques of trinitarian incoherence) are not present at the councils.
% DISAPPEARANCE_RATIONALE: If the trinitarian doctrine and its institutional enforcement disappeared, Christian communities would reorganize around alternative Christological readings (modalist, unitarian, adoptionist). No ecumenical consensus would replace the homoousios; churches would fragment further or adopt locally-accepted readings. The institutional Church's power to define orthodoxy would erode, making the boundary between Christian and non-Christian fluid and contestable. Sacramental and liturgical practices would diverge (some churches retaining trinitarian formulas, others abandoning them). The modern ecumenical movement would cease to have a doctrinal center point.
% FOUNDING_PROBLEM: 4th-century Christian communities faced a theological crisis: the incarnation and apostolic writings (especially John 1:1, Colossians 1:15-17, Hebrews 1) appear to attribute full divinity to Christ; yet the inherited monotheism (Shema: 'Hear, O Israel, the Lord our God is one Lord') seems to forbid multiple divine beings. Arian theology resolved this by subordinating Christ; trinitarian theology resolved it by asserting three hypostases in one ousia. The founding problem was: how to be both monotheist and christologically orthodox without incoherence or polytheism.
% FOUNDING_PROBLEM_CORROBORATION: Trinitarian authority (the ecumenical councils, magisterial theology from Augustine to Aquinas to modern Reformed theology) attests the founding problem is live and permanently solved by the homoousios formula. Non-trinitarian communities (Arian survivors, Unitarian theologians like Faustus Socinus, Oneness Pentecostals) attest the founding problem is not solved by trinitarianism but rather obscured — trinitarianism sacrifices biblical monotheism for Christological affirmation, whereas their readings preserve both. Ecumenical scholars and historians attest the founding problem was real (genuine theological tension in the 2nd-4th century sources) but that its 'solution' was a political and institutional victory, not a logical resolution. No independent external corroboration exists beyond the theological traditions themselves; the founding problem is an artifact of competing interpretive communities, each of which claims to have resolved it.
narrative_ontology:disappearance_verdict(biblical_divine_nature__trinitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__trinitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__trinitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_divine_nature__trinitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__trinitarian_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.68) measures how much the constraint moves authority and legitimacy from non-trinitarian believers to trinitarian institutional authority. The value is high because the doctrine grants full legitimacy to one interpretive community while denying it to others; no compromise position exists. Suppression (0.76) measures how completely non-trinitarian alternatives are excluded from institutional voice and theological education. It is high because the constraint operates through institutional gatekeeping (councils, magisterium, accreditation of theology faculty), not through persuasion alone. Non-trinitarians cannot teach trinitarian seminaries, cannot be ordained in trinitarian denominations, and their writings are preserved only through trinitarian refutations. Theater (0.42) measures how much of the constraint's operation is performative maintenance (re-affirming the creed annually, liturgical trinitarian language) versus active enforcement (currently removing non-trinitarian clergy is rare; the constraint is maintained partly through liturgical repetition and assumption). The measurement series show extraction rising rapidly from t=0 to t=15 (the post-Nicene institutional consolidation period, roughly 325-750 CE), then plateauing. Suppression rises fastest over the same period (imperial enforcement machinery; anathema formalizing into political exclusion) then stabilizes. Theater rises as active enforcement becomes routine and the doctrine becomes culturally assumed. The series share one time grid so every metric is authored at every time point (interval 0-40 representing roughly 0-1700 CE in compressed form).
 *
 * PERSPECTIVAL GAP:
 *   The trinitarian institutional authority seat should compute the constraint as rope (genuine coordination with minimal overhead, no extraction). The payer seats should compute it as snare (pure extraction using institutional power, no genuine coordination benefit). The trinitarian believer seats should compute it as rope (coordination benefit they willingly accept; they do not experience anathema as imposed). The engine computes per-seat classifications and should surface this divergence: some seats see coordination, others see extraction, all from the same structural data. The claim/metric divergence is intentional and deliberate: the constraint is CLAIMED as tangled_rope (the institutional position: 'we coordinate doctrine AND some non-orthodox views are excluded') while the authored metrics describe substantially extractive operation. The engine's per-seat computation should reveal whether different power atoms and exit_options actually produce the divergence the structural data suggests.
 *
 * DIRECTIONALITY LOGIC:
 *   Trinitarian institutional authority sits at d ≈ 0.1-0.2 (full beneficiary): the constraint grants them the power to define orthodoxy, collect allegiance, control education and ordination, and exclude rivals. They experience the arrangement as genuine coordination they defend and maintain. Trinitarian believer communities sit at d ≈ 0.35-0.45 (near symmetric): they benefit from unified ecclesiology and inclusion in sacramental community, but also bear the cost of conformity (they must affirm the creed or face exclusion, though many do so willingly). Arian, unitarian, and modalist communities sit at d ≈ 0.8-0.95 (full targets): they bear all costs (exclusion, anathema, loss of community), accrue no benefits, and cannot exit without renouncing their theology. Their exit_options are constrained (identity-locked for modalists: renouncing modalism means renouncing lived faith experience) or trapped (for historical Arians and Unitarians: no alternative Christian structures existed to join, so exit meant leaving Christianity entirely or forming schismatic communities). The engine derives directionality automatically from the beneficiary/victim declarations and exit options; the directionality_logic explains why different seats hold different structural relationships to the same constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (4th-century theodicy: how to reconcile monotheism with Christological fullness) was genuine and real. The trinitarian reading was one solution among several. The constraint is claimed as tangled_rope: it solves a genuine coordination problem (unified ecclesiology) AND extracts rents (power to define orthodoxy and exclude rivals). Mandatrophy would arise if the constraint were reclassified as pure extraction (snare) once the founding problem is deemed 'solved' or 'obsolete' by non-trinitarian lights. However, trinitarians maintain the founding problem is permanently live (christological orthodoxy must be defended against subordinationism perpetually); non-trinitarians maintain it was a false problem to begin with (strict monotheism suffices). The founding_problem_status is 'contested' precisely because the two readings hold incompatible verdicts on whether the problem remains live. This blocks mandatrophy instantiation: mandatrophy requires consensus that the founding problem is dead, but consensus is absent. The constraint persists because the parties disagree on whether its justification still holds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the trinitarian doctrine a recovery of the inherent structure of biblical revelation, or is it a 4th-century institutional construction that freezes one interpretation as orthodoxy?',
    'Genealogical analysis: trace the doctrine''s emergence from pre-Nicene sources (is it continuous or discontinuous with earlier exegesis?). Comparative analysis: do non-trinitarian readings of the same scriptural passages cohere as well as trinitarian readings, or do they require additional ad-hoc moves? Hermeneutical analysis: can one derive the homoousios directly from scriptural exegesis, or does it require neo-Platonist philosophical apparatus external to Scripture?',
    'If trinitarian doctrine is a continuous development of biblical revelation (natural law of interpretation), the constraint is a rope preserving genuine coordination. If it is a 4th-century institutional imposition, the constraint is a snare using doctrine as a cover for institutional power consolidation. The finding would inform whether FSM (false summit mountain) should reclassify the doctrine as naturalized extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Whether trinitarian doctrine is a natural exegetical outcome or an institutional construction.').

omega_variable(
    suppression_mechanism_internalization,
    'Among non-trinitarian believers, how much of the measured suppression (0.76) is structural (institutional barriers: no access to seminaries, ordained ministry, ecumenical forums) versus internalized (the believers themselves internalize the ''heresy'' verdict and stop transmitting their theology)?',
    'Post-exit suppression trajectory: study communities that left trinitarian institutional structures and established independent churches (historical Unitarian congregations, Oneness Pentecostal denominations). Do they report reduced suppression after institutional exit? If suppression persists after exit, a portion is internalized. If suppression drops sharply, the mechanism is primarily structural. Interview data from non-trinitarian leaders asking whether they experience suppression as external coercion or internalized self-censorship.',
    'If suppression is primarily structural, fixing the constraint requires institutional policy change (recognizing non-trinitarian ordination, ecumenical inclusion). If suppression is partly internalized, the constraint has deeper cognitive lock-in: even after institutional exit, non-trinitarians internalize the heresy verdict. This would elevate the exit_options value from ''constrained'' to ''identity_locked'' for some believers, raising effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism in non-trinitarian communities.').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Does the trinitarian reading logically foreclose the unitarian reading and modalist reading within a single coherent framework, or do all three readings remain live positions that can coexist?',
    'Logical analysis: Do the core premises contradict such that no single framework could hold two of them? (E.g., if trinitarian hypostasis-ousia distinction is NECESSARY to preserve both monotheism and Christological fullness, does rejecting hypostasis necessarily collapse into either subordinationism or Christological denial?) Theological history: Did ecumenical councils and magisterial theology claim that alternative readings are incoherent, or merely that they are unorthodox/excluded? Do contemporary non-trinitarian theologians produce internally coherent alternatives, suggesting foreclosure is institutional, not logical?',
    'If readings logically foreclose each other, this reading''s relation to siblings is ''forecloses''. If they remain logically live (even if institutionally excluded), the relation is ''coexists_with''. The classification would inform the reading_relations axioms in cs_structure and clarify whether non-trinitarians are victims of a necessary logical structure or victims of institutional power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Whether trinitarian premise logically excludes unitarian and modalist premises.').

omega_variable(
    homoousios_referent_stability,
    'Does the homoousios (essence-unity) formula have a stable philosophical referent across the 4th-30th centuries, or has its meaning drifted with changes in metaphysical vocabulary?',
    'Lexical analysis: trace uses of homoousios and ousia from Nicaea (325) through medieval scholasticism (Thomas Aquinas), Reformation debates (Calvin, Owen), modern analytic theology (Leftow, Craig). Did the term retain its meaning or undergo substantial reinterpretation? Compare Cappadocian Fathers'' use (Gregory of Nazianzus, Basil) with medieval Thomist use with modern process theology or social trinitarian use — did the doctrine''s content stay constant while vocabulary changed, or did vocabulary changes introduce new content?',
    'If homoousios has a stable referent, the constraint''s extraction and enforcement mechanisms can be traced diachronically — enforcement of the same doctrine. If the referent drifts, later enforcement (e.g., Reformation condemnation of Unitarians) may be enforcing a different constraint than Nicene enforcement, making the measurement series (t=0 to t=40) conflate structurally distinct constraints. Theater_ratio rising would then partly reflect doctrinal drift into purely liturgical/cultural maintenance of a term whose philosophical content has emptied out.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homoousios_referent_stability, empirical, 'Semantic stability of homoousios formula across centuries.').

omega_variable(
    institutional_authority_substitutability,
    'Could the coordination function (unified ecclesiology, coherent creedal witness) be achieved without institutional authority gatekeeping access to the doctrine? Could scattered communities reach trinitarian consensus through persuasion alone, or does enforcement infrastructure (councils, anathema, ordination gatekeeping) constitute the sine qua non of coordination?',
    'Counterfactual analysis: In periods where trinitarian institutional enforcement weakened (e.g., Arian ascendancy in 4th-5th century; post-Reformation fragmentation), did trinitarian doctrine persist or collapse? Did communities with no central enforcement machinery but trinitarian conviction maintain the doctrine without magisterial enforcement? Compare denominations: do Latter-day Saints, Eastern Orthodoxy, Catholicism, and Protestantism all maintain trinitarian doctrine even though they disagree on enforcement mechanisms, suggesting the doctrine''s persistence is independent of any single enforcement structure?',
    'If trinitarian doctrine persists without enforcement, the constraint''s extraction component is separable from its coordination component: the doctrine coordinates belief; the enforcement machinery extracts institutional power. Tangled_rope classification would hold but with a weaker connection between the two components than currently modeled. If doctrine collapses without enforcement, the two are inseparable: the constraint is genuinely tangled (coordination requires extraction). The finding would inform whether alternative (less extractive) enforcement mechanisms could preserve coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_substitutability, empirical, 'Institutional enforcement necessity for trinitarian doctrinal persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__trinitarian_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trinity_measurement_tr_t0, biblical_divine_nature__trinitarian_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(trinity_measurement_tr_t5, biblical_divine_nature__trinitarian_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(trinity_measurement_tr_t10, biblical_divine_nature__trinitarian_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(trinity_measurement_tr_t15, biblical_divine_nature__trinitarian_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(trinity_measurement_tr_t20, biblical_divine_nature__trinitarian_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(trinity_measurement_tr_t25, biblical_divine_nature__trinitarian_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(trinity_measurement_tr_t30, biblical_divine_nature__trinitarian_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(trinity_measurement_tr_t35, biblical_divine_nature__trinitarian_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(trinity_measurement_tr_t40, biblical_divine_nature__trinitarian_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(trinity_measurement_be_t0, biblical_divine_nature__trinitarian_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(trinity_measurement_be_t5, biblical_divine_nature__trinitarian_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(trinity_measurement_be_t10, biblical_divine_nature__trinitarian_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(trinity_measurement_be_t15, biblical_divine_nature__trinitarian_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(trinity_measurement_be_t20, biblical_divine_nature__trinitarian_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(trinity_measurement_be_t25, biblical_divine_nature__trinitarian_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(trinity_measurement_be_t30, biblical_divine_nature__trinitarian_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(trinity_measurement_be_t35, biblical_divine_nature__trinitarian_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(trinity_measurement_be_t40, biblical_divine_nature__trinitarian_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(trinity_measurement_su_t0, biblical_divine_nature__trinitarian_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(trinity_measurement_su_t5, biblical_divine_nature__trinitarian_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(trinity_measurement_su_t10, biblical_divine_nature__trinitarian_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(trinity_measurement_su_t15, biblical_divine_nature__trinitarian_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(trinity_measurement_su_t20, biblical_divine_nature__trinitarian_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(trinity_measurement_su_t25, biblical_divine_nature__trinitarian_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(trinity_measurement_su_t30, biblical_divine_nature__trinitarian_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(trinity_measurement_su_t35, biblical_divine_nature__trinitarian_reading, suppression_requirement, 35, 0.76).
narrative_ontology:measurement(trinity_measurement_su_t40, biblical_divine_nature__trinitarian_reading, suppression_requirement, 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__trinitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__trinitarian_reading, 0.12).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, biblical_divine_nature__unitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, biblical_divine_nature__modalist_reading).

% DUAL FORMULATION NOTE:
% The constraint 'biblical_divine_nature' decomposes into three structurally distinct constraints corresponding to three readings of the same kernel: trinitarian_reading, unitarian_reading, modalist_reading. Each reading carries different epsilon values, different beneficiary/victim structures, different institutional authority configurations, and different enforcement mechanisms. All three stories are linked via network.affects_constraints. This story instantiates only the trinitarian reading; the other stories are separate JSON files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
