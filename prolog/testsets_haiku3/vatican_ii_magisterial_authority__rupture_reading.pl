% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__rupture_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II Magisterial Authority (Rupture Reading)
 *   domain: ecclesiology/institutional/hermeneutical
 *
 * SUMMARY:
 *   Vatican II (1962–1965) is read in this constraint as encoding a
 *   fundamental rupture with pre-conciliar Catholic teaching. Under the
 *   rupture reading, conciliar texts authorize new ecclesiology (People of
 *   God, subsidiarity, religious freedom, ecumenical openness) that
 *   explicitly supersedes prior positions (error has no rights, liturgical
 *   uniformity, exclusivist claims). The constraint is the institutional
 *   authority structure enforcing this rupture reading as THE official
 *   reading of Vatican II. The rupture reading benefits the post-conciliar
 *   theological establishment and papal magisterium by legitimizing radical
 *   implementation as faithful conciliar development; it extracts costs from
 *   pre-conciliar institutional defenders and traditional communities whose
 *   praxis is suppressed as aberrant. The constraint is claimed as
 *   tangled_rope because it simultaneously coordinates around new conciliar
 *   texts AND asymmetrically extracts from those defending pre-conciliar
 *   teaching. KEY AGENTS: post_conciliar_theological_establishment
 *   (agenda_setter, benefits from interpretive authority);
 *   pre_conciliar_institutional_defenders (payer, constrained by
 *   institutional consensus); traditional_latin_mass_communities (payer,
 *   identity_locked, suppressed); continuity_reading_advocates (excluded,
 *   systematically marginalized); papal_magisterium (agenda_setter, benefits
 *   from directive authority).
 *
 * KEY AGENTS:
 *   - post_conciliar_theological_establishment: Institutional beneficiary, sets interpretive standards through seminaries and theology faculties
 *   - papal_magisterium_post_paul_vi: Agenda-setter, legitimizes conciliar interpretation and directs implementation
 *   - pre_conciliar_institutional_defenders: Powerful but suppressed payers, constrained by institutional consensus
 *   - traditional_latin_mass_communities: Identity-locked payers, liturgically and doctrinally suppressed
 *   - continuity_reading_advocates: Excluded powerful actors, marginalized from curriculum and appointment authority
 *   - vatican_curial_bureaucracy: Agenda-setter, enforces rupture reading through regulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.68).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II Magisterial Authority (Rupture Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "ecclesiology/institutional/hermeneutical").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, '279f0170-31a3-4954-9840-a5132b786e64').
narrative_ontology:cs_kernel_codification('279f0170-31a3-4954-9840-a5132b786e64', fixed_text).
narrative_ontology:cs_authority_grounding('279f0170-31a3-4954-9840-a5132b786e64', lineage).
narrative_ontology:cs_interpretation_layer_present('279f0170-31a3-4954-9840-a5132b786e64').
narrative_ontology:cs_reading_relation('279f0170-31a3-4954-9840-a5132b786e64', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('279f0170-31a3-4954-9840-a5132b786e64', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('279f0170-31a3-4954-9840-a5132b786e64', foundational, vatican_ii_encodes_new_doctrinal_content).
narrative_ontology:cs_axiom_status(vatican_ii_encodes_new_doctrinal_content, holdable).
narrative_ontology:cs_axiom_grounding('279f0170-31a3-4954-9840-a5132b786e64', vatican_ii_encodes_new_doctrinal_content, deontological).
narrative_ontology:cs_axiom('279f0170-31a3-4954-9840-a5132b786e64', foundational, pre_conciliar_positions_explicitly_superseded).
narrative_ontology:cs_axiom_status(pre_conciliar_positions_explicitly_superseded, holdable).
narrative_ontology:cs_axiom_grounding('279f0170-31a3-4954-9840-a5132b786e64', pre_conciliar_positions_explicitly_superseded, empirically_contingent).
narrative_ontology:cs_reference_frame('279f0170-31a3-4954-9840-a5132b786e64', rupture_with_pre_conciliar_magisterium).
narrative_ontology:cs_drift_state('279f0170-31a3-4954-9840-a5132b786e64', contemporary_papal_hermeneutic_ambiguity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('279f0170-31a3-4954-9840-a5132b786e64', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, reformed_institutional_catholicism).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_theological_establishment).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_institutional_defenders).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditional_latin_mass_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, papal_magisterium_post_paul_vi).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, lay_catholics_no_latin_attachment).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, vatican_curial_bureaucracy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, lay_catholics_no_latin_attachment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Vatican II conciliar texts as authorizing fundamental break from pre-conciliar ecclesiology. Controls seminaries, academic theologicals institutions, and the institutional reading of the magisterium. Enforces the rupture reading through curriculum, tenure decisions, and appointment authority. Benefits from the institutional legitimacy conferred by framing conciliar innovation as authentic development rather than reversal.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_theological_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Clergy, bishops, and institutional actors who affirm the pre-conciliar magisterium as binding and consistent with Vatican II. They are compelled to either accept the rupture reading or justify continuity arguments against the institutional consensus. Their institutional positions depend on navigating or conforming to the post-conciliar reading; resistance invites marginalization.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_institutional_defenders, payer,
    powerful, generational, constrained, global).

% Reject the rupture reading and maintain pre-conciliar liturgical and doctrinal practice. They are institutionally suppressed: the mass they practice is declared an aberration, their parishes are restricted, their seminaries are defunded, their bishops are marginalized. Exit from this identity means abandoning the entire theological and liturgical framework that constitutes their Catholicism.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditional_latin_mass_communities, payer,
    moderate, biographical, identity_locked, local).

% The conciliar documents (Sacrosanctum Concilium, Unitatis Redintegratio, Dignitatis Humanae, Lumen Gentium) are vindicated as containing genuinely new doctrinal content requiring implementation and development. The rupture reading treats the texts as a kernel of new authority that supersedes prior teaching.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_text_itself, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_text_itself).

% Post-conciliar popes (Paul VI, John Paul II, Benedict XVI, Francis) gain interpretive authority to direct conciliar implementation and development. The rupture reading legitimizes their authority to authorize radical liturgical and doctrinal change (altar relocation, vernacular mass, ecumenical engagement, religious freedom doctrine) as faithful conciliar interpretation rather than as breaks from prior magisterium.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, papal_magisterium_post_paul_vi, beneficiary,
    institutional, generational, arbitrage, global).

% Theologians, bishops, and institutional actors who argue that Vatican II is organic development within unbroken tradition. They are systematically excluded from curriculum design, academic hiring, and episcopal appointment. Their reading, though articulated within Catholic institutional structures, is suppressed by interpretive enforcement: the rupture reading is treated as settled while continuity arguments are marginalized as reactionary.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, continuity_reading_advocates, excluded,
    powerful, generational, constrained, global).

% Scholars (sedevacantists, SSPX sympathizers, postconciliar restoration advocates) who read Vatican II as an unresolved compromise encoding incompatible visions. They are institutionally excluded: their analysis is not permitted in mainstream theological discourse, their ordinations are not recognized, their communities are juridically suppressed.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, composite_overdetermination_advocates, excluded,
    moderate, biographical, constrained, regional).

% Benefit from vernacular mass, lay participation, and ecumenical openness. They are the constituency the rupture reading was written for. They also pay a cost: the liturgical and doctrinal instability created by accelerated implementation and the loss of continuity with pre-conciliar practice creates catechetical confusion and doctrinal disorientation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, lay_catholics_no_latin_attachment, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, lay_catholics_no_latin_attachment, payer).

% Dicasteries and Vatican offices enforce the rupture reading through regulation of liturgical experimentation, doctrinal development, and theological education. They benefit from expanded interpretive authority: conciliar innovation requires continuous guidance and regulation, which sustains bureaucratic power and relevance.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, vatican_curial_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, vatican_curial_bureaucracy, beneficiary).

% The documented pre-conciliar magisterium (papal encyclicals, council decisions, theological consensus from 1870–1962) forms the reference point for assessing whether Vatican II represents rupture or continuity. The rupture reading asserts this record is superseded; the continuity reading asserts no break is present; the overdetermination reading asserts the texts are ambiguous and both readings are extractable.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, historical_record_pre_conciliar_teaching, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_magisterial_authority__rupture_reading, historical_record_pre_conciliar_teaching).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_theological_establishment).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes Vatican II conciliar texts as binding magisterial authority that coordinates post-conciliar Catholicism around a renewed ecclesiology: People of God (subsidiarity, lay participation), religious freedom (human right, not merely tolerated), ecumenical engagement (separation is sin, reunion is goal), vernacular liturgy (accessibility, not Latin exclusivity). Solves the coordination problem of how to preserve Catholic institutional continuity while authorizing radical pastoral and doctrinal innovation.
% TRANSFER_FUNCTION: Transfers doctrinal authority from the pre-conciliar magisterial consensus to the conciliar texts as interpreted by post-conciliar papal and theological leadership. Transfers institutional legitimacy from hierarchical exclusivism to inclusive People-of-God ecclesiology. Transfers liturgical authority from the Latin mass to the vernacular novus ordo. Moves teaching authority on religious freedom from error-has-no-rights (Syllabus) to religious freedom as a human right (Dignitatis Humanae). Moves from those defending pre-conciliar positions to the post-conciliar establishment that directs implementation.
% ABSENT_VOICES: Sedevacantist communities that deny Vatican II's ecumenical legitimacy are juridically excluded. Bishops and theologians defending the continuity reading are present in the Church but systematically marginalized from curriculum, publishing, and episcopal appointment authority. The SSPX and traditionalist communities that reject the rupture reading are institutionally suppressed and their sacraments declared illicit (until recent gestures toward normalization). The voices that would argue the founding problem was misdiagnosed are excluded from authoritative theological discourse.
% DISAPPEARANCE_RATIONALE: If the rupture reading vanished overnight and the Church reverted to the continuity reading (Vatican II represents organic development within unbroken tradition), the entire post-conciliar institutional apparatus — seminaries organized around post-conciliar theology, parishes structured for vernacular mass and lay participation, episcopal appointment criteria favoring progressive theology — would lose their justification and would require radical reorganization. The papal magisterium's authority to declare doctrinal progress (e.g., John Paul II on contraception, Francis on divorce and remarriage) would be severely constrained. The institutional legitimacy of the post-conciliar Church itself (its identity as 'renewed') would evaporate. Traditionalist communities would be vindicated and would demand restoration of pre-conciliar practices. The world absolutely rearranges because the constraint constitutes the operational identity of the post-conciliar institutional Church.
% FOUNDING_PROBLEM: The pre-conciliar Church was perceived by the reforming majority at Vatican II (and by subsequent papal interpretation) as excessively hierarchical, institutionally rigid, doctrinally defensive against modernity, pastorally detached from the laity, and unnecessarily exclusivist in its claims against non-Catholic Christianity and religious traditions. The founding problem is that pre-conciliar ecclesiology was inadequate to the modern historical moment and required fundamental institutional and doctrinal renewal.
% FOUNDING_PROBLEM_CORROBORATION: The papal opening statement of Vatican II (John XXIII's Gaudet Mater Ecclesia, 1962: the Church must respond to the signs of the times) attests the founding problem as real and pressing. The post-conciliar theological establishment and Vatican bureaucracy attest the problem as solved by conciliar implementation. The papal magisterium post-Paul VI attests conciliar renewal as necessary and justified. HOWEVER: Continuity advocates (Aidan Nichols, Matthew Levering, later Ratzinger) dispute that the problem required rupture; they attest the founding problem was either misdiagnosed (the pre-conciliar Church was not failing) or could have been solved through organic development without suppressing pre-conciliar teaching. Sedevacantists attest the founding problem was invented to justify heresy. The consensus on the reality and severity of the founding problem is split along the same lines as the three readings: the rupture reading treats it as genuine and solved; the continuity reading treats it as exaggerated and organically solvable; the overdetermination reading treats it as unresolved by the texts themselves. No corroboration exists outside the beneficiary seats that does not reflect the beneficiary's reading.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__rupture_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the rupture reading persists by continuously suppressing alternatives and requiring conformity to its magisterial claim. The measurement series shows extraction rising sharply from 1962 to 1994 (0.35→0.61) as the post-conciliar consensus hardened, then moderating slightly post-2010 as Benedict XVI and Francis acknowledged 'hermeneutic of continuity' language while maintaining rupture enforcement. Theater rises from 0.15 to 0.60 (then slightly recedes to 0.58): the early conciliar period was genuine institutional reform; by 1994 much enforcement activity was defending the rupture-reading settlement rather than advancing coordination (defending liturgical restrictions, suppressing traditionalist communities, excluding continuity scholars from publishing and appointments). Suppression requirement tracks extraction closely but peaks at 0.74 in 2019, reflecting the intensity required to maintain the reading against mounting challenges. The slight decline to 0.72 by 2024 reflects the ambiguity introduced by Francis's inconsistent signals on the Latin mass and the rise of traditionalist institutional recovery, which has fractured the post-conciliar consensus without yet displacing the rupture reading.
 *
 * PERSPECTIVAL GAP:
 *   From the post-conciliar establishment's seat, the rupture reading appears as necessary institutional renewal and authentic conciliar interpretation. From the pre-conciliar institutional defender's seat, the same structure appears as a coup d'état disguised as development, the conciliar texts as ambiguous compromise captured by the progressive minority. From the sedevacantist seat, both are false: Vatican II itself is illegitimate. From the continuity advocate's seat (occupied by powerful theologians and bishops), the rupture reading is a hermeneutical choice, not a discovered fact — yet that seat is systematically excluded from governance while being held as formally in communion. The engine should compute this divergence per seat: the agenda-setter and beneficiary seats compute to tangled_rope or snare (coordination + extraction); the payer and excluded seats should compute higher extraction and lower coordination; the sedevacantist observer seat computes the whole structure as illegitimate. The authored metrics describe the constraint FROM THE RUPTURE READING'S OWN SELF-DESCRIPTION as coordination plus enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   The post-conciliar theological establishment and papal magisterium are the structural beneficiaries (d→0.2): they direct interpretation, control institutional appointments, and collect interpretive authority. Pre-conciliar defenders are targets (d→0.85): they must either conform or accept marginalization. Traditional communities are doubly constrained: they are both victims (suppression) and identity-locked (their Catholicism is constituted by pre-conciliar praxis), making exit equivalent to spiritual death from their own perspective. Continuity advocates occupy an ambiguous position: they are excluded from agenda-setting but present in the Church, constrained but not wholly suppressed. The rupture reading's strength is that it aligns the papacy, the theological establishment, and lay progressive constituencies around a single interpretive frame; its weakness is that it requires continuous suppression to maintain, because the alternative readings (continuity and composite_overdetermination) remain live historical claims the texts themselves do not adjudicate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of Vatican II (the pre-conciliar Church's perceived inadequacy to modernity) is CONTESTED. The rupture reading treats the founding problem as live and justified the radical implementation. The continuity reading treats the founding problem as misdiagnosed: the pre-conciliar Church did not fail; rather, a progressive minority reinterpreted conciliar texts to authorizing innovation. The overdetermination reading treats the texts as ambiguous and the founding problem as unresolved by the council itself, therefore lending itself to capture by whatever faction controls implementation. The constraint's mandatrophy is conditional: IF the pre-conciliar Church's pastoral and doctrinal response to modernity was actually adequate (the continuity reading), THEN the rupture reading is mandatrophic — a solution persisting after its founding problem was solved by other means. IF the founding problem was genuine (the rupture reading), THEN the constraint is still live. The measurement series shows extraction rising while founding-problem consensus fractured, which suggests a shift toward mandatrophy: post-2010, the founding problem narrative is defended principally by beneficiary seats while alternative readings gain intellectual credibility. Authored as tangled_rope because both genuine coordination (around conciliar texts) and asymmetric extraction (suppression of alternatives) are structurally present, but the founding-problem contestation should trigger a post-hoc mandatrophy reassessment by the engine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_reality_contested,
    'Was the pre-conciliar Church''s response to modernity genuinely inadequate (founding problem live), or does the continuity reading correctly identify the founding problem as a retrospective reinterpretation used to justify innovation (founding problem dead)?',
    'Careful historical analysis of pre-conciliar episcopal response to modernity (1870–1962), contemporary sociological data on Catholic institutional vitality in the pre-conciliar era, and examination of whether post-conciliar pastoral reforms have actually improved outcomes relative to pre-conciliar methods (catechesis, lay participation, missionary effectiveness, doctrinal transmission).',
    'If the founding problem was genuine, the rupture reading remains live and the constraint is justified as solving a real problem. If the founding problem was constructed retrospectively, the constraint shifts toward mandatrophic snare: a solution persisting after its founding problem was resolved by other means or was misdiagnosed entirely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_reality_contested, empirical, 'Whether pre-conciliar ecclesiology was actually deficient or the founding problem was retroactively invented.').

omega_variable(
    conciliar_texts_semantic_stability,
    'Do the Vatican II conciliar texts encode genuinely new doctrinal content, or are they ambiguous formulations that permit both rupture and continuity readings as legitimate extractions from the same texts?',
    'Detailed textual exegesis comparing the exact formulations of Dignitatis Humanae (religious freedom) with pre-conciliar Syllabus of Errors and papal teaching on error; Sacrosanctum Concilium (liturgy) with Pre-Tridentine and Tridentine liturgical theology; Lumen Gentium (Church) with First Vatican Council and prior hierarchical teaching. The question is whether the texts contradict prior teaching explicitly or whether the rupture reading is an interpretive choice among possible readings.',
    'If the texts explicitly contradict prior teaching, the rupture reading is textually grounded and the constraint''s classification as tangled_rope (coordination + enforcement) is justified. If the texts are ambiguous, the constraint shifts toward composite_overdetermination: the texts encode incompatible visions and the rupture reading''s persistence depends on suppression of the alternative readings rather than on textual clarity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conciliar_texts_semantic_stability, conceptual, 'Whether Vatican II texts are genuinely new or ambiguously formulated compromises.').

omega_variable(
    suppression_mechanism_structural_vs_rhetorical,
    'Is the measured suppression (0.72) a structural feature of the rupture reading''s authority (incompatible readings logically cannot coexist in one framework), or is it primarily rhetorical and institutional (the suppression is chosen by beneficiary seats, not logically entailed)?',
    'Historical analysis of doctrinal consistency: Can the rupture reading be held simultaneously with the continuity reading without internal contradiction? (If yes: suppression is chosen; if no: it is structural.) Examination of what happens in jurisdictions where the suppression is relaxed (FSSP communities, Summorum Pontificum parishes, Benedict XVI''s hermeneutics of continuity) — do both readings coexist peacefully or in tension?',
    'If suppression is structural (the readings logically foreclose each other), the constraint''s tangled_rope classification stands: coordination around the rupture reading structurally requires suppression of the continuity reading. If suppression is rhetorical/institutional, the constraint may be better classified as snare: the coordination function is secondary to the extraction (defending the beneficial reinterpretation against challenge).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_rhetorical, conceptual, 'Whether suppression of alternative readings is logically entailed or institutionally chosen.').

omega_variable(
    identity_lock_mechanism_theological_vs_social,
    'Is the traditional_latin_mass_communities'' identity_locked exit status grounded in theological conviction (they cannot accept the rupture reading on doctrine), or in social/ecclesiastical suppression (they could accept it doctrinally but are expelled by institutional enforcement)?',
    'Survey and interview data from traditional communities asking: (a) would you accept the rupture reading if the institutional suppression were removed? (b) do you reject the rupture reading on doctrinal grounds or primarily because it condemns your praxis? Examine historical cases where institutional suppression was relaxed (e.g., Summorum Pontificum 2007–2019) and document whether communities moved toward acceptance of the rupture reading or maintained doctrinal opposition.',
    'If identity-lock is primarily theological, the exit_options coding is correct: they cannot leave without abandoning their faith. The suppression then represents coercion of a genuinely held position, supporting higher-extraction classification. If identity-lock is primarily social/institutional, the suppression creates it rather than enforcing it — the suppression itself is the mechanism of identity-locking. This would suggest the suppression is more extractive than coordinating and might support snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_theological_vs_social, empirical, 'Whether traditional communities'' identity-lock is theological conviction or institutional suppression.').

omega_variable(
    kernel_reading_logical_exhaustion,
    'Are the three declared readings (rupture, continuity, overdetermination) logically exhaustive — i.e., do all positions within Catholicism on Vatican II''s relationship to pre-conciliar teaching fall into one of these three? Or are there coherent readings that the three-reading frame misses?',
    'Comprehensive taxonomy of published theological and hierarchical positions on Vatican II and pre-conciliar continuity (last 30 years): Ratzinger''s later writings, John Paul II''s theology, traditionalist scholarship, progressive post-conciliar theology, Vatican documents on the ''hermeneutics of continuity'' and ''paradigm shift'' language. Check whether every major position is classifiable into one of the three readings or whether there are additional coherent positions the frame excludes.',
    'If the frame is logically exhaustive, the three-reading kernel structure is adequate to model the contested interpretation of Vatican II. If coherent positions exist outside the frame, the kernel decomposition is incomplete and additional readings may need to be authored as separate constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_logical_exhaustion, conceptual, 'Whether the three-reading kernel frame is logically exhaustive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement(vati_tr_t1978, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1978, 0.28).
narrative_ontology:measurement(vati_tr_t1994, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1994, 0.42).
narrative_ontology:measurement(vati_tr_t2010, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2010, 0.54).
narrative_ontology:measurement(vati_tr_t2019, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2019, 0.6).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1962, 0.35).
narrative_ontology:measurement(vati_be_t1978, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1978, 0.52).
narrative_ontology:measurement(vati_be_t1994, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1994, 0.61).
narrative_ontology:measurement(vati_be_t2010, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(vati_be_t2019, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2019, 0.69).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1962, 0.25).
narrative_ontology:measurement(vati_su_t1978, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1978, 0.48).
narrative_ontology:measurement(vati_su_t1994, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1994, 0.62).
narrative_ontology:measurement(vati_su_t2010, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2010, 0.71).
narrative_ontology:measurement(vati_su_t2019, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2019, 0.74).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__rupture_reading, 0.18).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The vatican_ii_magisterial_authority kernel decomposes into three constraint stories, each representing a distinct reading held by different institutional factions within Catholicism. The rupture_reading (this constraint) asserts Vatican II encodes genuinely new ecclesiology superseding pre-conciliar teaching. The continuity_reading asserts Vatican II develops prior teaching without rupture. The composite_overdetermination_reading asserts Vatican II texts are ambiguous and encode irreconcilable visions. All three readings reference the same kernel (the conciliar texts) but extract different ε values, beneficiary structures, and institutional consequences. The rupture reading is the dominant institutional reading (post-conciliar establishment), therefore it influences and suppresses the siblings. Constraint family links: rupture_reading → continuity_reading (influences), rupture_reading → composite_overdetermination_reading (influences).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__rupture_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
