% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_rupture_progressive, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_progressive_reading
 *   human_readable: Vatican II Rupture-Progressive Reading: Doctrinal Authority and Reform Mandate
 *   domain: religious/institutional/hermeneutical
 *
 * SUMMARY:
 *   Vatican II (1962-1965) authorized institutional and doctrinal changes in
 *   the Catholic Church. The progressive reading interprets the Council as a
 *   necessary break with pre-conciliar rigidity, treating doctrinal shifts
 *   (especially on religious freedom) as legitimate reversals, and claiming
 *   that the Council's 'spirit' authorizes developments beyond its literal
 *   text. This reading has dominated post-conciliar theology and episcopal
 *   practice, marginalizing traditionalist alternatives. The progressive
 *   reading extracts authority from traditionalist communities (displacing
 *   their practices, voiding their objections as archaism) while benefiting
 *   reform theologians, progressive bishops, and laity seeking institutional
 *   relevance. The constraint is simultaneously a genuine coordination
 *   solution (the Church must interpret its founding texts; some
 *   hermeneutical framework must prevail) and an asymmetric extraction
 *   (traditionalists bear the cost of institutional displacement without
 *   meaningful voice in the interpretation process). Claim and metrics
 *   diverge intentionally: the constraint is CLAIMED as rope (legitimate
 *   coordination) while authored metrics describe substantially extractive,
 *   actively enforced operation sustained by institutional suppression of
 *   dissent.
 *
 * KEY AGENTS:
 *   - reform_theologians: institutional beneficiaries, set interpretive agendas, dominate theological education
 *   - progressive_bishops_and_clergy: implement the reading, draw Magisterial authority from it, constrained to remain within its framework
 *   - traditionalist_communities: structurally displaced, trapped by identity-fusion and institutional dependence, their objections disempowered
 *   - vatican_doctrinal_authority: sets the interpretive frame, adjudicates disputes, internally contested on how to weight the progressive reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.68).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.52).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "Vatican II Rupture-Progressive Reading: Doctrinal Authority and Reform Mandate").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "religious/institutional/hermeneutical").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'f363e903-2164-4c67-85ce-9208e4da971e').
narrative_ontology:cs_kernel_codification('f363e903-2164-4c67-85ce-9208e4da971e', fixed_text).
narrative_ontology:cs_authority_grounding('f363e903-2164-4c67-85ce-9208e4da971e', lineage).
narrative_ontology:cs_interpretation_layer_present('f363e903-2164-4c67-85ce-9208e4da971e').
narrative_ontology:cs_reading_relation('f363e903-2164-4c67-85ce-9208e4da971e', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('f363e903-2164-4c67-85ce-9208e4da971e', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('f363e903-2164-4c67-85ce-9208e4da971e', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('f363e903-2164-4c67-85ce-9208e4da971e', foundational, conciliar_rupture_with_rigidity_is_legitimate).
narrative_ontology:cs_axiom_status(conciliar_rupture_with_rigidity_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('f363e903-2164-4c67-85ce-9208e4da971e', conciliar_rupture_with_rigidity_is_legitimate, deontological).
narrative_ontology:cs_axiom('f363e903-2164-4c67-85ce-9208e4da971e', foundational, spirit_of_council_hermeneutically_authorizes_development).
narrative_ontology:cs_axiom_status(spirit_of_council_hermeneutically_authorizes_development, holdable).
narrative_ontology:cs_axiom_grounding('f363e903-2164-4c67-85ce-9208e4da971e', spirit_of_council_hermeneutically_authorizes_development, conventional).
narrative_ontology:cs_axiom('f363e903-2164-4c67-85ce-9208e4da971e', secondary, religious_freedom_constitutes_doctrinal_reversal).
narrative_ontology:cs_axiom_status(religious_freedom_constitutes_doctrinal_reversal, holdable).
narrative_ontology:cs_axiom_grounding('f363e903-2164-4c67-85ce-9208e4da971e', religious_freedom_constitutes_doctrinal_reversal, empirically_contingent).
narrative_ontology:cs_reference_frame('f363e903-2164-4c67-85ce-9208e4da971e', pre_conciliar_institutional_rigidity_requiring_structural_break).
narrative_ontology:cs_drift_state('f363e903-2164-4c67-85ce-9208e4da971e', contemporary_post_2013_papacy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f363e903-2164-4c67-85ce-9208e4da971e', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, reform_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_bishops_and_clergy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, modern_laity_seeking_relevance).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_communities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_institutional_arrangements).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, necessity_of_doctrinal_break_with_pre_conciliar_rigidity).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, spirit_of_council_as_intentional_authorization).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, religious_freedom_as_doctrinal_reversal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theologians including Karl Rahner, Edward Schillebeeckx, Walter Kasper, and their intellectual successors who read Vatican II as authorizing doctrinal reinterpretation and development beyond the literal text. They interpret 'spirit of the Council' as a hermeneutical mandate to identify the Council's underlying intent and apply it to contemporary circumstances. Their institutional position strengthens as the conciliar text becomes their interpretive key; they can frame any reform proposal as conciliar fulfillment. They dominate theological education, seminary formation, and bishops' conferences. They face minimal suppression (they define the official interpretation) and collect the primary benefits of the constraint.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, reform_theologians, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, reform_theologians, agenda_setter).

% Bishops and priests implementing liturgical change, ecumenical outreach, and doctrinal development aligned with the progressive reading. They draw authority from the Council texts (especially Gaudium et Spes on engagement with modernity, Dignitatis Humanae on religious freedom, Unitatis Redintegratio on ecumenical dialogue) to justify departures from pre-conciliar practice. Their institutional position depends on the reading's legitimacy; they are freed from certain pre-conciliar constraints (mandatory celibacy objections, liturgical experimentation approval, doctrinal restatement authority) but bound to interpret within the Council's putative 'spirit.' They face moderate suppression when they venture beyond the reading's boundaries, but operate comfortably within them.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_bishops_and_clergy, beneficiary,
    institutional, biographical, constrained, global).

% Laity (particularly educated professionals, women seeking expanded roles, married Catholics with contraception questions) who experience the pre-conciliar Church as culturally distant and doctrinally rigid. The progressive reading offers them an institution that legitimates engagement with modern thought, accepts their plural vocations (professional careers, intellectual pursuits), permits scientific and philosophical dialogue, and treats religious freedom as a principle rather than an indult. They benefit from the reading's framing; their continued participation is conditioned on the Church remaining 'relevant' by progressive lights. Their exit option is available but expensive (social, familial, spiritual identity costs, loss of sacramental belonging). They face minimal direct suppression as long as they accept the progressive reading's framework.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, modern_laity_seeking_relevance, beneficiary,
    moderate, biographical, mobile, global).

% Traditionalist religious communities, institutes, and lay movements (Fraternity of St. Peter, Institute of Christ the King, traditional Latin Mass communities, sedevacantist groups) that read the progressive interpretation as illegitimate rupture. They bear the cost of institutional displacement: their liturgical forms are restricted (Latin Mass permitted 'in the absence of pastoral necessity,' then severely limited, then canonically restored but with conditions), their theological emphases are marginalized or condemned as 'pre-conciliar rigidity,' their pastoral practices are prohibited or monitored. They are trapped by identity-fusion: Catholic identity is constituted through fidelity to pre-conciliar forms, and they cannot exit without spiritual self-annihilation. Their institutional dependence compounds the trap: their seminaries operate under Vatican oversight; their bishops are appointed or recalled by Rome; their communities exist within or negotiate with the hierarchical Church. The progressive reading's ascendancy means their objections are structurally disempowered — their voices are excluded from dominant conciliar interpretation machinery, and their witness is reframed as resistance to change rather than fidelity to tradition.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_communities, payer,
    organized, generational, identity_locked, regional).

% The institutional Magisterium (Roman Curia, Papal Office, episcopal college) that set the framework within which both progressive and traditionalist readings operate. It authorized Vatican II, issued documents open to multiple interpretations, and must adjudicate disputes about conciliar meaning. The Magisterium holds the ultimate interpretive authority but is internally contested; different post-conciliar pontificates have weighted the progressive reading differently. Paul VI (1963-1978) cautiously supported the progressive reading while expressing concern about 'the smoke of Satan' entering the Church through dissent. John Paul II (1978-2005) sought to retrieve pre-conciliar theological frameworks (natural law, Thomistic metaphysics) while formally affirming conciliar documents, creating institutional tension. Benedict XVI (2005-2013) explicitly articulated the hermeneutical tension between 'rupture' and 'continuity,' critiquing the progressive reading's scope while defending certain conciliar documents. Francis (2013-present) has rehabilitated the progressive reading's authority, especially on social justice and ecclesial openness. The Magisterium's own position vacillates, which reinforces the constraint's extractiveness: traditionalists cannot achieve stable institutional recognition, and progressives must continuously reaffirm their reading's legitimacy against Magisterial hedging.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_doctrinal_authority, agenda_setter,
    institutional, civilizational, analytical, universal).

% Cardinals, theologians, and hierarchical actors (including Cardinal Ottaviani, Cardinal Stickler, Cardinal Ratzinger pre-2005, and contemporary figures) who interpret Vatican II as continuous with pre-conciliar doctrine and object to the progressive reading's reinterpretation. They would argue that the Council documents do not authorize the scope of change implemented, that conciliar ambiguities should be read narrowly rather than as intentional openings, and that the 'spirit of the Council' is a hermeneutical abuse. They are structurally disadvantaged in formal settings: their arguments are reframed as resistance to change, their objections are marginalized in bishops' conferences and theological congresses, their institutional influence is limited. Yet they retain moral authority within traditionalist communities and have influenced some pontifical positions (Benedict XVI's 2007 Summorum Pontificum expanding Latin Mass access, though with conditions). Their exclusion is a feature of how the progressive reading maintains authority: they remain in the Church but cannot set the dominant interpretive frame.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_magisterial_voices, excluded,
    institutional, generational, trapped, global).

% Non-confessional scholars (church historians, textual analysts, phenomenologists, hermeneutical philosophers) who study Vatican II and its reception neutrally. They observe the hermeneutical contest without needing to adjudicate it ecclesiastically; their function is to clarify the historical record (what the Council texts actually said, what was intended in the redaction processes, how different parties interpreted it), document the textual ambiguities that enable multiple readings, and analyze the institutional mechanisms by which one reading becomes dominant. They are excluded from Magisterial authority-structures but retain the analytical distance to assess the constraint's structural properties.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, academic_theological_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_progressive_reading, reform_theologians).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_progressive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Legitimates a post-conciliar institutional transformation: the Church integrates modern thought (pluralism, religious freedom, secular scholarship), adopts vernacular worship and reformed liturgy, and engages ecumenical dialogue with non-Catholic Christianity. The progressive reading coordinates this transformation by treating it as the conciliar mandate realized, rather than as institutional rupture requiring separate justification.
% TRANSFER_FUNCTION: Transfers authority over Church teaching from pre-conciliar institutional forms (Latin liturgy, catechetical structure, hierarchical governance) to post-conciliar intellectual and pastoral frameworks (inculturated liturgy, theological re-interpretation, episcopal collegiality rhetoric). Concretely: moves teaching authority from pre-conciliar institutional conservators to progressive theologians and reforming bishops who claim to embody the Council's spirit. Transfers spiritual legitimacy from traditional communities to modernizing communities.
% ABSENT_VOICES: Traditionalist bishops and theologians who oppose the progressive reading are structurally absent from dominant conciliar interpretation machinery. They object but lack institutional levers to shape the dominant narrative. Pre-conciliar institutional practitioners (clergy and laity attached to the Latin liturgy, Tridentine catechesis) find their concerns re-framed as rigidity rather than fidelity, and are displaced without formal voice in the interpretation process.
% DISAPPEARANCE_RATIONALE: If the progressive reading's legitimacy vanished overnight, the post-conciliar institutional apparatus would lose its hermeneutical justification. The vernacular liturgy, collegial governance structures, and ecumenical engagement would require new authorization outside the Council framework, or would be rolled back. The careers and communities built on the reading's authority would need to reorganize. The contest would become explicit rather than mediated through interpretation.
% FOUNDING_PROBLEM: The pre-conciliar Church was rigid, culturally isolated, hostile to modern thought, and losing credibility with educated laity and younger clergy. Vatican II was convened to open the Church to the modern world, validate new theological insights, and reform institutional arrangements that had become historically contingent rather than essential.
% FOUNDING_PROBLEM_CORROBORATION: Reform theologians and progressive bishops attest the founding problem remains partially live: doctrinal rigidity persists in pockets, and modernization remains incomplete. Traditionalist bishops and academic observers attest the founding problem was real but has been substantially solved (the Church has integrated modern thought, albeit with tensions). No neutral independent corroboration exists; the parties dispute whether the problem persists or has been solved by the reforms themselves.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the progressive reading's authority depends on interpreting away traditionalist objections and closing alternative interpretations as illegitimate. The reading benefits organized theologians and progressive bishops who claim it; it imposes costs on traditionalist communities whose institutional arrangements are displaced. Suppression rises over the interval (0.35 to 0.52) as institutional enforcement machinery hardens: papal documents clarify conciliar intent; seminary curricula are reformed; pre-conciliar liturgical forms are restricted. Theater ratio (0.41) indicates that post-conciliar institutions devote significant effort to framing their transformation as conciliar fidelity rather than admitting rupture — much of the suppression apparatus is dressed as authoritative exegesis rather than institutional coercion. Accessibility collapse is moderate (0.48): traditionalist alternatives remain theoretically available but practically suppressed (they require institutional courage to defend, risk marginalization, and carry identity costs). Resistance is high (0.71): traditionalists mount continuous objection; SSPX schism (1988), sedevacantist communities, and growing traditionalist institutional foothold (personal prelatures, FSSP) represent organized resistance. The measurement series track the hardening of both extraction and suppression over six decades: initial extractiveness low (Council ambiguous), rising sharply through the 1970s-1980s (Paul VI and John Paul II institutional consolidation), then plateauing (by the 1990s the framework is locked in). Theater ratio rises in parallel, indicating performative reinforcement: papal emphases on 'authentic conciliar interpretation,' theological congresses defending the reading, seminaries reframing their curricula as conciliar compliance. Suppression requirement rises similarly: progressively explicit institutional measures (restriction of Latin Mass, removal of traditionalist bishops, doctrinal sanctions on traditionalist theologians, institutional marginalization).
 *
 * PERSPECTIVAL GAP:
 *   The progressive-reading seats (reform theologians, progressive bishops, modern laity) experience the constraint as legitimate coordination: the Council established an authoritative interpretive framework, and they are its faithful implementers. They face low effective extraction (the framework benefits them) and low suppression (they are freely choosing implementation). The traditionalist seats experience the same constraint as enforced extraction: the progressive reading's dominance is maintained by suppressing their objections, displacing their institutional arrangements, and framing their resistance as disobedience. They face high effective extraction (institutional displacement) and high suppression (structured exclusion from interpretive authority). The Vatican's institutional seat is internally conflicted: it authorized the Council, benefits from the progressive reading's institutional consolidation, but must adjudicate traditionalist objections and maintain institutional unity. Different pontificates weight the progressive reading differently (Paul VI cautious, John Paul II corrective, Francis rehabilitative), which keeps the constraint extractive rather than stabilizing it. The engine computes per-seat directionality from beneficiary/victim + power + exit, capturing this perspectival gap: reform theologians (beneficiary, organized power, high exit options via academic/theological markets) → d near 0.0; traditionalists (victim, organized but institutionally trapped, identity-locked exit) → d near 1.0; progressive bishops (beneficiary but institutionally bound, constrained exit) → d moderate (0.3-0.4); Vatican authority (agenda-setter, institutional power, analytical position) → d moderate-symmetric (0.4-0.5).
 *
 * DIRECTIONALITY LOGIC:
 *   Reform theologians are direct beneficiaries: the progressive reading legitimates their intellectual work, positions them as authoritative interpreters, and creates institutional demand for their expertise. They have high exit options (academic markets, theological publishing, international networks) and organized power, so their d is low (0.15-0.25). Progressive bishops are constrained beneficiaries: they benefit from the reading's authorization to reform their dioceses, but their exit options are limited (episcopal identity, institutional hierarchy) and their power is institutional but hierarchically bounded. Their d is moderate (0.35-0.45). Modern laity seeking relevance are diffuse beneficiaries: the reading legitimates their engagement with modern thought and plural vocations, but they have higher exit options (switching churches, secular identity, religious disengagement) and only moderate organized power. Their d is near-symmetric (0.45-0.55). Traditionalist communities are clear targets: they bear the institutional displacement, are trapped by identity-fusion, and have structured exclusion from interpretive authority. Their d is high (0.75-0.85). The Vatican's institutional seat is the agenda-setter: it authorized the Council, sets the interpretive frame, and must adjudicate disputes. Its d is moderate-symmetric (0.45-0.55) because it benefits from the reading's institutional stability but faces continuous pressure to adjudicate traditionalist objections and risks institutional schism if it moves too far in either direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pre-conciliar rigidity, cultural isolation, loss of credibility with educated laity) was real and urgent. The progressive reading's classification as tangled_rope depends on distinguishing its coordination function (the Church must interpret its founding texts; some framework must legitimate institutional transformation) from its extraction function (the progressive reading benefits organized theologians and progressive bishops while displacing traditionalist communities). The coordination function is genuine: Vatican II did accomplish a needed institutional and intellectual opening; the progressive reading provides a hermeneutical framework that makes this opening coherent rather than arbitrary. The extraction function is also genuine: the reading's authority depends on suppressing traditionalist alternatives; traditionalist objections are reframed as rigidity rather than fidelity; pre-conciliar institutional arrangements are displaced without negotiation. The tangled_rope classification holds if both functions persist: the Church continues to need a coherent interpretive framework (coordination), and the progressive reading continues to extract benefits from organized theologians and bishops while suppressing traditionalist dissent (extraction). Mandatrophy (the founding problem outliving the constraint's function) would arise if: (a) the founding problem were solved (the Church has achieved adequate cultural integration and is no longer rigid — contested; traditionalists argue it is solved; progressives argue it is incomplete), and (b) the constraint persisted anyway (the interpretive framework has calcified and is defended theatrically rather than functionally). The measurement data show moderate theater_ratio rise (0.22 to 0.41), suggesting performative maintenance is increasing, but not yet dominant. If theater_ratio approaches 0.6-0.7 while extractiveness plateaus, mandatrophy would be indicated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is Vatican II a rupture with pre-conciliar doctrine or an organic development within unchanging tradition?',
    'Textual analysis of conciliar documents (especially Unitatis Redintegratio, Dignitatis Humanae, Gaudium et Spes) against pre-conciliar Magisterial teaching; historical study of conciliar intent vs. post-conciliar interpretation; formal theological comparison of pre- and post-conciliar doctrinal content.',
    'If rupture is defensible as doctrinally legitimate, the progressive reading''s structural authority is strengthened; if continuity is defensible, the reading becomes interpretive overreach, collapsing into the continuity_reading. This omega is the kernel contest itself — its resolution would reclassify multiple sibling constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether Vatican II constitutes a break or development in Catholic doctrine.').

omega_variable(
    spirit_of_council_hermeneutical_mandate,
    'Does the Council''s ''spirit'' authorize developments beyond the literal text, or is appeal to spirit a hermeneutical abuse that extends authority beyond what the texts warrant?',
    'Formal hermeneutical study: (1) Does the Council itself authorize interpretive development beyond its text? (2) What textual markers indicate intentional openness vs. deliberate precision? (3) Comparative analysis: do other authoritative texts (papal encyclicals, prior councils) appeal to spirit, and how is overreach distinguished from legitimate development?',
    'If spirit-hermeneutics is structurally authorized, the progressive reading''s extraction from traditionalist communities is justified as conciliar fulfillment. If spirit-hermeneutics is hermeneutically unjustified, the reading''s authority collapses into textual overreach, and the suppression of traditionalist communities becomes institutional coercion rather than conciliar mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spirit_of_council_hermeneutical_mandate, conceptual, 'Whether appeal to the Council''s spirit is a legitimate hermeneutical move or an abuse that exceeds textual warrant.').

omega_variable(
    religious_freedom_as_doctrinal_reversal,
    'Does Dignitatis Humanae represent a doctrinal reversal of the Syllabus of Errors (as the progressive reading asserts) or an organic development of Catholic teaching on human dignity (as the continuity reading asserts)?',
    'Textual comparison of the Syllabus (1864), pre-conciliar encyclicals (Quanta Cura, Immortale Dei), and Dignitatis Humanae. Historical study of the redaction history of Dignitatis Humanae, including the input of conservative bishops and theologians who negotiated the text. Examination of whether a doctrine of conscientious freedom can be derived from prior Catholic moral theology without explicit reversal.',
    'If reversal is defensible, religious freedom becomes a marker of the Council''s intentional break with pre-conciliar rigidity, and the progressive reading''s framing is validated. If development is defensible, the reading loses a key structural marker of rupture, and collapses toward the continuity reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_freedom_as_doctrinal_reversal, empirical, 'Whether the Council''s teaching on religious freedom reverses prior doctrine or develops it.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of traditionalist voices and communities a structural feature of the post-conciliar institutional apparatus, or is it an internalized suppression (traditionalists accept subordination through their own identity-fusion to conciliar obedience)?',
    'Post-exit trajectory study: when traditionalist communities acquire institutional autonomy (personal prelatures, canonical recognitions), does their suppression persist? Qualitative analysis of traditionalist testimonies about identity-lock mechanisms (fidelity to the magisterium as a constituting identity, even when the magisterium contradicts their witness).',
    'If suppression is structural, the constraint is sustained by institutional enforcement machinery; if internalized, traditionalist agents carry the suppression with them even if institutional barriers weaken. This distinction affects whether the constraint''s classification changes if institutional enforcement is removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of traditionalist dissent is structural or internalized identity-lock.').

omega_variable(
    identity_fusion_traditionalist_exit,
    'How far does Catholic identity-fusion lock traditionalist agents into the constraint? If institutional enforcement weakened, would exit costs (schism, spiritual excommunication, social rupture) prevent them from leaving, or would the constraint''s legitimacy become contestable?',
    'Historical precedent from traditionalist ruptures (SSPX schism 1988, sedevacantism): agents who exited faced identity costs but did exit. Study the heterogeneity of exit costs within traditionalist populations — some communities (Fraternity of St. Peter) chose integration; others (SSPX) chose schism. Assess whether the reading''s authority is sustained by institutional force or by traditionalists'' internalized acceptance of magisterial authority even against their own witness.',
    'If identity-fusion is complete, traditionalists remain trapped and suppression remains high even if institutional mechanisms weaken. If exit is possible (even costly), the constraint''s persistence depends on continued institutional enforcement, not on internalized lock-in — classification implications for seat-level directionality and effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_traditionalist_exit, empirical, 'Depth of identity-lock binding traditionalist agents into the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(vati_tr_t0, projected).
narrative_ontology:measurement(vati_tr_t8, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(vati_tr_t8, observed).
narrative_ontology:measurement(vati_tr_t16, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement_basis(vati_tr_t16, observed).
narrative_ontology:measurement(vati_tr_t24, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement_basis(vati_tr_t24, observed).
narrative_ontology:measurement(vati_tr_t32, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(vati_tr_t32, observed).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(vati_tr_t40, observed).
narrative_ontology:measurement(vati_tr_t48, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 48, 0.41).
narrative_ontology:measurement_basis(vati_tr_t48, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(vati_be_t0, projected).
narrative_ontology:measurement(vati_be_t8, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement_basis(vati_be_t8, observed).
narrative_ontology:measurement(vati_be_t16, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(vati_be_t16, observed).
narrative_ontology:measurement(vati_be_t24, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement_basis(vati_be_t24, observed).
narrative_ontology:measurement(vati_be_t32, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(vati_be_t32, observed).
narrative_ontology:measurement(vati_be_t40, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(vati_be_t40, observed).
narrative_ontology:measurement(vati_be_t48, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 48, 0.68).
narrative_ontology:measurement_basis(vati_be_t48, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(vati_su_t0, projected).
narrative_ontology:measurement(vati_su_t8, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement_basis(vati_su_t8, observed).
narrative_ontology:measurement(vati_su_t16, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement_basis(vati_su_t16, observed).
narrative_ontology:measurement(vati_su_t24, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 24, 0.49).
narrative_ontology:measurement_basis(vati_su_t24, observed).
narrative_ontology:measurement(vati_su_t32, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 32, 0.51).
narrative_ontology:measurement_basis(vati_su_t32, observed).
narrative_ontology:measurement(vati_su_t40, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(vati_su_t40, observed).
narrative_ontology:measurement(vati_su_t48, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 48, 0.52).
narrative_ontology:measurement_basis(vati_su_t48, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Vatican II doctrinal authority constraint family: four distinct readings of the same kernel, each with different ε values, beneficiary structures, and institutional effects. The rupture_progressive_reading (this constraint) reads the Council as authorizing break with rigidity; the continuity_reading reads it as development; the rupture_traditionalist_reading reads it as rupture-with-errors enabling heterodox implementation; the composite_overdetermination_reading treats the Council as convergence of multiple structural changes conflated as unified. Each reading is a separate constraint with its own Magisterial authority grounding, institutional beneficiaries, and suppression mechanics. Sibling readings are linked via network.affects_constraints; they share the kernel but diverge structurally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
