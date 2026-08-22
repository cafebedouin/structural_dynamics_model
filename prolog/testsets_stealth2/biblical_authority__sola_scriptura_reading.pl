% ============================================================================
% CONSTRAINT STORY: biblical_authority__sola_scriptura_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__sola_scriptura_reading, []).

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
 *   constraint_id: biblical_authority__sola_scriptura_reading
 *   human_readable: Sola Scriptura: Scripture as Sufficient, Self-Interpreting Authority
 *   domain: theology/religious history
 *
 * SUMMARY:
 *   The sola scriptura arrangement designates the biblical canon as the sole
 *   sufficient and self-interpreting authority for doctrine and practice,
 *   displacing the magisterial and conciliar interpretive structures of the
 *   late-medieval church. It is ONE reading of the kernel biblical_authority;
 *   the tradition_scripture and conciliar readings are separate constraint
 *   stories (linked via network.affects_constraints) with their own epsilon,
 *   victim sets, and classifications — this story does not average across
 *   them. The expected structural delta holds: clerical extraction is low
 *   relative to the magisterial sibling (no tithe on interpretation,
 *   congregational autonomy, vernacular access), but the arrangement carries
 *   real standing costs — doctrinal fragmentation with no adjudicative
 *   monopoly, local discipline of dissenting readers without appeal, and a
 *   devolution of interpretive authority onto local teaching seats and
 *   entrepreneurial teachers. The epsilon referent is the standing sola
 *   scriptura arrangement itself, not the magisterial structure it displaced
 *   and not any endorsed reunification. Claim/metric independence is
 *   preserved: claimed_type is tangled_rope from the structural analysis
 *   (genuine coordination function plus identifiable victims plus active
 *   enforcement); the metrics describe the arrangement's actual operation,
 *   and any divergence from the reading's self-assessment is the measurement,
 *   not an error to reconcile.
 *
 * KEY AGENTS:
 *   - - lay_believers: dual-positioned beneficiary/payer (organized/constrained) — gain direct vernacular access to the text; bear the interpretive labor, fragmentation confusion, and exposure to local discipline
 *   - - congregational_teaching_office: distributed local agenda-setter (organized/mobile) — occupies the seat where interpretive authority landed; preaches, decides doctrine, disciplines dissent
 *   - - dissenting_interpreters: primary target (powerless/trapped) — read differently, face discipline, have no adjudicative venue above the local community
 *   - - magisterial_tradition_communities: excluded rival authority (institutional/mobile) — suppressed alternative whose claim cannot count inside the arrangement's frame
 *   - - independent_bible_teachers: secondary beneficiary (moderate/arbitrage) — entrepreneurial interpreters legitimated by the self-interpreting claim
 *   - - vernacular_bible_publishers: secondary beneficiary (organized/arbitrage) — run the translation and distribution economy the arrangement requires
 *   - - denominational_bodies: distributed agenda-setter (institutional/mobile) — maintain confessional standards and seminary infrastructure while professing sufficiency
 *   - - ecumenical_scholars_and_bodies: analytical observer (analytical/analytical) — document fragmentation and attest the historical record from outside the benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.47).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.3).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Sola Scriptura: Scripture as Sufficient, Self-Interpreting Authority").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theology/religious history").

domain_priors:requires_active_enforcement(biblical_authority__sola_scriptura_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, '2a81ee69-e926-45f5-b8b7-7b1d5454cf25').
narrative_ontology:cs_kernel_codification('2a81ee69-e926-45f5-b8b7-7b1d5454cf25', fixed_text).
narrative_ontology:cs_authority_grounding('2a81ee69-e926-45f5-b8b7-7b1d5454cf25', self_enforcing).
narrative_ontology:cs_reading_relation('2a81ee69-e926-45f5-b8b7-7b1d5454cf25', biblical_authority__tradition_scripture_reading, forecloses).
narrative_ontology:cs_reading_relation('2a81ee69-e926-45f5-b8b7-7b1d5454cf25', biblical_authority__conciliar_reading, forecloses).
narrative_ontology:cs_axiom('2a81ee69-e926-45f5-b8b7-7b1d5454cf25', foundational, scripture_alone_sufficient_for_doctrine).
narrative_ontology:cs_axiom_status(scripture_alone_sufficient_for_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('2a81ee69-e926-45f5-b8b7-7b1d5454cf25', scripture_alone_sufficient_for_doctrine, theological).
narrative_ontology:cs_axiom('2a81ee69-e926-45f5-b8b7-7b1d5454cf25', foundational, scripture_perspicuous_to_faithful_reader).
narrative_ontology:cs_axiom_status(scripture_perspicuous_to_faithful_reader, holdable).
narrative_ontology:cs_axiom_grounding('2a81ee69-e926-45f5-b8b7-7b1d5454cf25', scripture_perspicuous_to_faithful_reader, empirically_contingent).
narrative_ontology:cs_reference_frame('2a81ee69-e926-45f5-b8b7-7b1d5454cf25', self_interpreting_apostolic_canon).
narrative_ontology:cs_drift_state('2a81ee69-e926-45f5-b8b7-7b1d5454cf25', contemporary_fragmentation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2a81ee69-e926-45f5-b8b7-7b1d5454cf25', '').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, congregational_teaching_office).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, independent_bible_teachers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, vernacular_bible_publishers).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, dissenting_interpreters).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, magisterial_tradition_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, denominational_bodies).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, perspicuity_of_scripture).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, sufficiency_of_scripture_doctrine).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, priesthood_of_all_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold direct access to the vernacular Bible and are taught that no hierarchical mediation stands between them and the text. In practice they depend on their congregation's teaching office for interpretive direction, carry the labor of personal reading, absorb the confusion of a fragmented doctrinal landscape, and can be disciplined by their local community for reading differently. Leaving means leaving their community and often their sense of self.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_believers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, lay_believers, payer).

% Pastors, teaching elders, and local preachers occupy the seat where interpretive authority landed when the magisterium was set aside. They preach, decide what the text means for their community, administer discipline, and legitimate their position by pointing to the text rather than to their office. They collect salary, deference, and the power to define orthodoxy locally. Exit is comparatively cheap: a trained preacher can move congregations, plant a church, or join a network.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, congregational_teaching_office, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, congregational_teaching_office, beneficiary).

% Media pastors, para-church teachers, and authors build audiences by teaching the Bible directly, with the doctrine that the text is self-interpreting serving as their credential — they bring the Bible, not a tradition. Their authority rests on charisma and platform; they capture attention, donations, and publishing income, and can move audiences between platforms if a ministry collapses.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, independent_bible_teachers, beneficiary,
    moderate, biographical, arbitrage, global).

% Bible societies and religious publishers run the translation, printing, and distribution economy the arrangement requires: new translations, study Bibles, reading plans, curriculum. They profit from every interpretive market the fragmentation opens and can redirect capital to other religious publishing if demand shifts.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, vernacular_bible_publishers, beneficiary,
    organized, generational, arbitrage, global).

% Members who read the text differently from their congregation's consensus — on baptism, gender, eschatology, or politics. They face teaching correction, formal discipline, or quiet exclusion, and have no adjudicative venue above the local community: appealing to the text means appealing to the same book their judges read. Exit requires leaving the community and often family and identity with it.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, dissenting_interpreters, payer,
    powerless, biographical, trapped, local).

% The Catholic and Orthodox churches hold that authoritative interpretation requires tradition and, in the Catholic case, a magisterium. The arrangement under contest defines the conversation so that their claim cannot count as adjudication. They were suppressed in Protestant territories during the confessional era and bear the schism of Western Christendom as a standing cost. They object from outside the arrangement's frame, with their own complete authority structure to retreat into.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, magisterial_tradition_communities, excluded,
    institutional, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, magisterial_tradition_communities, payer).

% Denominations and confessional associations maintain the standards, seminaries, and discipline machinery through which congregational interpretation stays roughly aligned. They profess the sufficiency of scripture while administering extensive interpretive infrastructure, and collect dues, loyalty, and institutional continuity. They can merge, split, or rebrand at moderate cost.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, denominational_bodies, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, denominational_bodies, beneficiary).

% Academic historians, theologians, and ecumenical dialogue commissions document the fragmentation, compare the readings of the authority question across traditions, and attest the historical record from outside any benefiting party. They hold no enforcement position and bear none of the arrangement's costs.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, ecumenical_scholars_and_bodies, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__sola_scriptura_reading, congregational_teaching_office).
narrative_ontology:fixing_cost_class(biblical_authority__sola_scriptura_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Designates a single accessible normative corpus (the canon) as the common standard for doctrine and practice, so that teaching, membership, and dispute resolution can proceed without a hierarchical adjudicating institution; enables vernacular religious life, congregational self-governance, and lay literacy-based faith.
% TRANSFER_FUNCTION: Moves interpretive authority out of a centralized magisterium and re-situates it in the text and, in operation, in local teaching offices and self-appointed teachers; moves enforcement of doctrinal boundaries from universal courts to congregational discipline; moves the interpretive labor burden onto each believer.
% ABSENT_VOICES: The magisterial traditions (Catholic and Orthodox) are excluded from adjudication by the constraint's own terms — they would argue that scripture requires tradition, but the arrangement defines the conversation so their argument cannot count; they sit outside it as the excluded seat. Dissenting interpreters within congregations have no venue above the local community that judges them. Historically, the radical reformers (Anabaptists) who applied the sufficiency principle more consistently than the magisterial Reformers were persecuted by those same Reformers — the enforcement machinery suppressed its own logical extension.
% DISAPPEARANCE_RATIONALE: Roughly half of global Christianity organizes its doctrinal life around this principle. Overnight disappearance would force every congregation, denomination, seminary, and media ministry to re-derive its authority structure — toward conciliar, magisterial, or novel charismatic forms. The Bible-publishing economy, confessional standards, congregational autonomy, and the lay-interpretive identity of hundreds of millions all presuppose it; nothing in the current arrangement survives its removal unchanged.
% FOUNDING_PROBLEM: The late-medieval authority crisis: a magisterium perceived as corrupt (indulgences, simony, curial politics) stood between believers and the text; the reformers needed a doctrinal authority that could not be captured by the compromised hierarchy and a standard available in principle to every believer.
% FOUNDING_PROBLEM_CORROBORATION: The historical crisis is corroborated from outside the benefiting parties: Catholic and Orthodox historical scholarship acknowledges the late-medieval corruption as real (Trent's own reform decrees concede it), and secular Reformation historiography documents both the crisis and the confessional enforcement response. No party outside the Protestant beneficiary set attests that the founding problem remains live in its original form — the still-live status is asserted by the beneficiaries and disputed by the magisterial traditions and much of the secular academy, which read the arrangement's persistence as identity and institutional momentum. That contest is itself the finding.
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_authority__sola_scriptura_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__sola_scriptura_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__sola_scriptura_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__sola_scriptura_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.47): low on the clerical axis relative to the magisterial sibling — no centralized office collects a rent on interpretation — but materially extractive on three other channels: the fragmentation cost borne by every community, disciplinary extraction from dissenting readers, and entrepreneurial capture of the interpretive free-for-all. Suppression is 0.30 in the standing arrangement: coercive territorial enforcement ended with disestablishment, but boundary maintenance persists through confessional discipline, institutional exclusion, and identity-based enforcement — the suppression_requirement series traces exactly this migration from coercive (peak 0.82 during the Thirty Years' War) to institutional to identity-based enforcement, which is why the enforcement-capacity series is authored for this story. Theater_ratio is high (0.58) and rising across the interval: the claim that scripture interprets itself coexists with an ever-larger mediating apparatus (confessions, catechisms, seminaries, study-Bible industries, celebrity teaching ministries); 'no creed but the Bible' has itself functioned as a creed since at least 1910. Accessibility_collapse is 0.45: the magisterial alternatives persist, remain institutionally intact, and people convert in both directions — the constraint does not fully collapse its alternatives. Resistance is 0.60: standing magisterial critique, internal schism (every new denomination is both product and protest), and critical scholarship. All three series run on one shared ten-point grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the congregational teaching office the arrangement is the liberation of the text from a corrupt hierarchy — the coordination it built and staffs. From the dissenting interpreter's seat the same structure is discipline without appeal: the constraint that freed the text from Rome offers no court above the local community that judges them. From the excluded magisterial seat the whole arrangement is a schism engine that delegitimizes rival authority by definition. The reading's own self-assessment would score its arrangement near zero extraction (liberation, not extraction); the analytical assessment of actual operation is moderate. That divergence is recorded here and in the omega set — the engine computes per-seat classifications from the structural data, and this story does not adjudicate it. Note also the same-function differentiation between the teaching office and independent teachers: identical nominal role (interpret the text), radically different exit mobility, which is what separates their directionalities despite equal doctrinal standing.
 *
 * DIRECTIONALITY LOGIC:
 *   The congregational teaching office sits near the beneficiary end: it receives the devolved interpretive authority and its fruits (deference, salary, local definitional power), and fragmentation multiplies teaching seats rather than threatening them. Independent bible teachers sit similarly low with arbitrage-grade exit; publishers likewise. Lay believers sit near symmetric (dual beneficiary/payer): genuine access benefit against interpretive burden, discipline exposure, and fragmentation confusion — the derivation reads their beneficiary declaration; their payer side is carried by the secondary role and this commentary. Dissenting interpreters sit near the full-target end: powerless, trapped, no appeal venue. Magisterial tradition communities bear high directionality (they are the suppressed alternative) but their mobile exit — a complete rival authority structure to retreat into — damps their effective extraction well below what a trapped target at the same directionality would bear. Denominational bodies are agenda-setters with modest personal extraction: they administer boundaries more than they collect from them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a magisterium perceived as corrupt standing between believers and the text, requiring an authority that could not be captured by it — is partially dead: the specific late-medieval crisis was addressed (the Counter-Reformation's own reform decrees concede it), and the arrangement now persists substantially by identity and institutional momentum. But the status is genuinely contested: Protestant communities attest a permanent version of the problem (every hierarchy is corruptible, so scriptural normativity must remain structurally independent). The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: no zombie flag fires, and honestly so — the arrangement still performs real coordination. The tangled_rope classification is what prevents mislabeling here: reading the arrangement as pure extraction would erase the genuine vernacular-access coordination that billions of lay believers use daily; reading it as pure coordination would erase the identifiable victims (dissenting interpreters, doctrinal coherence) and the suppression record. The rising theater series marks the specific drift risk: the self-interpreting claim is increasingly performed while mediation apparatus does the work — if the founding problem were ever adjudicated dead while the profession persisted, the arrangement's trajectory is toward piton (theatrical maintenance of a claim its own practice has abandoned), not snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is the sola_scriptura_reading of the kernel biblical_authority; would the structural classification of ''biblical authority'' change under the sibling readings tradition_scripture_reading or conciliar_reading?',
    'Comparative classification across the three sibling constraint stories: author the tradition_scripture and conciliar readings with the same interval and grid, then compare victim sets, epsilon, and computed types.',
    'Under the tradition reading the victim set inverts (lay interpretive autonomy bears the extraction; the clerical hierarchy collects) and epsilon rises; under the conciliar reading adjudicative authority concentrates in assembly and fragmentation costs fall. Network contamination flows between the siblings would reverse direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Which reading of the biblical-authority kernel this classification describes, and what siblings would change.').

omega_variable(
    perspicuity_empirical_status,
    'Does the fragmentation record (thousands of mutually incompatible denominations under a shared profession of scriptural sufficiency) falsify the perspicuity premise that the text is self-interpreting?',
    'Comparative doctrine history: measure interpretive convergence among faithful readers under controlled access conditions, and compare divergence rates against traditions with adjudicative monopoly.',
    'If perspicuity fails, the self-interpreting claim is largely theatrical and the arrangement drifts toward pure extraction through local charismatic capture with no appeal venue; if it holds, fragmentation has exogenous causes and the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perspicuity_empirical_status, empirical, 'Whether the self-interpreting premise survives its own operating record.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression that maintains interpretive boundaries structural (confessional discipline, institutional exclusion, historically coercive enforcement) or internalized (believers self-censor interpretive doubt as part of their identity)?',
    'Post-exit suppression trajectory: track converts who leave sola scriptura communities for magisterial or secular contexts; if interpretive self-censorship and authority-deferral persist after the disciplinary structure is removed, a substantial share is internalized.',
    'If internalized, effective suppression is materially higher than the structural 0.30, and the lay exit option reads closer to identity_locked than constrained, raising effective extraction on the lay seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of interpretive dissent.').

omega_variable(
    fragmentation_attribution,
    'Is doctrinal fragmentation across Protestant communities caused by the sola scriptura arrangement itself, or by confounders (printing, vernacular literacy, nationalism, disestablishment, modernity) that would fragment any tradition?',
    'Compare fragmentation rates of traditions with adjudicative monopoly (Catholic, Orthodox) under the same modernizing pressures, controlling for polity and geographic spread.',
    'If fragmentation is largely endogenous, doctrinal coherence is a genuine victim of this constraint and epsilon is correctly moderate; if exogenous, the victim declaration weakens and the arrangement classifies closer to rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fragmentation_attribution, empirical, 'Attribution of the fragmentation cost to this constraint versus background modernizing forces.').

omega_variable(
    canon_self_attestation,
    'The constraint''s authority (scripture) cannot establish its own boundaries (which books are scripture) without appeal to the tradition it displaces: is the arrangement''s foundation self-grounding or parasitic on the rival authority it suppresses?',
    'Conceptual analysis within the reading''s own framework: test whether Reformed self-attestation arguments (the canon recognized, not authorized, by the church) can be stated without borrowing the epistemic weight of historical transmission — i.e., of tradition.',
    'If parasitic, the coordination function is inherited from the suppressed rival and the independence claim is theatrical at the foundation, raising the theater assessment and supporting the magisterial sibling''s critique; if self-grounding, the arrangement stands on its own epistemic feet and the measured theater is operational only.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(canon_self_attestation, conceptual, 'Whether the constraint''s foundation is self-grounding or borrowed from the authority it displaces.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 1521, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1521, biblical_authority__sola_scriptura_reading, theater_ratio, 1521, 0.22).
narrative_ontology:measurement(bibl_tr_t1555, biblical_authority__sola_scriptura_reading, theater_ratio, 1555, 0.34).
narrative_ontology:measurement(bibl_tr_t1580, biblical_authority__sola_scriptura_reading, theater_ratio, 1580, 0.44).
narrative_ontology:measurement(bibl_tr_t1618, biblical_authority__sola_scriptura_reading, theater_ratio, 1618, 0.46).
narrative_ontology:measurement(bibl_tr_t1648, biblical_authority__sola_scriptura_reading, theater_ratio, 1648, 0.44).
narrative_ontology:measurement(bibl_tr_t1750, biblical_authority__sola_scriptura_reading, theater_ratio, 1750, 0.4).
narrative_ontology:measurement(bibl_tr_t1830, biblical_authority__sola_scriptura_reading, theater_ratio, 1830, 0.42).
narrative_ontology:measurement(bibl_tr_t1910, biblical_authority__sola_scriptura_reading, theater_ratio, 1910, 0.52).
narrative_ontology:measurement(bibl_tr_t1965, biblical_authority__sola_scriptura_reading, theater_ratio, 1965, 0.48).
narrative_ontology:measurement(bibl_tr_t2026, biblical_authority__sola_scriptura_reading, theater_ratio, 2026, 0.58).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1521, biblical_authority__sola_scriptura_reading, base_extractiveness, 1521, 0.36).
narrative_ontology:measurement(bibl_be_t1555, biblical_authority__sola_scriptura_reading, base_extractiveness, 1555, 0.52).
narrative_ontology:measurement(bibl_be_t1580, biblical_authority__sola_scriptura_reading, base_extractiveness, 1580, 0.58).
narrative_ontology:measurement(bibl_be_t1618, biblical_authority__sola_scriptura_reading, base_extractiveness, 1618, 0.64).
narrative_ontology:measurement(bibl_be_t1648, biblical_authority__sola_scriptura_reading, base_extractiveness, 1648, 0.56).
narrative_ontology:measurement(bibl_be_t1750, biblical_authority__sola_scriptura_reading, base_extractiveness, 1750, 0.44).
narrative_ontology:measurement(bibl_be_t1830, biblical_authority__sola_scriptura_reading, base_extractiveness, 1830, 0.38).
narrative_ontology:measurement(bibl_be_t1910, biblical_authority__sola_scriptura_reading, base_extractiveness, 1910, 0.46).
narrative_ontology:measurement(bibl_be_t1965, biblical_authority__sola_scriptura_reading, base_extractiveness, 1965, 0.4).
narrative_ontology:measurement(bibl_be_t2026, biblical_authority__sola_scriptura_reading, base_extractiveness, 2026, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1521, biblical_authority__sola_scriptura_reading, suppression_requirement, 1521, 0.28).
narrative_ontology:measurement(bibl_su_t1555, biblical_authority__sola_scriptura_reading, suppression_requirement, 1555, 0.66).
narrative_ontology:measurement(bibl_su_t1580, biblical_authority__sola_scriptura_reading, suppression_requirement, 1580, 0.74).
narrative_ontology:measurement(bibl_su_t1618, biblical_authority__sola_scriptura_reading, suppression_requirement, 1618, 0.82).
narrative_ontology:measurement(bibl_su_t1648, biblical_authority__sola_scriptura_reading, suppression_requirement, 1648, 0.6).
narrative_ontology:measurement(bibl_su_t1750, biblical_authority__sola_scriptura_reading, suppression_requirement, 1750, 0.42).
narrative_ontology:measurement(bibl_su_t1830, biblical_authority__sola_scriptura_reading, suppression_requirement, 1830, 0.3).
narrative_ontology:measurement(bibl_su_t1910, biblical_authority__sola_scriptura_reading, suppression_requirement, 1910, 0.36).
narrative_ontology:measurement(bibl_su_t1965, biblical_authority__sola_scriptura_reading, suppression_requirement, 1965, 0.24).
narrative_ontology:measurement(bibl_su_t2026, biblical_authority__sola_scriptura_reading, suppression_requirement, 2026, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, information_standard).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__conciliar_reading).

% DUAL FORMULATION NOTE:
% Constraint family: 'biblical authority' decomposes per the epsilon-invariance principle into three readings of one kernel — sola_scriptura (this story), tradition_scripture, and conciliar. The colloquial label 'biblical authority' conflates structurally distinct claims: whether the text's authority requires an interpretive institution. Each reading gets its own epsilon, beneficiary/victim structure, and classification; this story's epsilon (0.47, moderate, fragmentation-and-discipline channels) is authored for the sola scriptura arrangement only. The siblings are downstream of this reading historically in one direction (its emergence forced Trent's formalization of the dual-source position) and upstream in another (patristic and conciliar practice predates it by a millennium and supplied the canon its authority depends on — see the canon_self_attestation omega). All three stories link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
