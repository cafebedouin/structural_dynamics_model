% ============================================================================
% CONSTRAINT STORY: biblical_authority__conciliar_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__conciliar_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: biblical_authority__conciliar_reading
 *   human_readable: Conciliar-Patristic Reading of Scriptural Authority (Eastern Orthodox Tradition)
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This constraint models the Eastern Orthodox reading of scriptural
 *   authority: Scripture is authoritative but its meaning is settled through
 *   the seven ecumenical councils and the consensus of the Church Fathers,
 *   understood as a living tradition rather than a magisterial decree issued
 *   by a single supreme office. This is structurally distinct from both the
 *   sola scriptura reading (Scripture alone, self-interpreting, no conciliar
 *   or magisterial mediation required) and the tradition_scripture_reading
 *   (Catholic magisterium as a standing, centralized interpretive authority
 *   that actively guards and can develop the deposit of faith). The conciliar
 *   reading distributes interpretive authority among peer autocephalous sees
 *   rather than concentrating it, which produces moderate rather than severe
 *   extraction (episcopal, not papal) but also produces jurisdictional
 *   fragmentation that the other two readings do not structurally generate in
 *   the same way.
 *
 * KEY AGENTS:
 *   - autocephalous_hierarchs: administer conciliar-patristic authority across self-governing churches (institutional/arbitrage) — set the agenda for what counts as continuity
 *   - patristic_scholars: gatekeep what counts as authentic Fathers' consensus (organized/constrained) — collateral beneficiaries whose exit is professionally costly
 *   - reform_minded_clergy: bear the cost of slow doctrinal development (moderate/constrained) — primary payers within the clerical class
 *   - laity_seeking_doctrinal_adaptation: bear the cost individually in daily life (powerless/constrained) — primary lay payers
 *   - diaspora_communities_across_jurisdictions: bear the cost of jurisdictional fragmentation with no recourse (powerless/trapped) — structurally excluded from the politics that determines their situation
 *   - roman_catholic_magisterium: sibling-reading institution excluded from adjudicating this reading's disputes
 *   - ecumenical_and_comparative_theologians: analytical observers of how conciliar consensus forms and calcifies across history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__conciliar_reading, 0.42).
domain_priors:suppression_score(biblical_authority__conciliar_reading, 0.38).
domain_priors:theater_ratio(biblical_authority__conciliar_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__conciliar_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__conciliar_reading, "Conciliar-Patristic Reading of Scriptural Authority (Eastern Orthodox Tradition)").
narrative_ontology:topic_domain(biblical_authority__conciliar_reading, "theology/religious_studies/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__conciliar_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__conciliar_reading, 'd22d6347-5156-44fb-99fa-9b854708fc54').
narrative_ontology:cs_kernel_codification('d22d6347-5156-44fb-99fa-9b854708fc54', distributed).
narrative_ontology:cs_authority_grounding('d22d6347-5156-44fb-99fa-9b854708fc54', practice).
narrative_ontology:cs_interpretation_layer_present('d22d6347-5156-44fb-99fa-9b854708fc54').
narrative_ontology:cs_reading_relation('d22d6347-5156-44fb-99fa-9b854708fc54', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('d22d6347-5156-44fb-99fa-9b854708fc54', biblical_authority__tradition_scripture_reading, coexists_with).
narrative_ontology:cs_axiom('d22d6347-5156-44fb-99fa-9b854708fc54', foundational, tradition_is_living_continuity_not_decree).
narrative_ontology:cs_axiom_status(tradition_is_living_continuity_not_decree, holdable).
narrative_ontology:cs_axiom_grounding('d22d6347-5156-44fb-99fa-9b854708fc54', tradition_is_living_continuity_not_decree, conventional).
narrative_ontology:cs_axiom('d22d6347-5156-44fb-99fa-9b854708fc54', foundational, conciliar_consensus_requires_no_single_arbiter).
narrative_ontology:cs_axiom_status(conciliar_consensus_requires_no_single_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('d22d6347-5156-44fb-99fa-9b854708fc54', conciliar_consensus_requires_no_single_arbiter, conventional).
narrative_ontology:cs_axiom('d22d6347-5156-44fb-99fa-9b854708fc54', secondary, patristic_witness_binds_across_autocephalous_sees).
narrative_ontology:cs_axiom_status(patristic_witness_binds_across_autocephalous_sees, holdable).
narrative_ontology:cs_axiom_grounding('d22d6347-5156-44fb-99fa-9b854708fc54', patristic_witness_binds_across_autocephalous_sees, conventional).
narrative_ontology:cs_reference_frame('d22d6347-5156-44fb-99fa-9b854708fc54', seven_ecumenical_councils_consensus).
narrative_ontology:cs_drift_state('d22d6347-5156-44fb-99fa-9b854708fc54', post_schism_autocephalous_fragmentation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d22d6347-5156-44fb-99fa-9b854708fc54', '').
narrative_ontology:cs_kernel_id(biblical_authority__conciliar_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, episcopal_collegiality).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, patristic_scholars).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, autocephalous_hierarchs).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, reform_minded_clergy).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, laity_seeking_doctrinal_adaptation).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, diaspora_communities_across_jurisdictions).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, consensus_patrum_doctrine).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, conciliar_infallibility_of_ecumenical_councils).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and patriarchs of self-governing churches administer doctrine through synods, adjudicating what counts as authentic continuity with the seven ecumenical councils and the patristic consensus. They convene councils, certify canonization, and rule on liturgical and doctrinal questions within their jurisdiction. They benefit from the collegial structure that distributes authority among peer sees rather than concentrating it in one see, preserving their own standing.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, autocephalous_hierarchs, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% The principle that no single bishop or see holds supreme jurisdictional authority; doctrinal legitimacy flows from conciliar consensus among equals. This structure is not itself an actor but the arrangement that autocephalous hierarchs collectively administer and from which their standing derives.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, episcopal_collegiality, beneficiary,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(biblical_authority__conciliar_reading, episcopal_collegiality).

% Theologians and monastics whose authority rests on demonstrated fidelity to the Church Fathers. They gatekeep what counts as authentic patristic consensus, training clergy and adjudicating theological disputes by appeal to the Fathers rather than to a single magisterial office. Their expertise is the currency of the system; leaving the interpretive framework means losing professional and spiritual standing.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, patristic_scholars, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, patristic_scholars, agenda_setter).

% Priests and lower clergy who wish to address contemporary pastoral problems (remarriage, bioethics, liturgical language, ecclesial governance) find that any proposed adaptation must first survive an appeal to conciliar precedent and patristic consensus, which functions as a de facto veto on rapid change. They can be marginalized, denied advancement, or accused of innovation (a serious charge) if they push too hard; formal exit means leaving Orthodoxy itself.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, reform_minded_clergy, payer,
    moderate, biographical, constrained, national).

% Ordinary believers whose lived circumstances (intercommunion in mixed marriages, divorce, participation in modern institutions) run ahead of what conciliar consensus has settled. They experience the slowness of doctrinal development as a cost borne individually; their only real exits are quiet non-compliance, jurisdiction-shopping among autocephalous churches, or departure from the Church.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, laity_seeking_doctrinal_adaptation, payer,
    powerless, biographical, constrained, regional).

% Orthodox emigrant communities fall under overlapping and sometimes competing jurisdictions of different autocephalous churches (Greek, Russian, Antiochian, and others) in the same city. The absence of a single magisterial authority to resolve overlapping claims means jurisdictional disputes are settled by inter-patriarchal politics far from where the communities actually live, and ordinary parishioners have no standing to intervene.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, diaspora_communities_across_jurisdictions, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, diaspora_communities_across_jurisdictions, excluded).

% The Catholic magisterial reading (the sibling tradition_scripture_reading constraint) is structurally excluded from adjudicating this reading's disputes: from the conciliar perspective, papal supremacy asserted after the schism has no standing to resolve questions the seven councils already settled collegially. Their objections to the lack of a final arbiter are heard as evidence of an illegitimate innovation, not engaged as a live counter-claim within this framework.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, roman_catholic_magisterium, excluded,
    institutional, civilizational, analytical, global).

% Scholars outside any single confession who study how conciliar consensus actually forms and dissolves across history, documenting when 'living tradition' functioned as genuine doctrinal development versus when it functioned as institutional inertia dressed as continuity.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, ecumenical_and_comparative_theologians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, trans-jurisdictional standard for what counts as authentic Christian doctrine — appeal to the ecumenical councils and patristic consensus lets geographically dispersed, self-governing churches recognize each other as orthodox without a single centralized office, preventing doctrinal fragmentation across a communion with no pope.
% TRANSFER_FUNCTION: Moves interpretive authority and the power to certify legitimate doctrine to hierarchs and patristic scholars who can speak fluently in conciliar and patristic terms, at the cost of pastoral responsiveness for clergy and laity whose lived circumstances outrun what has been conciliarly settled; also moves practical dispute-resolution power to inter-patriarchal politics rather than to a single accountable authority, at the cost of coherent governance for diaspora communities.
% ABSENT_VOICES: Laity in pastorally difficult situations (mixed marriages, remarriage, bioethical dilemmas) rarely have standing in synodal deliberation; diaspora parishioners caught between competing jurisdictions have no forum to be heard by the patriarchs whose politics determines their situation. The Catholic magisterial reading and Protestant sola scriptura readings are excluded as illegitimate framings rather than engaged as live alternatives.
% DISAPPEARANCE_RATIONALE: If conciliar-patristic authority vanished overnight, the mechanism that lets autocephalous churches recognize each other as orthodox without a pope would disappear, and each local church would need either a new coordinating mechanism or would fragment into mutually unrecognized bodies; patristic scholars would lose their gatekeeping function; reform-minded clergy and laity would gain much faster paths to doctrinal change, but at the cost of doctrinal coherence across the communion.
% FOUNDING_PROBLEM: After the fall of a unified imperial church structure and especially after the schism with Rome, Orthodox churches needed a way to maintain doctrinal unity across politically independent, self-governing sees without submitting to a single supreme bishop — the ecumenical councils and patristic consensus provided a shared reference point that did not require centralized magisterial authority.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox hierarchs and patristic scholars attest the founding problem remains live — communion coherence across autocephalous churches genuinely requires a shared doctrinal reference. Independent comparative theologians and historians of Christianity attest that the specific mechanism has calcified in places: some doctrinal questions the councils never addressed (because the circumstances did not exist in the patristic era) are treated as already settled by extension of consensus, which functions less as genuine continuity and more as a foreclosure device against contemporary pastoral pressure. Reform-minded clergy corroborate this reading from inside the tradition, at professional risk to themselves.
narrative_ontology:disappearance_verdict(biblical_authority__conciliar_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__conciliar_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__conciliar_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__conciliar_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__conciliar_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__conciliar_reading_tests).
:- end_tests(biblical_authority__conciliar_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored moderate (0.42 at interval end) — lower than a magisterial or papal reading would be, because authority is distributed across peer sees rather than concentrated in a single office capable of extracting uniform rents. But it is not negligible: patristic scholars and hierarchs still gatekeep legitimate interpretation, and that gatekeeping has a real cost borne by those whose pastoral situations move faster than conciliar consensus. Suppression is authored moderate (0.38) reflecting that dissenting clergy face professional and reputational consequences (accusations of 'innovation') rather than the harder coercive suppression a centralized magisterium with disciplinary machinery could apply. Theater ratio rises over the interval (0.22 to 0.40) reflecting that appeals to 'the consensus of the Fathers' increasingly function as a rhetorical foreclosure device on questions the patristic era never actually addressed, rather than as genuine engagement with inherited doctrine — a drift the corroboration in six_questions documents directly.
 *
 * PERSPECTIVAL GAP:
 *   From the hierarch/scholar seat, the conciliar-patristic mechanism is living continuity — an organic, non-coercive unfolding of what the Church has always believed, verified by the collegial consent of many independent sees rather than dictated. From the reform-minded clergy and laity seat, the same mechanism operates as a slow-motion veto: any proposed doctrinal or pastoral adaptation must survive an appeal to a a body of precedent that cannot itself be consulted, decided instead by whoever currently controls the interpretation of what the Fathers 'really' meant. The engine's per-seat computation should register this divergence rather than resolve it toward either seat's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Autocephalous hierarchs and patristic scholars sit near the beneficiary end: they administer and are credentialed by the very interpretive apparatus that grants their authority, and their exit options (arbitrage for hierarchs across sees, constrained-but-substantial standing for scholars) reflect low structural cost to them. Reform-minded clergy and laity sit toward the target end: they bear the transfer (slow adaptation, professional risk, jurisdiction-shopping as their only informal exit) without commensurate voice in the councils that would need to convene to change anything. Diaspora communities are directionality-extreme: trapped exit options and powerless status combine with the structural fact that the dispute-resolution mechanism (inter-patriarchal politics) operates entirely above their level.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — maintaining doctrinal coherence across politically independent sees without a supreme bishop — remains genuinely live in the sense that Orthodox communion coherence is a real, unsolved coordination problem this mechanism addresses better than the alternatives available in its historical context. But founding_problem_status is authored 'contested' rather than 'live' cleanly, because the mechanism now also functions to foreclose contemporary pastoral questions by treating silence in the patristic record as settled doctrine — a mandatrophy-adjacent drift where the coordination function (mutual recognition across sees) persists while an extraction function (blocking adaptation the founders never contemplated) has grown alongside it. This is precisely why the classification lands as tangled_rope rather than a clean rope or a pure snare: both the genuine coordination and the asymmetric cost are real and simultaneous.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conciliar_consensus_genuine_vs_retrospective,
    'Is ''the consensus of the Fathers'' a genuine, discoverable historical fact that the councils recognized, or is it substantially constructed retrospectively by whichever hierarchs and scholars currently control the narrative of what counts as patristic?',
    'Historical-critical examination of whether patristic sources show genuine doctrinal unanimity on contested questions at the time, versus later theological schools projecting unanimity backward onto a more divided historical record.',
    'If largely genuine, the coordination function is real and extraction is closer to incidental overhead of legitimate gatekeeping. If substantially retrospective construction, the ''living tradition'' framing is closer to a legitimation device for present-day episcopal authority, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conciliar_consensus_genuine_vs_retrospective, empirical, 'Whether patristic consensus is discovered or retrospectively constructed.').

omega_variable(
    distributed_authority_natural_vs_constructed,
    'Is the absence of a single magisterial arbiter a theologically necessary feature of authentic apostolic ecclesiology (as the tradition claims), or a historically contingent outcome of the schism and subsequent political fragmentation that has since been retroactively theologized as principled?',
    'Comparative ecclesiological and historical analysis of pre-schism conciliar practice versus post-schism Orthodox self-understanding of collegiality.',
    'If theologically necessary, episcopal collegiality is closer to a genuine coordination feature; if historically contingent and retroactively rationalized, the beneficiary structure (episcopal collegiality) looks more like an extraction-preserving story dressed as principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributed_authority_natural_vs_constructed, conceptual, 'Whether distributed episcopal authority is principled or a rationalized historical accident.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does this reading''s disagreement with tradition_scripture_reading and sola_scriptura_reading actually live — is it about WHO holds interpretive authority (person/office), or about WHAT KIND of thing ''tradition'' is (a living organic process versus a codified deposit versus an unnecessary intermediary)?',
    'Systematic comparison of the three readings'' foundational axioms against how each functions in actual doctrinal disputes (e.g., divorce, contraception, ordination questions) across the three traditions.',
    'If the disagreement is primarily about WHO (office), the readings are more substitutable and coexist as competing power structures. If primarily about WHAT KIND of thing tradition is, the disagreement is deeper and the readings are less commensurable even in principle — bearing on how strongly the reading_relations should be typed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether kernel disagreement is located in authority structure or in the concept of tradition itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__conciliar_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__conciliar_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(bibl_tr_t20, biblical_authority__conciliar_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(bibl_tr_t40, biblical_authority__conciliar_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(bibl_tr_t60, biblical_authority__conciliar_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement(bibl_tr_t80, biblical_authority__conciliar_reading, theater_ratio, 80, 0.37).
narrative_ontology:measurement(bibl_tr_t100, biblical_authority__conciliar_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__conciliar_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bibl_be_t20, biblical_authority__conciliar_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(bibl_be_t40, biblical_authority__conciliar_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(bibl_be_t60, biblical_authority__conciliar_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(bibl_be_t80, biblical_authority__conciliar_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(bibl_be_t100, biblical_authority__conciliar_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__conciliar_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(bibl_su_t20, biblical_authority__conciliar_reading, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(bibl_su_t40, biblical_authority__conciliar_reading, suppression_requirement, 40, 0.34).
narrative_ontology:measurement(bibl_su_t60, biblical_authority__conciliar_reading, suppression_requirement, 60, 0.35).
narrative_ontology:measurement(bibl_su_t80, biblical_authority__conciliar_reading, suppression_requirement, 80, 0.37).
narrative_ontology:measurement(bibl_su_t100, biblical_authority__conciliar_reading, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__conciliar_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_authority__conciliar_reading, 0.1).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, tradition_scripture_reading).

% DUAL FORMULATION NOTE:
% Part of the biblical_authority kernel family (3 readings). conciliar_reading (this story) sits structurally between the other two: it shares with tradition_scripture_reading the commitment that Scripture requires authoritative extra-textual interpretation, but rejects the centralized, standing magisterium tradition_scripture_reading requires, distributing authority instead among autocephalous sees via periodic conciliar consensus. It shares with sola_scriptura_reading a rejection of a single living magisterial office, but rejects sola_scriptura_reading's claim that Scripture is self-interpreting without need of any authoritative extra-textual reference point. Each reading carries its own ε and stakeholder structure; do not average or resolve the contest inside any one file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
