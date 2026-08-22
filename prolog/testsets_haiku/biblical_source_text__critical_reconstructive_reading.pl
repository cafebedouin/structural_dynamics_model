% ============================================================================
% CONSTRAINT STORY: biblical_source_text__critical_reconstructive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__critical_reconstructive_reading, []).

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
 *   constraint_id: biblical_source_text__critical_reconstructive_reading
 *   human_readable: Critical Reconstructive Reading of Biblical Source Text
 *   domain: religious/academic
 *
 * SUMMARY:
 *   The critical reconstructive reading of biblical source text is ONE
 *   reading of a contested kernel: the question of how to establish,
 *   interpret, and authorize the biblical texts themselves. This reading
 *   claims that historical recovery of the hypothetical original (Urtext) is
 *   methodologically primary; neither the textual structure nor the meaning
 *   of any transmitted text can be established until the earliest recoverable
 *   text-state is reconstructed through eclectic analysis of manuscript
 *   evidence. The reading destabilizes received textual tradition and
 *   redistributes hermeneutical authority toward academic expertise, creating
 *   high extractiveness for confessional communities (whose textual
 *   presuppositions are invalidated) while conferring low extractiveness on
 *   academic readers (whose professional identity and institutional power are
 *   reinforced). The constraint operates as tangled_rope: genuine
 *   coordination function (systematic textual basis for scholarly
 *   communication) coupled with asymmetric extraction (authority transferred
 *   from religious to academic seats).
 *
 * KEY AGENTS:
 *   - academic_biblical_scholarship: institutional beneficiary and agenda-setter — controls methodological standard and professional credentialing
 *   - confessional_communities: organized victims, identity-locked — theological frameworks presume textual stability; the reading destabilizes this presupposition
 *   - pastoral_practitioners: moderate-power payers — face hermeneutical friction between critical reconstructive findings and pastoral communication needs
 *   - manuscript_tradition_communities: powerful beneficiaries — valorization of earliest witnesses attracts institutional investment
 *   - translation_committees: organized dual-positioned agents — serve both academic and confessional constituencies under tension
 *   - evangelical_traditioners: excluded moderate-power actors — their textual axioms treated as methodological error rather than legitimate hermeneutical stance
 *   - analytical_observer: examines structural asymmetry — the hypothetical Urtext is unobserved; the reading's authority rests on academic institutional consensus, not direct textual access
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, 0.68).
domain_priors:suppression_score(biblical_source_text__critical_reconstructive_reading, 0.72).
domain_priors:theater_ratio(biblical_source_text__critical_reconstructive_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__critical_reconstructive_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__critical_reconstructive_reading, "Critical Reconstructive Reading of Biblical Source Text").
narrative_ontology:topic_domain(biblical_source_text__critical_reconstructive_reading, "religious/academic").

domain_priors:requires_active_enforcement(biblical_source_text__critical_reconstructive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__critical_reconstructive_reading, 'a983399c-be29-4aaf-9051-fc613bb5f405').
narrative_ontology:cs_kernel_codification('a983399c-be29-4aaf-9051-fc613bb5f405', fixed_text).
narrative_ontology:cs_authority_grounding('a983399c-be29-4aaf-9051-fc613bb5f405', lineage).
narrative_ontology:cs_interpretation_layer_present('a983399c-be29-4aaf-9051-fc613bb5f405').
narrative_ontology:cs_reading_relation('a983399c-be29-4aaf-9051-fc613bb5f405', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a983399c-be29-4aaf-9051-fc613bb5f405', biblical_source_text__dynamic_equivalence_reading, coexists_with).
narrative_ontology:cs_axiom('a983399c-be29-4aaf-9051-fc613bb5f405', foundational, historical_recovery_is_methodologically_primary).
narrative_ontology:cs_axiom_status(historical_recovery_is_methodologically_primary, holdable).
narrative_ontology:cs_axiom_grounding('a983399c-be29-4aaf-9051-fc613bb5f405', historical_recovery_is_methodologically_primary, empirically_contingent).
narrative_ontology:cs_axiom('a983399c-be29-4aaf-9051-fc613bb5f405', foundational, earliest_manuscript_evidence_authoritative).
narrative_ontology:cs_axiom_status(earliest_manuscript_evidence_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('a983399c-be29-4aaf-9051-fc613bb5f405', earliest_manuscript_evidence_authoritative, empirically_contingent).
narrative_ontology:cs_reference_frame('a983399c-be29-4aaf-9051-fc613bb5f405', historical_textual_priority_framework).
narrative_ontology:cs_drift_state('a983399c-be29-4aaf-9051-fc613bb5f405', contemporary_identity_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a983399c-be29-4aaf-9051-fc613bb5f405', '2026-06-12T14:37:00Z').
narrative_ontology:cs_kernel_id(biblical_source_text__critical_reconstructive_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, confessional_communities).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, pastoral_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, manuscript_tradition_communities).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, translation_committees).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, translation_committees).
narrative_ontology:constraint_vindicates(biblical_source_text__critical_reconstructive_reading, historical_priority_principle).
narrative_ontology:constraint_vindicates(biblical_source_text__critical_reconstructive_reading, eclectic_method_validity).
narrative_ontology:constraint_vindicates(biblical_source_text__critical_reconstructive_reading, hypothetical_original_recoverable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Disciplines and credentialing biblical studies through universities, journals, and professional organizations. Sets the methodological standard: historical recovery of original text through eclectic analysis of manuscript evidence. Controls what counts as 'rigorous' scholarship and who is credentialed as an expert. Benefits from the authority this method confers on academic expertise and from the exclusion of non-historical reading approaches from credentialed channels. Could modify or replace the method if institutional consensus shifted, but the method currently serves their power interests.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship, beneficiary).

% Religious communities (Catholic, Orthodox, mainline Protestant, evangelical) whose theology, liturgy, and practice presume stable, authoritative biblical texts. Encounter the critical reading's verdict: the texts they treat as authoritative are medieval compilations far removed from originals, no single 'original' can be recovered with certainty, and the canonical boundaries rest on textual accidents. This destabilizes their textual presupposition. Exit is blocked by identity: their doctrinal and spiritual framework is constituted through the received texts; abandoning textual stability means doctrinal reformulation. Are excluded from academic methodology-setting: their hermeneutical approaches (theological reading, confessional presupposition, liturgical authority) are deemed non-historical and therefore out of bounds for credentialed scholarship.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, confessional_communities, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, confessional_communities, excluded).

% Pastors, preachers, and pastoral theologians who work with congregations using sermon texts, liturgy, and teaching grounded in received translations and traditional interpretations. The critical reading's output—radical uncertainty about original text, instability of canonical boundaries, multiplicity of competing scholarly reconstructions—creates pastoral friction. Congregants ask which text to trust; preachers face hermeneutical paralysis (should I teach the received text or acknowledge the critical reconstructions?). They cannot exit because seminary training and theological education increasingly mediate through academic critical approaches; pastoral credibility now depends on some engagement with critical findings.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, pastoral_practitioners, payer,
    moderate, biographical, constrained, national).

% Museums, universities, research institutes, and collectors holding ancient New Testament manuscripts (P45, Codex Sinaiticus, Codex Vaticanus, etc.). The critical reading privileges earliest manuscript evidence and eclectic apparatus work; this valorization attracts funding, research grants, conservation investment, and scholarly attention toward preservation and digitization. Benefits from institutional visibility and resource allocation directed toward ancient manuscript holdings.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, manuscript_tradition_communities, beneficiary,
    powerful, civilizational, mobile, global).

% Major Bible translation projects (ESV, NRSV, NIV, CSB, NLT, etc.) are pressured to incorporate critical reconstructive decisions: choosing between competing manuscript readings, omitting or bracketing interpolations (longer ending of Mark, pericope adulterae, etc.), including textual notes. Serve both academic and confessional constituencies; the reading creates tension between these audiences. Bear the cost of maintaining dual apparatus (critical text notes, textual commentary explaining variants), managing confessional resistance to critical choices, and defending methodology to congregations. Benefits from scholarly credibility and institutional legitimacy conferred by critical engagement; pays through complexity, controversy, and resource expenditure on apparatus and explanation.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, translation_committees, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, translation_committees, beneficiary).

% Communities affirming biblical inerrancy, Received Text (Textus Receptus) authority, or KJV-based textual presuppositions. Structurally excluded from the academic conversation: their foundational textual axiom (the received texts reliably transmit the original; variants are peripheral corruptions to be rejected) is treated as a methodological error or theological bias rather than a legitimate hermeneutical stance grounded in different axioms. Their objections to critical conclusions are framed as non-scholarly rather than as competing readings. Are identity-locked: their entire religious identity and community practice centers on textual tradition they treat as divinely preserved; exit from that presupposition means identity dissolution.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, evangelical_traditioners, excluded,
    moderate, biographical, identity_locked, national).

% Examines the structural dynamics from outside all parties: the critical reading claims to restore the 'historical' ground, but that ground is a hypothesis (the Urtext is unobserved and unobservable). The reading's authority rests on institutional consensus within the academic frame, not on direct access to originals. Can trace how the reading redistributes costs (to confessional communities, whose textual presuppositions are destabilized) and benefits (to academic expertise, whose authority is reinforced). Can measure theater_ratio: how much institutional activity defends the method versus advancing historical recovery?
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, analytical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship).
narrative_ontology:fixing_cost_class(biblical_source_text__critical_reconstructive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of textual instability created by manuscript transmission: different ancient witnesses preserve different readings; systematic historical analysis selects readings and reconstructs an earlier text-state than any surviving manuscript. Enables scholarly communication around a canonical textual basis (the critical edition) rather than accepting multiple received traditions as equally authoritative.
% TRANSFER_FUNCTION: Transfers hermeneutical authority from received textual tradition (embodied in historic translations and confessional communities) to academic historical expertise (credentialed scholars, peer-reviewed textual criticism). Moves the seat of canonical judgment from religious authority structures to academic institutional consensus.
% ABSENT_VOICES: Confessional communities and evangelical traditioners are excluded: their reading methods (theological interpretation, presuppositional fidelity) are not admitted to the academic venue as legitimate hermeneutical approaches. Oral tradition communities, non-Western textual traditions (Syriac, Coptic, Georgian), and non-academic textual scholarship are marginalized. Lay preachers and pastoral practitioners whose work depends on textual stability have minimal voice in method-setting conversations.
% DISAPPEARANCE_RATIONALE: If the critical reconstructive framework disappeared, academic biblical studies would lose its primary methodological anchor; confessional communities would return to received text authority without the destabilizing pressure; pastoral practice would stabilize around local textual traditions; translation committees would simplify to formal equivalence without critical apparatus. The organizational structure of biblical scholarship—journals, universities, credentialing—depends on the framework's reproduction.
% FOUNDING_PROBLEM: Eighteenth and nineteenth-century textual critics observed that New Testament manuscripts preserved variant readings and that no single medieval exemplar could serve as the 'true' text. The founding problem: which reading preserves the original? Which manuscripts are more reliable? Is there a recoverable original at all? Historical-critical method was built to answer these questions systematically.
% FOUNDING_PROBLEM_CORROBORATION: Academic textual scholars attest the founding problem is live: manuscript variation persists and no single witness is complete or perfect. Confessional communities attest the founding problem is resolved by their textual axiom (the received texts reliably transmit the original; variants are peripheral). Independent analysis from historians and philosophers of science identifies the problem as methodologically intractable: the 'original' cannot be definitively recovered because it is not extant and multiple reconstructions remain plausible. No corroboration from outside the academic benefiting parties supports the 'open question' reading; corroboration of the 'problem is solved' reading comes from confessional and evangelical communities whose methodology the frame excludes.
narrative_ontology:disappearance_verdict(biblical_source_text__critical_reconstructive_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__critical_reconstructive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__critical_reconstructive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_source_text__critical_reconstructive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__critical_reconstructive_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__critical_reconstructive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__critical_reconstructive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68 at interval end) reflects high asymmetry in hermeneutical authority transfer: academic seats gain interpretive power and resource allocation, while confessional communities lose textual authority and are pressured to defend received texts against academic dismissal as 'medieval' or 'corrupt.' Suppression (0.72) is higher than extractiveness because the constraint's persistence depends on actively excluding non-historical reading methods (theological, confessional, pastoral) from the academic credentialing venue—not on participant preference but on institutional boundary maintenance. Theater ratio (0.41) is moderate-to-low: the historical-critical method is genuinely productive (produces new textual knowledge, enables comparative analysis), but a substantial share of institutional energy (peer review, policing methodological boundaries, funding allocation) defends disciplinary orthodoxy rather than advancing historical recovery. The measurement series shows extractiveness rising sharply from t=0 to t=20 (the period of critical method's ascendancy in academic institutions), then plateauing after t=30 (institutional stabilization at higher extraction level). Theater ratio shows similar trajectory: functional analysis is high early, then increases as performative boundary-maintenance becomes routine. Suppression rises steadily: institutional mechanisms for excluding non-critical approaches become more sophisticated and entrenched over time.
 *
 * PERSPECTIVAL GAP:
 *   The critical reconstructive reading produces seat-divergent type assignments because its beneficiary (academic institutional authority) and its victims (confessional textual presupposition) experience opposite effects from the same constraint. An academic reader sees coordination and genuine historical progress; a confessional reader sees authority expropriation dressed as methodology. The engine's per-seat computation from power + exit + directionality should capture this divergence. If both seats compute to tangled_rope, the asymmetry is accurately modeled (genuine coordination coupled with extraction). If the academic seat computes to rope and the confessional seat to snare, the constraint is correctly classified as having seat-divergent character.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic biblical scholarship sits as agenda_setter with power=institutional and exit_options=arbitrage: they designed the method, control its reproduction through universities and credentialing, and could choose alternative approaches if the current one ceased to serve their interests. Their directionality (d) is low—the constraint subsidizes them. Confessional communities have power=organized but exit_options=identity_locked: their entire theological, liturgical, and spiritual framework presumes the textual stability that the critical reading destabilizes. Exiting would mean doctrinal reformulation, not merely changing reading methods. Their d is high—the constraint extracts from them. Pastoral practitioners occupy d near 0.6 (moderate-power, constrained exit): they depend on received texts for sermon material and pastoral communication but are increasingly pressured to justify their approach against academic dismissal. Translation committees are genuinely dual-positioned: they benefit from the critical apparatus (scholarly credibility, institutional legitimacy) but pay the cost of managing confessional objection and maintaining dual textual bases. Their d reflects this: around 0.45 (slight bias toward payer, because the costs of maintaining alternatives are borne by the committees while the methodological benefits accrue to academic institutional prestige).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (which reading preserves the original? how to systematically handle manuscript variation?) remains live in academic venues but contested in confessional ones. Confessional communities argue the founding problem is resolved by received text authority; academics argue it requires ongoing research. The constraint's persistence is justified by the academic frame as research necessity (the original is still being recovered, new manuscripts still emerge, new analytical methods still refine the text). The pastoral frame treats this justification as performative: the uncertainty is maintained to perpetuate academic authority. The mandatrophy signal: if extractiveness plateaus (as measurements show after t=30) while theater rises or holds steady, the constraint is maintaining extraction even as the coordination problem that justified it is increasingly delegated to non-academic communities. This is the classical pattern of authority capture—the method justifies itself, the institutional benefits persist even as the founding problem's urgency declines for anyone outside the academic beneficiary set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    urtext_recoverability_ambiguity,
    'Can the hypothetical original text (Urtext) be recovered with sufficient confidence to ground textual authority, or is the ''original'' an analytical construct useful for scholarship but not recoverable as a determinate historical fact?',
    'Empirical: discovery of earlier manuscripts or genetic analysis of scribal transmission might establish firmer ground. Conceptual: extended debate within textual criticism about whether any two scholars'' reconstructions converge sufficiently to justify the ''recovered original'' claim.',
    'If the Urtext is genuinely recoverable to useful precision, the critical reading''s methodological priority is justified (historical recovery is primary because possible). If it is fundamentally irrecoverable, the reading''s authority rests on institutional consensus about an unprovable entity, shifting classification toward snare (authority extraction dressed as method).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(urtext_recoverability_ambiguity, empirical, 'Whether the hypothetical original can be recovered with sufficient determinacy to justify its methodological priority.').

omega_variable(
    historical_vs_hermeneutical_primacy,
    'Does establishing historical textual priority (which reading is earlier) automatically determine which reading should govern interpretation and authority, or are historical and hermeneutical claims structurally independent?',
    'Conceptual: distinguish the empirical question (which reading is earlier?) from the normative question (which reading should govern meaning and authority?). They are distinct; no historical fact determines the answer.',
    'If they are independent, the critical reading''s claim that historical priority is ''primary'' is a normative choice (not a methodological necessity), and the constraint is more extractive (choosing an authority basis through institutional consensus rather than historical discovery). If they are linked, the primacy claim is justified and the constraint is more coordinative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_vs_hermeneutical_primacy, conceptual, 'Whether historical priority logically entails hermeneutical or textual authority.').

omega_variable(
    academic_vs_confessional_methodological_legitimacy,
    'Are confessional reading methods (theological presupposition, received text authority, doctrinal consistency) non-scholarly failures, or are they legitimate hermeneutical approaches operating under different foundational axioms?',
    'Examine whether confessional methods produce coherent, internally-consistent textual interpretation. If they do, the academic frame''s exclusion of them is institutional boundary-maintenance (supporting extraction claim); if they do not, the exclusion is justified by superior interpretive yield (supporting coordination claim).',
    'If confessional methods are legitimate, the suppression mechanism (excluding them from academic credentialing) is pure institutional extraction. If they are genuinely inferior, the exclusion is quality-control. The measurement consequence: high suppression + low-quality excluded methods = rent-seeking piton; high suppression + excluded methods producing useful analysis = pure extraction snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(academic_vs_confessional_methodological_legitimacy, conceptual, 'Whether confessional hermeneutical approaches are legitimate or methodologically bankrupt.').

omega_variable(
    kernel_reading_contest_structure,
    'Do the three readings (critical_reconstructive, formal_equivalence, dynamic_equivalence) represent genuinely different choices about which textual property is authoritative, or do they represent different emphases on a unified criterion?',
    'Trace whether a single party could coherently hold more than one reading as governing their textual practice simultaneously. If yes, they are emphases (not alternative constraints); if no, they are distinct commitments (distinct constraints).',
    'If they are emphases, they belong in one constraint story with perspectival sections—three seats experiencing one constraint differently. If they are distinct, each deserves a separate constraint story linked through network.affects_constraints. The prompt specifies decomposition (three separate constraint IDs for three readings), indicating they are distinct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Whether the three readings represent distinct constraints or different perspectives on one.').

omega_variable(
    identity_locked_vs_constrained_exit,
    'Is the confessional community''s attachment to received text-authority genuinely identity-locked (doctrinal self-constitution through textual stability) or constrained-exit (economic/institutional pressure that could be removed)?',
    'Test scenarios: can a confessional community adopt critical reconstructive method while maintaining its doctrinal integrity and liturgical practice? If yes, exit is merely constrained; if no, the identity is fused with the textual presupposition and exit is identity-locked.',
    'If identity-locked, the suppression is internalized as well as structural (the community cannot exit even if institutional barriers were removed). The constraint''s effective extraction is higher because it persists even after external coercion is removed. If merely constrained-exit, removing institutional pressure would allow exit; the suppression is primarily structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_constrained_exit, empirical, 'Whether confessional communities'' attachment to received texts is identity-constituting or institutionally-imposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__critical_reconstructive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__critical_reconstructive_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t5, biblical_source_text__critical_reconstructive_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(bibl_tr_t5, observed).
narrative_ontology:measurement(bibl_tr_t10, biblical_source_text__critical_reconstructive_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(bibl_tr_t10, observed).
narrative_ontology:measurement(bibl_tr_t15, biblical_source_text__critical_reconstructive_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(bibl_tr_t15, observed).
narrative_ontology:measurement(bibl_tr_t20, biblical_source_text__critical_reconstructive_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(bibl_tr_t20, observed).
narrative_ontology:measurement(bibl_tr_t25, biblical_source_text__critical_reconstructive_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(bibl_tr_t25, observed).
narrative_ontology:measurement(bibl_tr_t30, biblical_source_text__critical_reconstructive_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(bibl_tr_t30, observed).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__critical_reconstructive_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(bibl_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t5, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(bibl_be_t5, observed).
narrative_ontology:measurement(bibl_be_t10, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(bibl_be_t10, observed).
narrative_ontology:measurement(bibl_be_t15, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(bibl_be_t15, observed).
narrative_ontology:measurement(bibl_be_t20, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(bibl_be_t20, observed).
narrative_ontology:measurement(bibl_be_t25, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(bibl_be_t25, observed).
narrative_ontology:measurement(bibl_be_t30, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(bibl_be_t30, observed).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(bibl_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t5, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(bibl_su_t5, observed).
narrative_ontology:measurement(bibl_su_t10, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(bibl_su_t10, observed).
narrative_ontology:measurement(bibl_su_t15, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(bibl_su_t15, observed).
narrative_ontology:measurement(bibl_su_t20, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(bibl_su_t20, observed).
narrative_ontology:measurement(bibl_su_t25, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(bibl_su_t25, observed).
narrative_ontology:measurement(bibl_su_t30, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(bibl_su_t30, observed).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(bibl_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__critical_reconstructive_reading, information_standard).
narrative_ontology:boltzmann_floor_override(biblical_source_text__critical_reconstructive_reading, 0.12).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__dynamic_equivalence_reading).

% DUAL FORMULATION NOTE:
% The critical_reconstructive_reading is one reading of the contested kernel biblical_source_text. Two sibling readings exist (formal_equivalence_reading and dynamic_equivalence_reading), each instantiating a different constraint with distinct beneficiary/victim structures and extraction mechanisms. All three readings share the kernel (authoritative biblical texts) but decompose along different axes of priority (historical original, source structure, target communication). The constraint family is linked via network.affects_constraints indicating the conceptual dependency: each reading offers a different answer to 'which textual property grounds authority,' and the choices influence the hermeneutical field available to non-chosen readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
