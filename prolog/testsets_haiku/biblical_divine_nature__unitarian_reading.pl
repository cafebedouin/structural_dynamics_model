% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__unitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__unitarian_reading, []).

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
 *   constraint_id: biblical_divine_nature__unitarian_reading
 *   human_readable: Biblical Divine Nature: Unitarian Reading (Father Alone as God)
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   The unitarian reading of biblical divine nature holds that God is
 *   numerically singular — the Father alone is God in the fullest sense,
 *   while the Son is subordinate (either eternally or by creation) and the
 *   Spirit is contingent. This reading claims fidelity to strict monotheism
 *   and scriptural authority against what it frames as the trinitarian
 *   formula's philosophical intrusion into biblical simplicity. The
 *   constraint describes how this reading operates within Christendom: it
 *   vindicates propositions about divine singularity and subordinationism
 *   that institutional trinitarian authority has suppressed through councils,
 *   creeds, anathema, and exclusion from ecclesiastical office. The reading
 *   instantiates a snare because its persistence depends on active
 *   suppression of alternative readings (trinitarian and modalist), and those
 *   alternatives dominate institutional Christianity, forcing unitarian
 *   practitioners into subordinated, heterodox positions. This is one reading
 *   of the contested kernel 'biblical divine nature' alongside trinitarian
 *   and modalist readings—all three claim the same scriptural texts as their
 *   authority and differ on fundamental interpretation premises.
 *
 * KEY AGENTS:
 *   - unitarian_theological_tradition: Practitioners maintaining the unitarian reading across historical periods (Arian, Unitarian, Socinian, modern Unitarian Universalism communities); holds the reading and bears suppression.
 *   - institutional_trinitarian_hierarchy: Church councils (Nicaea, Constantinople, Chalcedon), magisterial churches (Catholic, Orthodox, Protestant establishment), creedal systems, theological seminaries teaching trinitarian orthodoxy as settled doctrine; enforces suppression.
 *   - scriptural_text: The biblical texts (Matthew 28:19, John 1:1, Colossians 1:15–17, etc.) themselves treated as authoritative kernel; both readings claim to honor the text; dispute is over interpretation, not authority.
 *   - creedal_orthodoxy_enforcement_apparatus: Nicene Creed, formalized doctrine, heresy proceedings, denial of communion and office to unitarian clergy; the machinery that sustains the suppression.
 *   - unitarian_exegetical_community: Scholars and practitioners who actively defend unitarian reading through biblical scholarship; victims of suppression within institutional Christianity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.62).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.71).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, snare).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Biblical Divine Nature: Unitarian Reading (Father Alone as God)").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__unitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, 'd7ace464-cb84-4f30-a9c2-a8cd57fb7177').
narrative_ontology:cs_kernel_codification('d7ace464-cb84-4f30-a9c2-a8cd57fb7177', fixed_text).
narrative_ontology:cs_authority_grounding('d7ace464-cb84-4f30-a9c2-a8cd57fb7177', extraction).
narrative_ontology:cs_interpretation_layer_present('d7ace464-cb84-4f30-a9c2-a8cd57fb7177').
narrative_ontology:cs_reading_relation('d7ace464-cb84-4f30-a9c2-a8cd57fb7177', biblical_divine_nature__trinitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7ace464-cb84-4f30-a9c2-a8cd57fb7177', biblical_divine_nature__modalist_reading, coexists_with).
narrative_ontology:cs_axiom('d7ace464-cb84-4f30-a9c2-a8cd57fb7177', foundational, numerical_monotheism_foundational).
narrative_ontology:cs_axiom_status(numerical_monotheism_foundational, holdable).
narrative_ontology:cs_axiom_grounding('d7ace464-cb84-4f30-a9c2-a8cd57fb7177', numerical_monotheism_foundational, deontological).
narrative_ontology:cs_axiom('d7ace464-cb84-4f30-a9c2-a8cd57fb7177', foundational, father_absolute_uniqueness).
narrative_ontology:cs_axiom_status(father_absolute_uniqueness, holdable).
narrative_ontology:cs_axiom_grounding('d7ace464-cb84-4f30-a9c2-a8cd57fb7177', father_absolute_uniqueness, empirically_contingent).
narrative_ontology:cs_reference_frame('d7ace464-cb84-4f30-a9c2-a8cd57fb7177', biblical_radical_monotheism).
narrative_ontology:cs_drift_state('d7ace464-cb84-4f30-a9c2-a8cd57fb7177', post_nicene_creedal_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d7ace464-cb84-4f30-a9c2-a8cd57fb7177', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, unitarian_theological_tradition).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, institutional_trinitarian_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, creedal_orthodoxy_enforcement_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, unitarian_theological_tradition).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, unitarian_exegetical_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces trinitarian orthodoxy through ecumenical councils (Nicaea, Constantinople, Chalcedon), magisterial churches (Catholic, Orthodox, Reformed), creedal systems, theological seminaries, and ordination gatekeeping. Justifies trinitarian formula as the coherent resolution of biblical witness and philosophical rigor. Collects doctrinal authority, institutional legitimacy, and unified ecclesiology from the constraint. Could revise doctrine (power is real, exit is mobile) but does not, because trinitarian orthodoxy is now institutional identity.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, institutional_trinitarian_hierarchy, agenda_setter,
    institutional, generational, mobile, global).

% Maintains the unitarian reading across Arian, Unitarian, Socinian, and modern Unitarian Universalism traditions. Bears the cost of institutional marginalization (denied mainstream pulpits, theological legitimacy, institutional recognition). Benefits by preserving what they believe is the scripturally grounded, hermeneutically honest reading of monotheism. Cannot exit by simply adopting trinitarian reading without violating their hermeneutic integrity; exit from Christianity itself is possible but high-cost. Within institutional Christianity, trapped in subordinated heterodox status; outside it (independent denominations), mobile and self-governing.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_theological_tradition, payer,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__unitarian_reading, unitarian_theological_tradition, beneficiary).

% The formalized machinery of doctrine enforcement: Nicene Creed recitations, heresy proceedings, seminary gatekeeping, denial of communion and ordination to unitarian clergy, anathema declarations, doctrinal policing in councils and synods. The apparatus is trapped in its function—it exists to defend trinitarian orthodoxy and cannot change doctrine without dissolving its own legitimacy. Enforces suppression through institutional mechanisms that have become self-perpetuating.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, creedal_orthodoxy_enforcement_apparatus, agenda_setter,
    institutional, generational, trapped, universal).

% The biblical texts (Matthew 28:19, John 1:1, Colossians 1:15–17, 1 Corinthians 8:6, 1 Timothy 2:5, etc.) treated as the ultimate authority by all three readings. The text does not benefit or pay; it is the ground both readings claim to honor. All interpretive dispute is about what the text says, not whether it is authoritative. Included as observer seat, not agent.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, scriptural_text, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(biblical_divine_nature__unitarian_reading, scriptural_text).

% Scholars, theologians, and pastors who actively defend the unitarian reading through biblical scholarship, historical research, and theological argument. They bear the cost of marginalization in academic and ecclesiastical institutions (lower career prospects in mainstream seminaries, publishing barriers in establishment journals, ordination denial). They contest the institutional framework that delegitimizes their reading. Their exit options are constrained: they can move to unitarian communities or secular academia, but exit from Christianity entirely means abandoning the hermeneutical practice that defines their intellectual identity.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_exegetical_community, payer,
    organized, biographical, constrained, global).

% Formal assemblies (Nicaea 325, Constantinople 381, Chalcedon 451, etc.) that defined trinitarian orthodoxy through creeds and anathemas against unitarian and other non-trinitarian readings. Their decisions became binding doctrinal law in major Christian traditions. The councils are trapped in their outcomes—revising them would delegitimize the entire magisterial authority structure they established.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, ecumenical_councils, agenda_setter,
    institutional, generational, trapped, universal).

% Platonic, Stoic, and Neoplatonic philosophical frameworks that both trinitarian and unitarian readings invoke to interpret scripture. Excluded from authoritative voice in the doctrinal contest because only Christian (scriptural-fidelity) frameworks are admitted. The constraint's enforceability depends on this exclusion—if secular or non-Christian philosophy were granted legitimacy in interpreting the texts, the trinitarian formula's philosophical scaffolding would be subject to external critique.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, competing_hellenistic_philosophical_frameworks, excluded,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(biblical_divine_nature__unitarian_reading, competing_hellenistic_philosophical_frameworks).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__unitarian_reading, institutional_trinitarian_hierarchy).
narrative_ontology:fixing_cost_class(biblical_divine_nature__unitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the apparent contradiction between strict biblical monotheism (one God) and the New Testament's threefold mention of Father, Son, and Spirit. Trinitarian formula: three persons, one essence (οὐσία/ousia), solved through philosophical distinction between person (ὑπόστασις/hypostasis) and essence. Unitarian reading: rejects the philosophical distinction as extra-scriptural; claims strict numerical monotheism (Father alone is God; Son and Spirit are subordinate) is the only coherent reading that preserves both the text and logical consistency.
% TRANSFER_FUNCTION: Transfers doctrinal legitimacy, institutional authority, and ecclesiastical position from unitarian readings to trinitarian readings. Unitarian exegetes transfer intellectual labor (biblical scholarship defending their reading) and credibility (their hermeneutic work is cited, even when rejected) to the institutional apparatus that denies them legitimacy. The constraint moves authority FROM scriptural fidelity AS DEMONSTRATED (unitarian exegesis often displays meticulous textual work) TO institutional creedal definition AS PERFORMED (trinitarian formula recited, taught, defended regardless of textual ambiguity). Unitarian communities transfer resources to maintaining their separate ecclesiology outside mainstream Christianity; mainstream Christianity transfers resources to doctrinal gatekeeping that excludes unitarian voices.
% ABSENT_VOICES: Modalist readings (Father/Son/Spirit as sequential modes of one person, not simultaneous persons) are excluded from mainstream trinitarian discourse despite being a live historical and scholarly position. Biblical voices emphasizing Father's unique status or the Son's subordination are reinterpreted through trinitarian lens rather than heard on their own terms. Jewish and Islamic monotheistic critics of trinitarian formula are excluded from doctrinal forums (Christian councils could not include non-Christian authority). Modern secular hermeneutics and comparative religion scholars who challenge the assumption that the kernel texts demand a single unified reading are excluded from magisterial doctrinal forums. Unitarian practitioners are present but voiceless—they are not in the rooms where doctrine is authoritatively taught (seminaries, councils, ordination boards).
% DISAPPEARANCE_RATIONALE: If the unitarian constraint disappeared (i.e., if unitarian readings were no longer suppressed and could circulate as live doctrinal options in mainstream Christianity), Christian institutional identity would reorganize: seminaries would teach biblical exegesis that admits unitarian as well as trinitarian readings; ordination would not require trinitarian creedal assent; the unified doctrinal structure sustained by trinitarian consensus would fracture into acknowledged plurality; ecumenical unity would fragment further (some churches would embrace unitarian biblicism, others would defend trinitarian formula on philosophical or traditional grounds). Doctrinal authority would shift from institutional decree (creed) to textual persuasion (exegesis). The constraint's disappearance is unlikely precisely because the institutional investments in trinitarian orthodoxy are deep (1,700 years of magisterial authority, built-in institutional identity). But if it vanished, Christian institutional life would be fundamentally different.
% FOUNDING_PROBLEM: Early Christian witness contained apparent tensions: Jesus subordinates himself to the Father (John 14:28, Mark 13:32), yet is called divine (John 1:1, Colossians 1:15); the Spirit is named alongside Father and Son but with ambiguous ontological status (Matthew 28:19); strict biblical monotheism (Deuteronomy 6:4, Isaiah 45:5) seems to contradict trinitarian language. How to affirm biblical monotheism AND the full divinity of the Son AND the work of the Spirit? This was the founding problem.
% FOUNDING_PROBLEM_CORROBORATION: Unitarian exegetes (both historical and contemporary) attest the founding problem is live: biblical texts do admit unitarian reading and serious scholars disagree on whether trinitarian philosophy resolves or obscures the texts. Institutional trinitarian authorities attest the problem is solved by the trinitarian formula. Modern biblical scholars outside the theological establishment (secular exegetes, scholars in Jewish and Islamic traditions) attest that the texts contain genuine tension and that the trinitarian solution is one philosophical choice, not a necessary deduction from the texts. Historical scholarship on the development of trinitarian doctrine (from Constantine to Chalcedon) demonstrates the problem WAS live and contested before trinitarian dominance; the problem is declared dead only by institutional fiat, not by textual resolution. Unitarian communities continue to exist and produce scholarly work defending their reading, which attests the problem remains live for them.
narrative_ontology:disappearance_verdict(biblical_divine_nature__unitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__unitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_divine_nature__unitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__unitarian_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__unitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__unitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 to 0.62 over the interval, reflecting the increasing dominance of trinitarian orthodoxy and the correspondingly deepening subordination of unitarian readings within institutional Christianity. Early in the interval (t=0), unitarian exegesis retained some scholarly voice and doctrinal diversity; by t=12, trinitarian triumphal consensus has become near-total in mainstream Christianity, forcing unitarian positions into marginal, heterodox space. Theater ratio rises from 0.18 to 0.44, indicating that enforcement activity shifts from genuine doctrinal dispute (when unitarian and trinitarian readings contended as live alternatives) to theatrical maintenance of creedal orthodoxy (when the outcome is institutionally settled but must be defended ritually against periodic challenge). Suppression requirement (0.71 by end) reflects the institutional machinery required to keep unitarian readings out of mainstream pulpits, seminaries, and communion—suppression is structural (institutional barriers) and internalized (unitarian practitioners absorbed the identity of heterodoxy). The measured extractiveness (0.62) captures the constraint's function: the trinitarian formula extracts credibility, institutional position, and doctrinal authority from the unitarian reading, funneling it to the institutional hierarchy. The claimed type is snare because persistence depends on active enforcement (creedal definition, exclusion from office, anathema) and the reading has identifiable victims (unitarian practitioners, scholars, communities denied institutional legitimacy). The claim/metric independence is deliberate: snare is authored as the structural type; the metrics document the actual operation.
 *
 * PERSPECTIVAL GAP:
 *   The gap is maximal (mountain-to-snare range). Trinitarian institutional actors frame this as doctrinal truth-enforcement (the constraint teaches real doctrine). Unitarian exegetes frame it as institutional suppression of hermeneutic diversity (the constraint excludes live readings through power, not argument). The measured suppression (0.71) is high because the constraint operates through multiple mechanisms: formal exclusion (creedal definition), institutional exclusion (denial of office), social exclusion (heresy designation), and internalized suppression (unitarian practitioners absorb minority status and lower institutional legitimacy as part of their identity).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the institutional trinitarian hierarchy: d ≈ 0.1 (beneficiary, high power, mobile exit—the hierarchy can change doctrine if it chooses but does not). Directionality for unitarian practitioners: d ≈ 0.85 (target, moderate to powerless power within Christian institutions, identity_locked exit—unitarian exegetes cannot unsee what the texts say to them without violating their hermeneutic integrity; exit from Christianity entirely is possible but high-cost). Directionality for the scriptural text: not an agent; included in vindicated_propositions (the text's interpretation is contested, not who benefits from its truth). The constraint's directionality is asymmetric because power, exit options, and beneficiary/victim status differ sharply between seats. The trinitarian hierarchy benefits and excludes; unitarian practitioners bear the suppression and remain within the conversation only as heterodox voices.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows signs of mandatrophy: the founding problem (what does strict biblical monotheism require?) remains contested at the scholarly level, but the institutional mandate (trinitarian formula as settled orthodoxy) has calcified and now persists partly by inertia and ritual enforcement. The rising theater_ratio (0.18→0.44) indicates increasing performative maintenance: modern Christian institutions perform trinitarian doctrine in creed recitations and seminaries without actively fighting live unitarian alternatives (they simply do not appear in mainstream pulpits). However, the constraint is NOT yet a full piton because unitarian communities remain organized and articulate (Unitarian Universalism, scholarly exegetical communities), and suppression remains active (doctrinal gatekeeping in seminaries and ordination). The constraint is a snare in transition toward piton: extractiveness is high (institutional authority accrues to trinitarian readings, unitarian readings are systematically excluded), but the resistance (0.58) from unitarian practitioners is building through scholarly work and the rise of non-institutional Christian communities, which may eventually shift the constraint's character. Declare mandatrophy_resolved: the institutional mandate (teach trinitarian doctrine) has outlived the function it was built for (resolve doctrinal chaos and unite the church)—modern Christianity remains divided on many doctrines and now uses trinitarian orthodoxy as a marker of institutional legitimacy rather than a solution to biblical ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the unitarian reading of biblical divine nature a coherent theological position within the same hermeneutical tradition as trinitarian and modalist readings, or does it foreclose those readings within a single Christian framework?',
    'Historical-comparative analysis of whether unitarian, trinitarian, and modalist exegetes claim to be reading the same texts under the same normative authority structure, or whether they have split on fundamental authority/interpretation premises that make coexistence impossible.',
    'If coherent coexistence in one tradition: the readings coexist_with each other and the constraint is contestation among live interpretations. If split on authority: the readings foreclose each other and the constraint describes a capture/zombie dynamic where one reading suppresses the others'' legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the unitarian reading forecloses trinitarian/modalist premises or coexists with them as rival live interpretations.').

omega_variable(
    suppression_institutional_or_textual,
    'Is the measured suppression (0.71) driven by institutional enforcement machinery (councils, creeds, hierarchy defending orthodoxy against heterodoxy) or by textual-interpretive lockdown (the reading itself claims scriptural texts permit only unitarian reading, and trinitarian readings are exegetical errors)?',
    'Examine whether unitarian exegetes present their reading as the only honest reading of scripture (textual suppression: high confidence in their own hermeneutic), or whether they frame trinitarian dominance as institutionally imposed despite textual ambiguity (institutional suppression: competing readings remain live at the text level).',
    'Institutional suppression: the constraint is a snare sustained by power asymmetry and exclusion, victims are the subordinated readings and their practitioners. Textual suppression: the constraint describes a contested exegesis where the unitarian reading claims the text forecloses alternatives. The classification does not change, but the localization of suppression does.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_institutional_or_textual, empirical, 'Whether suppression operates through institutional force or through hermeneutic claim of textual uniqueness.').

omega_variable(
    beneficiary_identification,
    'Who actually benefits from the unitarian reading''s persistence: practitioners of unitarian theology (real historical actors), or the reading itself as a vindicated proposition claiming truth-status regardless of who holds it?',
    'Distinguish between communities that actively maintain and defend the unitarian reading (Arian, Unitarian, Socinian traditions, modern Unitarian Universalism) and abstract propositions the reading vindicates. The former are stakeholders with agency and interest; the latter are non-agents whose ''benefit'' is merely that they are claimed true.',
    'If beneficiary = the reading/proposition: reassign from beneficiaries[] to vindicated_propositions[]; the constraint is a struggle over which propositions are credible, not who collects. If beneficiary = the communities: the constraint is a real institutional struggle and remains correctly classified as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification, conceptual, 'Whether the constraint''s beneficiary is a real historical community or an abstract proposition.').

omega_variable(
    founding_problem_live_or_dead,
    'The founding problem (biblical monotheism vs. trinitarian formula) is declared contested in six_questions. Is the problem actually live in contemporary Christian theology, or do different traditions treat it as settled (and just in opposite ways)?',
    'Survey of contemporary theological scholarship: do major Christian traditions continue to dispute whether Matthew 28:19, John 1:1, Colossians 1:15–17, and other key texts demand unitarian, trinitarian, or modalist reading? Or has trinitarian reading achieved near-universality in institutional Christianity, leaving unitarian positions as relic traditions?',
    'If settled in favor of trinitarian consensus: the founding problem is dead (monopoly reading achieved), the constraint persists as performative maintenance (piton) rather than active snare. If genuinely contested: the constraint remains snare, with real suppression of live alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_live_or_dead, empirical, 'Whether the doctrinal dispute over divine nature is live or settled by institutional consensus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__unitarian_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(bibl_tr_t3, biblical_divine_nature__unitarian_reading, theater_ratio, 3, 0.26).
narrative_ontology:measurement(bibl_tr_t6, biblical_divine_nature__unitarian_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement(bibl_tr_t9, biblical_divine_nature__unitarian_reading, theater_ratio, 9, 0.39).
narrative_ontology:measurement(bibl_tr_t12, biblical_divine_nature__unitarian_reading, theater_ratio, 12, 0.44).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__unitarian_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bibl_be_t3, biblical_divine_nature__unitarian_reading, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(bibl_be_t6, biblical_divine_nature__unitarian_reading, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(bibl_be_t9, biblical_divine_nature__unitarian_reading, base_extractiveness, 9, 0.57).
narrative_ontology:measurement(bibl_be_t12, biblical_divine_nature__unitarian_reading, base_extractiveness, 12, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__unitarian_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(bibl_su_t3, biblical_divine_nature__unitarian_reading, suppression_requirement, 3, 0.61).
narrative_ontology:measurement(bibl_su_t6, biblical_divine_nature__unitarian_reading, suppression_requirement, 6, 0.66).
narrative_ontology:measurement(bibl_su_t9, biblical_divine_nature__unitarian_reading, suppression_requirement, 9, 0.69).
narrative_ontology:measurement(bibl_su_t12, biblical_divine_nature__unitarian_reading, suppression_requirement, 12, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__unitarian_reading, 0.12).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__modalist_reading).

% DUAL FORMULATION NOTE:
% The biblical divine nature kernel decomposes into three constraint stories representing three readings: unitarian (this file), trinitarian, and modalist. Each reading claims fidelity to the same scriptural texts but instantiates a different constraint with different ε, beneficiary/victim structures, and institutional trajectories. The readings are linked because institutional dominance of one reading suppresses the others—trinitarian reading's institutional triumph creates the snare structure for the unitarian reading. All three stories must be authored to model the full doctrinal contest; a single unified 'divine nature' constraint would falsify the observables (which reading dominates, how suppression operates per reading, what each reading's actual institutional position is).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_divine_nature__unitarian_reading, institutional, 0.15).
constraint_indexing:directionality_override(biblical_divine_nature__unitarian_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
