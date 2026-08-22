% ============================================================================
% CONSTRAINT STORY: biblical_authority__sola_scriptura_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: biblical_authority__sola_scriptura_reading
 *   human_readable: Sola Scriptura: Scripture as Sufficient and Self-Interpreting Authority
 *   domain: theological/religious
 *
 * SUMMARY:
 *   Sola scriptura emerged as a Protestant reading of biblical authority in
 *   the 16th-century Reformation, articulated primarily by Luther, Calvin,
 *   and their successors. It claims that Scripture is the sufficient,
 *   supreme, and self-interpreting authority for Christian doctrine and
 *   practice—no external tradition, magisterial pronouncement, or
 *   ecclesiastical institution can bind doctrine beyond what Scripture states
 *   or clearly implies. This reading directly confronts the medieval Catholic
 *   arrangement where papal magisterium and ecumenical councils possessed
 *   binding interpretive authority over Scripture. The claim is that lay
 *   believers, by reading Scripture directly, can access doctrine without
 *   clerical mediation. The constraint's structure is: Scripture functions as
 *   the sole authority, congregations read it autonomously, and clerical
 *   hierarchy is displaced. However, the actual operation shows substantial
 *   enforcement costs (suppression of Catholic and Orthodox alternatives,
 *   suppression of heterodox lay interpretations, pastoral authority over
 *   congregational readings) and growing theatricality as sola scriptura
 *   becomes institutionalized in Protestant confessions and catecheses—the
 *   principle of lay autonomy is increasingly mediated through reformed
 *   clergy and study-Bible frameworks that shape interpretation.
 *
 * KEY AGENTS:
 *   - lay_believers: primary beneficiary (gain autonomy in belief and practice); located powerless with constrained exit (must remain in congregation to claim the benefit)
 *   - reformed_protestant_clergy: agenda_setter and secondary beneficiary (displace Catholic hierarchy; retain authority as pastors but ground it in scriptural competence rather than apostolic succession)
 *   - roman_catholic_magisterium: primary payer (loses monopoly on binding interpretation); located institutional with identity-locked exit (cannot accept sola scriptura without dissolving the institutional identity)
 *   - doctrinal_coherence_across_communities: abstract victim (fragmentation into Protestant sects)
 *   - ecumenical_councils: institutional payer (downgraded from adjudicative to consultative status)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.38).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.52).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Sola Scriptura: Scripture as Sufficient and Self-Interpreting Authority").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theological/religious").

domain_priors:requires_active_enforcement(biblical_authority__sola_scriptura_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, 'dc86c062-ef81-4d1e-b32d-f15405b006d5').
narrative_ontology:cs_kernel_codification('dc86c062-ef81-4d1e-b32d-f15405b006d5', fixed_text).
narrative_ontology:cs_authority_grounding('dc86c062-ef81-4d1e-b32d-f15405b006d5', lineage).
narrative_ontology:cs_interpretation_layer_present('dc86c062-ef81-4d1e-b32d-f15405b006d5').
narrative_ontology:cs_reading_relation('dc86c062-ef81-4d1e-b32d-f15405b006d5', biblical_authority__tradition_scripture_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc86c062-ef81-4d1e-b32d-f15405b006d5', biblical_authority__conciliar_reading, influences).
narrative_ontology:cs_axiom('dc86c062-ef81-4d1e-b32d-f15405b006d5', foundational, scripture_self_interpreting).
narrative_ontology:cs_axiom_status(scripture_self_interpreting, holdable).
narrative_ontology:cs_axiom_grounding('dc86c062-ef81-4d1e-b32d-f15405b006d5', scripture_self_interpreting, deontological).
narrative_ontology:cs_axiom('dc86c062-ef81-4d1e-b32d-f15405b006d5', foundational, sola_scriptura_sufficient_doctrine).
narrative_ontology:cs_axiom_status(sola_scriptura_sufficient_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('dc86c062-ef81-4d1e-b32d-f15405b006d5', sola_scriptura_sufficient_doctrine, deontological).
narrative_ontology:cs_reference_frame('dc86c062-ef81-4d1e-b32d-f15405b006d5', scriptural_autonomy_unmediated).
narrative_ontology:cs_drift_state('dc86c062-ef81-4d1e-b32d-f15405b006d5', early_modern_to_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dc86c062-ef81-4d1e-b32d-f15405b006d5', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, congregational_autonomy).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, centralized_interpretive_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, reformed_protestant_clergy).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, roman_catholic_magisterium).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, ecumenical_councils).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, priesthood_of_all_believers).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, sola_scriptura_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain direct interpretive access to Scripture and are empowered to read doctrine from the text without requiring priestly mediation or magisterial permission. Their agency is enlarged. But they remain constrained by local congregational boundaries and the practical necessity of pastoral guidance to understand the text. The benefit is real (autonomy) but the exit cost is high (leaving the congregation means losing the community that sustained their belief).
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_believers, beneficiary,
    powerless, biographical, constrained, local).

% Set the sola scriptura agenda through catechesis, pulpit preaching, and church discipline. They benefit from the displacement of papal authority but retain pastoral authority grounded in scriptural competence and persuasiveness. Their authority is now defended by the principle of sola scriptura itself (they are faithful readers, not institutional mediators), which requires them to demonstrate scriptural warrant for every doctrine they teach. They are constrained by the principle they articulate—they cannot claim authority beyond what Scripture supports.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, reformed_protestant_clergy, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, reformed_protestant_clergy, beneficiary).

% The magisterium's identity is built on its authority to interpret Scripture authoritatively and bind conscience through the teaching of the Church. Sola scriptura directly denies this authority—it asserts that Scripture is self-interpreting and that believers can access doctrine without magisterial mediation. The magisterium cannot accept sola scriptura without dissolving the identity that constitutes it as an institution. Its exit is identity-locked: to exit the constraint would be to cease being the magisterium.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, roman_catholic_magisterium, payer,
    institutional, civilizational, identity_locked, global).

% Their authority to pronounce binding doctrine is downgraded under sola scriptura. The councils become advisory bodies whose pronouncements carry weight only insofar as they reflect what Scripture already teaches, not as independent authoritative acts. The early ecumenical councils (Nicaea, Constantinople, Chalcedon) are respected by Protestants as faithful readings of Scripture, but their authority is derivative—Scripture is supreme. Councils cannot exit this constraint without reasserting their own supremacy, which requires rejecting sola scriptura.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, ecumenical_councils, payer,
    institutional, civilizational, constrained, global).

% The mediating power of priests to convey grace through sacraments is undermined. Under sola scriptura, baptism and communion are ordinances (obedient acts commanded by Scripture) rather than salvific channels (means by which grace is conveyed). The shift from sacrament to ordinance removes the priest's unique mediating role. Lay believers cannot be priests; they can read Scripture. This structural change erodes the extraction mechanism of sacramental monopoly.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, clerical_sacramental_authority, payer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(biblical_authority__sola_scriptura_reading, clerical_sacramental_authority).

% Without a binding adjudicative authority to settle interpretive disputes, Christian doctrine fragments. Different congregations and regions read Scripture differently and develop divergent doctrinal traditions. The Reformed, Lutheran, radical Reformation, and later evangelical and fundamentalist strands all claim scriptural warrant but teach different doctrines about predestination, the atonement, sacraments, and church order. Doctrinal unity is lost; the cost is a Christendom divided by competing scriptural readings.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities, payer,
    moderate, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities).

% The patristic and medieval exegetical heritage loses binding status. The Fathers are consulted as wise interpreters, not as authorities whose readings bind conscience. Medieval scholastic theology is optional rather than normative. The cumulative interpretive work of centuries is downgraded to the status of 'helpful context' rather than authoritative guidance. Later Protestant theology (Reformed orthodoxy, Westminster Catechism) attempts to bind interpretation through confessions and catecheses, but these are presented as summaries of Scripture, not as independent authorities.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, interpretive_tradition_patristic_medieval, payer,
    moderate, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(biblical_authority__sola_scriptura_reading, interpretive_tradition_patristic_medieval).

% Lay readers are freed from the requirement to defer to institutional authority in matters of doctrine. They can read Scripture for themselves, discuss it with their congregation, and arrive at their own understanding. This autonomy is genuine—congregations are empowered to set their own doctrine—but it is constrained by the expectation that their reading will remain faithful to the scriptural text and aligned with their congregation's interpretation.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_interpretive_autonomy, beneficiary,
    powerless, biographical, analytical, local).
narrative_ontology:stakeholder_non_agent(biblical_authority__sola_scriptura_reading, lay_interpretive_autonomy).

% Lay readers who interpret Scripture in ways that contradict core doctrine (denying the Trinity, the deity of Christ, the atonement) are structurally excluded from congregational authority even under sola scriptura. Reformed churches enforce orthodoxy through church discipline, catechetical training, and pulpit authority. The principle of sola scriptura is used to suppress heterodox readings—those readings are declared to be unfaithful to Scripture. The autonomy promised by sola scriptura is constrained by orthodoxy enforcement.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, heterodox_lay_interpretations, excluded,
    powerless, biographical, analytical, local).
narrative_ontology:stakeholder_non_agent(biblical_authority__sola_scriptura_reading, heterodox_lay_interpretations).

% Observes the tension between sola scriptura's promise (lay autonomy in doctrine) and its enforcement structure (pastoral and magisterial control disguised as scriptural fidelity). Notes that reformulated clerical authority persists in different form—no longer through Rome, but through Reformed pastors and study-Bible mediations. Asks whether the reading represents genuine liberation or merely displacement of authority.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__sola_scriptura_reading, reformed_protestant_clergy).
narrative_ontology:fixing_cost_class(biblical_authority__sola_scriptura_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of doctrinal authority in the absence of institutional hierarchy: provides a fixed common text (Scripture) that all communities reference, enabling doctrinal conversation without requiring submission to a mediating authority. Congregations coordinate around shared textual reference without needing a supreme interpreter.
% TRANSFER_FUNCTION: Transfers interpretive authority from clerical/institutional monopoly to lay believers and congregations. What moves is the right to bind conscience: no longer flows from Rome or councils to believers, but is claimed to reside in the text itself and each believer's reading of it.
% ABSENT_VOICES: Institutional clergy who benefit from interpretive monopoly and would object to sola scriptura's implementation; Catholic hierarchies defending magisterial authority; voices arguing that lay autonomy requires safeguards (trained hermeneutics, community standards) to prevent error. These voices are excluded from the reading's own institutional home (Protestant congregations) but remain active in Catholic and Orthodox contexts.
% DISAPPEARANCE_RATIONALE: If sola scriptura disappeared and clerical/magisterial interpretive authority were fully restored, Protestant Christianity would reorganize around hierarchical doctrinal authority, sacramental mediation, and apostolic succession. The lay autonomy that Protestant theology grounds in sola scriptura would be lost; doctrinal unity would be reasserted but at the cost of congregational sovereignty. The entire institutional structure of Protestantism depends on this reading persisting.
% FOUNDING_PROBLEM: In medieval Christendom, doctrinal authority was monopolized by the papal magisterium and clerical hierarchy, mediated through Tradition and enforced through sacramental control and institutional discipline. Lay believers had no direct standing to contest doctrine; conscience was bound to institutional pronouncement. The Reformation's founding problem: how can believers know doctrine directly without institutional mediation?
% FOUNDING_PROBLEM_CORROBORATION: Protestant Reformation historians (Heiko Oberman, David Bagchi) document the late-medieval monopoly of interpretive authority by the Roman magisterium and the Reformers' explicit challenge to it. Catholic historians (John O'Malley, Hubert Jedin) and contemporary magisterial teaching attest that institutional mediation and tradition-guided interpretation remain necessary to prevent doctrinal chaos. The dispute is LIVE: Reformation-era conflicts are not settled; contemporary Catholicism and Protestantism remain in structural disagreement about this founding problem's persistence.
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__sola_scriptura_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__sola_scriptura_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__sola_scriptura_reading_tests).
:- end_tests(biblical_authority__sola_scriptura_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the reading genuinely reduces clerical monopoly extraction—lay believers are empowered to read and interpret. But it does NOT eliminate extraction: reformed clergy retain authority grounded now in pastoral competence and scriptural persuasiveness rather than institutional decree. Local magistrates in Protestant territories enforce orthodoxy (though through different mechanisms than Rome). Suppression is substantial (0.52) because the reading requires active suppression of Catholic/Orthodox alternative readings and, increasingly over time, suppression of heterodox lay interpretations (Anabaptists, radicals, sectarians). The measurement series shows DECLINING suppression over 500 years as sola scriptura becomes institutionalized and its alternatives fade in Protestant-dominated regions—but at t=500, suppression persists because doctrinal fragmentation requires pastoral enforcement to maintain coherence within congregations. Theater rises from 0.22 to 0.41 because the principle of lay autonomy becomes increasingly mediated through study Bibles, pastoral guidance, and confessional standards—the performative maintenance of 'autonomy within orthodoxy' grows as the tension between lay freedom and pastoral control becomes visible. The extraction trend (0.75→0.38) reflects the genuine displacement of clerical monopoly extraction, but the floor (0.38) reflects the residual pastoral/magisterial extraction in Protestant contexts.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (Catholic hierarchy, councils, clerical institutions) and the beneficiary seats (lay believers, congregational autonomy) will compute different types from identical structural data. Rome's seat computes toward snare or tangled_rope because the reading actively suppresses Catholic authority and extracts jurisdiction. The lay believer's seat computes toward rope because genuine coordination happens (shared Scripture reference, congregational conversation) with low clerical enforcement. The reformed clergy's seat is mixed—they displace Rome but retain local authority grounded in scriptural persuasiveness. This is NOT a measurement error; it is the signature of an extractive reading that reorders power without abolishing it.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay believers have d near the beneficiary end (low d, ~0.15-0.25) because they receive autonomy benefit even though they bear interpretive responsibility. Reformed clergy have d near symmetric (~0.40-0.50)—they lose magisterial authority but gain pastoral authority; costs and benefits balance differently at different time points. Clerical institutional authority (Catholic/Orthodox) has d near the target end (high d, ~0.80-0.90) because they bear extraction loss without corresponding benefit. Doctrinal coherence and councils have d as targets (high d, ~0.75+) because the reading's operation costs them directly. The engine will compute per-seat divergence: from Rome's seat, this is pure extraction and suppression of a legitimate authority; from lay believers' seats, this is liberation; from reformed clergy's seats, this is reorganization of authority (loss of Rome's monopoly, gain of congregational authority). These divergences are STRUCTURAL and reflect the reading's actual operation—not a failure of the constraint description, but evidence of the reading's extractive asymmetry when viewed from the institutional seats losing authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (medieval monopoly of interpretive authority by Rome) is CONTESTED as to its live status. Catholic authorities (magisterium, councils) deny sola scriptura solves the problem and assert that the reading creates doctrinal chaos that requires external authority to remedy. Protestant authorities attest the founding problem remains live in contemporary Catholicism and deny mandatrophy. The constraint itself does not carry a mandatrophy flag because the reading is actively maintained by living Protestant traditions whose authority structure depends on sola scriptura persisting. However, the rising theater_ratio (0.22→0.41) indicates increasing performance load: as sola scriptura becomes institutionalized in Reformed confessions, study Bibles, and pastoral authority, the original principle of lay autonomy becomes increasingly formal and mediated—a tension between the stated principle and the lived enforcement structure. The mandatrophy concern is NOT that the reading is abandoned, but that the reading's own promise (lay readers accessing doctrine directly) is increasingly satisfied through institutional channels that resemble the pre-Reformation arrangements the reading claimed to displace.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_interpretation_claim_empirical,
    'Can ordinary lay believers reliably interpret Scripture independently without trained hermeneutical frameworks, historical-linguistic expertise, or community adjudication?',
    'Empirical study of lay-reader comprehension outcomes: do isolated lay readers converge on doctrinal readings, or diverge sharply? Historical analysis of early Protestant congregations'' actual interpretive practices.',
    'If lay readers converge naturally, the self-interpreting claim holds empirically; if divergence is endemic, the constraint''s extraction structure shifts—suppression of competing interpretations becomes necessary for the reading to persist, and the constraint reclassifies toward snare. If convergence emerges only with tacit training (study Bibles, pastors'' guidance), the autonomy benefit claim weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_interpretation_claim_empirical, empirical, 'Whether Scripture''s self-interpretation thesis empirically holds for lay readers without hermeneutical training').

omega_variable(
    kernel_reading_distinction,
    'Is sola scriptura a structural claim about authority (Scripture is the sufficient/supreme source for doctrine), or is it a hermeneutical claim about interpretation method (the meaning derives from the text itself, not external tradition)?',
    'Historical genealogy of the doctrine: does Luther/Calvin frame it as authority supremacy, hermeneutical sufficiency, or both? Do later Protestant confessions (Westminster, Heidelberg) distinguish the axes?',
    'If primarily structural (authority claim), the constraint models as a reorganization of the authority hierarchy—low clerical extraction because no institutional class mediates. If primarily hermeneutical (meaning derives from text), the constraint can coexist with institutional tradition interpreting the text—different constraint from the structural reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Whether sola scriptura is fundamentally an authority or a hermeneutics thesis').

omega_variable(
    fragmentation_as_cost_or_feature,
    'Does doctrinal fragmentation across congregations count as a cost of sola scriptura (victim declaration), or as a feature of its beneficiary (lay autonomy costed against unified doctrine)?',
    'Genealogy of Protestant fragmentation: does the reading''s own tradition acknowledge fragmentation as an unintended side effect, or as a necessary consequence of lay autonomy? Reformed vs. Lutheran vs. radical Reformation framings differ here.',
    'If fragmentation is acknowledged as a cost of the reading itself, the victims[] declaration stands. If fragmentation is reframed by later Reformed theology as ''spiritual diversity under material Scripture,'' the victims[] list may need narrowing. The classification outcome (rope vs. tangled_rope vs. scaffold) hinges on whether the reading''s own tradition is satisfied with the fragmentation trade-off.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_as_cost_or_feature, conceptual, 'Whether doctrinal fragmentation is a cost of sola scriptura or a consistent feature of the reading').

omega_variable(
    sibling_reading_coexistence,
    'Can sola scriptura and tradition-based readings coexist within a single institutional framework (one church, one council), or do they foreclose each other?',
    'Historical case: did Reformation-era councils or denominations hold both readings simultaneously, or did the readings segregate into different institutional homes? Contemporary ecumenical dialogue: can a modern Anglican or Methodist congregation hold sola scriptura while acknowledging tradition?',
    'If they foreclose, the reading_relations entry is ''forecloses''. If they coexist in different seats, ''coexists_with''. If one reading creates pressure on the other (e.g., sola scriptura delegitimizes magisterial claims but doesn''t prevent them), ''influences''. The engine uses this topology to compute whether the constraint and its siblings form a contention or a partition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether sola scriptura logically forecloses or merely differs from tradition-based authority reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__sola_scriptura_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(bibl_tr_t0, projected).
narrative_ontology:measurement(bibl_tr_t50, biblical_authority__sola_scriptura_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(bibl_tr_t50, observed).
narrative_ontology:measurement(bibl_tr_t150, biblical_authority__sola_scriptura_reading, theater_ratio, 150, 0.35).
narrative_ontology:measurement_basis(bibl_tr_t150, observed).
narrative_ontology:measurement(bibl_tr_t250, biblical_authority__sola_scriptura_reading, theater_ratio, 250, 0.39).
narrative_ontology:measurement_basis(bibl_tr_t250, observed).
narrative_ontology:measurement(bibl_tr_t350, biblical_authority__sola_scriptura_reading, theater_ratio, 350, 0.4).
narrative_ontology:measurement_basis(bibl_tr_t350, observed).
narrative_ontology:measurement(bibl_tr_t500, biblical_authority__sola_scriptura_reading, theater_ratio, 500, 0.41).
narrative_ontology:measurement_basis(bibl_tr_t500, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__sola_scriptura_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement_basis(bibl_be_t0, projected).
narrative_ontology:measurement(bibl_be_t50, biblical_authority__sola_scriptura_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(bibl_be_t50, observed).
narrative_ontology:measurement(bibl_be_t150, biblical_authority__sola_scriptura_reading, base_extractiveness, 150, 0.55).
narrative_ontology:measurement_basis(bibl_be_t150, observed).
narrative_ontology:measurement(bibl_be_t250, biblical_authority__sola_scriptura_reading, base_extractiveness, 250, 0.48).
narrative_ontology:measurement_basis(bibl_be_t250, observed).
narrative_ontology:measurement(bibl_be_t350, biblical_authority__sola_scriptura_reading, base_extractiveness, 350, 0.42).
narrative_ontology:measurement_basis(bibl_be_t350, observed).
narrative_ontology:measurement(bibl_be_t500, biblical_authority__sola_scriptura_reading, base_extractiveness, 500, 0.38).
narrative_ontology:measurement_basis(bibl_be_t500, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__sola_scriptura_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement_basis(bibl_su_t0, projected).
narrative_ontology:measurement(bibl_su_t50, biblical_authority__sola_scriptura_reading, suppression_requirement, 50, 0.81).
narrative_ontology:measurement_basis(bibl_su_t50, observed).
narrative_ontology:measurement(bibl_su_t150, biblical_authority__sola_scriptura_reading, suppression_requirement, 150, 0.7).
narrative_ontology:measurement_basis(bibl_su_t150, observed).
narrative_ontology:measurement(bibl_su_t250, biblical_authority__sola_scriptura_reading, suppression_requirement, 250, 0.62).
narrative_ontology:measurement_basis(bibl_su_t250, observed).
narrative_ontology:measurement(bibl_su_t350, biblical_authority__sola_scriptura_reading, suppression_requirement, 350, 0.56).
narrative_ontology:measurement_basis(bibl_su_t350, observed).
narrative_ontology:measurement(bibl_su_t500, biblical_authority__sola_scriptura_reading, suppression_requirement, 500, 0.52).
narrative_ontology:measurement_basis(bibl_su_t500, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_authority__sola_scriptura_reading, 0.12).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__conciliar_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, clerical_extraction_through_sacramental_mediation).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, lay_reader_hermeneutical_competence).

% DUAL FORMULATION NOTE:
% This story instantiates ONE reading of the biblical_authority kernel. The sibling readings (tradition_scripture_reading, conciliar_reading) instantiate alternative readings of the same kernel—different constraints from the same text. The ε-invariance principle requires separate stories because the readings' authority structures and beneficiary/victim sets differ substantially. Sola scriptura reduces clerical extraction via lay autonomy; tradition_scripture reading centralizes extraction via magisterial authority; conciliar reading distributes authority through councils. These are three distinct constraint structures with three distinct ε values, not one constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__sola_scriptura_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
