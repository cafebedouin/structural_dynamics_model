% ============================================================================
% CONSTRAINT STORY: biblical_authority__tradition_scripture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__tradition_scripture_reading, []).

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
 *   constraint_id: biblical_authority__tradition_scripture_reading
 *   human_readable: Magisterial Interpretation Requirement: Scripture + Tradition + Magisterium
 *   domain: theological/religious/institutional
 *
 * SUMMARY:
 *   The tradition-scripture-magisterium reading holds that Scripture cannot
 *   be rightly understood apart from the living tradition of the Church, and
 *   that the magisterium—the teaching authority of the ordained clergy,
 *   centered in Rome—is the authoritative guardian and interpreter of that
 *   deposit of faith. This reading constitutes one of three coherent framings
 *   of the biblical authority kernel (the others being sola scriptura and
 *   conciliar tradition). Under this reading, lay believers do not possess
 *   the capacity or charism to interpret Scripture authoritatively; doing so
 *   requires magisterial sanction and the mediation of the sacramental
 *   priesthood. The arrangement simultaneously solves a genuine coordination
 *   problem (how to prevent doctrinal fragmentation across geographically
 *   dispersed Christian communities) and creates asymmetric extraction (the
 *   magisterium concentrates interpretive authority and mediation of grace,
 *   while lay believers' right to direct scriptural access is suppressed).
 *   The constraint is CLAIMED as tangled_rope—coordination plus
 *   extraction—and the authored metrics reflect that structure:
 *   extractiveness is substantial (0.68 at interval end) and rising,
 *   suppression is high (0.71) and persistent, yet the coordination function
 *   (doctrinal unity) is demonstrably real and valued by many believers.
 *
 * KEY AGENTS:
 *   - magisterium (institutional hierarchy): sets interpretive rules, declares doctrine, controls sacramental access
 *   - institutional_clergy (ordained priests, bishops): execute magisterial authority, mediate sacraments, catechize believers
 *   - lay_believers (non-ordained): forbidden from authoritative interpretation, dependent on magisterial teaching, mediated access to grace
 *   - non_magisterial_interpreters (theological challengers, reform movements): claim direct scriptural insight or alternative tradition sources; suppressed by enforcement
 *   - reform movements (historical): Waldensians, Wycliffites, pre-Reformation challengers; present-day Pentecostals and independent biblical scholars
 *   - magisterial councils and encyclicals (documents): the mechanism through which magisterium declares doctrine and reiterates suppression of lay interpretive claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, 0.68).
domain_priors:suppression_score(biblical_authority__tradition_scripture_reading, 0.71).
domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__tradition_scripture_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__tradition_scripture_reading, "Magisterial Interpretation Requirement: Scripture + Tradition + Magisterium").
narrative_ontology:topic_domain(biblical_authority__tradition_scripture_reading, "theological/religious/institutional").

domain_priors:requires_active_enforcement(biblical_authority__tradition_scripture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__tradition_scripture_reading, 'ec77f1b1-aaef-4d0e-970c-7120e493af9c').
narrative_ontology:cs_kernel_codification('ec77f1b1-aaef-4d0e-970c-7120e493af9c', fixed_text).
narrative_ontology:cs_authority_grounding('ec77f1b1-aaef-4d0e-970c-7120e493af9c', lineage).
narrative_ontology:cs_interpretation_layer_present('ec77f1b1-aaef-4d0e-970c-7120e493af9c').
narrative_ontology:cs_reading_relation('ec77f1b1-aaef-4d0e-970c-7120e493af9c', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('ec77f1b1-aaef-4d0e-970c-7120e493af9c', biblical_authority__conciliar_reading, influences).
narrative_ontology:cs_axiom('ec77f1b1-aaef-4d0e-970c-7120e493af9c', foundational, magisterium_sole_tradition_custodian).
narrative_ontology:cs_axiom_status(magisterium_sole_tradition_custodian, holdable).
narrative_ontology:cs_axiom_grounding('ec77f1b1-aaef-4d0e-970c-7120e493af9c', magisterium_sole_tradition_custodian, deontological).
narrative_ontology:cs_axiom('ec77f1b1-aaef-4d0e-970c-7120e493af9c', foundational, lay_interpretive_incompetence).
narrative_ontology:cs_axiom_status(lay_interpretive_incompetence, overridden).
narrative_ontology:cs_axiom_grounding('ec77f1b1-aaef-4d0e-970c-7120e493af9c', lay_interpretive_incompetence, empirically_contingent).
narrative_ontology:cs_reference_frame('ec77f1b1-aaef-4d0e-970c-7120e493af9c', apostolic_succession_magisterial_authority).
narrative_ontology:cs_drift_state('ec77f1b1-aaef-4d0e-970c-7120e493af9c', contemporary_biblical_criticism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ec77f1b1-aaef-4d0e-970c-7120e493af9c', '2026-06-15T14:32:00Z').
narrative_ontology:cs_kernel_id(biblical_authority__tradition_scripture_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, magisterium).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, institutional_clergy).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, lay_believers).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, non_magisterial_interpreters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Church (centered in Rome for Catholic tradition, distributed across patriarchates for Orthodox tradition). Sets the rules of doctrinal interpretation, declares what counts as apostolic tradition, determines which scriptural readings are orthodox and which are heretical. Can reformulate doctrine (development of doctrine) in response to pressure, which gives it exit options even when challenged. Collects authority, deference, and institutional power from the arrangement.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, magisterium, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Ordained priests, bishops, and theologians authorized to interpret Scripture and administer sacraments. They benefit from clerical privilege: interpretive monopoly, sacramental power, institutional status. They also execute magisterial enforcement (catechizing lay believers in magisterial teaching, suppressing non-approved interpretation, controlling sacramental access). Their exit is constrained because leaving the priesthood means surrendering clerical status and sacramental authority, which is the source of their power.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, institutional_clergy, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, institutional_clergy, agenda_setter).

% Non-ordained church members. Prohibited from authoritative scriptural interpretation; required to receive doctrine from the magisterium. Their access to grace is mediated through sacraments administered by ordained clergy. They pay through suppressed interpretive agency and mandatory deference. Exit is identity-locked because membership in the Church is constitutive of their religious identity; leaving means spiritual rupture and loss of community.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, lay_believers, payer,
    powerless, biographical, identity_locked, global).

% Theologians, biblical scholars, and reform movements claiming direct insight into Scripture or alternative tradition sources (e.g., patristic consensus, conciliar authority, contemporary exegesis). They claim the magisterium has departed from authentic tradition or misread Scripture. Suppressed through heresy charges, institutional marginalization, or exclusion from Church structures if internal; if external (Protestants), they compete but are branded as schismatic. Their constrained exit reflects that internal challengers risk exclusion, and external challengers operate outside the framework the constraint governs.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, non_magisterial_interpreters, payer,
    moderate, biographical, constrained, global).

% Waldensians, Wycliffites, Hussites, and pre-Reformation proto-reformers (historically); Protestants post-Reformation; modern Pentecostals, biblical fundamentalists, and independent Christian communities (contemporarily). These movements reject magisterial interpretive authority and claim direct access to scriptural meaning or alternative tradition sources (councils, fathers, Spirit-leading). They are excluded from magisterial decision-making by definition of the constraint: if they were included, the constraint would dissolve. Their trapped status reflects that they cannot reshape magisterial Christianity from outside; their only options are exit (forming separate communities) or suppression (historical: execution, inquisition; modern: institutional marginalization).
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, reform_movements, excluded,
    organized, generational, trapped, global).

% Orthodox patriarchates, Eastern Christian communities, and other magisterial-like authorities claiming apostolic succession and tradition-guarding roles. Each claims to be the authentic guardian of the deposit of faith. They are excluded from the Catholic magisterium's framework by institutional separation; they compete but cannot reshape each other's interpretive rules. Their trapped status reflects institutional rupture (schism) that cannot be unilaterally bridged.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, competing_ecclesiastical_authorities, excluded,
    institutional, generational, trapped, global).

% Biblical scholars, historians of Christianity, comparative religionists, and philosophers of language who study the constraint from outside any committed framework. They measure whether the constraint actually prevents fragmentation, whether the magisterium's claims to tradition are historically grounded, and whether the suppression of lay interpretation is justified by the coordination benefits it produces. Their analytical position allows them to see the constraint's structure without being subject to its enforcement.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, analytical_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__tradition_scripture_reading, magisterium).
narrative_ontology:fixing_cost_class(biblical_authority__tradition_scripture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevention of doctrinal fragmentation and maintenance of unified Christian identity across geographically dispersed and culturally diverse communities. Early Christian communities lacked instant communication; a centralized interpretive authority reduced the risk that local groups would diverge into incompatible doctrines and lose communion with each other. The magisterium provides a single authoritative voice on what Scripture means and what the deposit of faith contains.
% TRANSFER_FUNCTION: Transfers interpretive authority and the power to mediate grace from lay believers to the ordained clergy (magisterium). Lay believers surrender direct scriptural reading authority and must accept magisterial teaching; in exchange, they receive the assurance of doctrinal safety and sacramental grace mediated through authorized channels. Institutional clergy receive power, authority, and institutional security; the magisterium collects interpretive monopoly and doctrinal control.
% ABSENT_VOICES: Lay believers who would claim direct scriptural insight are systematically excluded by the constraint's definition (their claims are declared heretical). Non-magisterial interpreters (Protestants, modern exegetes, conciliarist theologians) would argue that the magisterium has departed from authentic tradition and that tradition should constrain magisterium, not magisterium define tradition. Historically silenced voices include Waldensians, Wycliffites, and proto-Reformers; contemporarily, they include biblical scholars working outside magisterial approval and reform movements within Catholicism advocating for lay voice and married clergy.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight—if lay believers suddenly possessed equal interpretive authority and sacramental access was decoupled from clergy mediation—Catholic institutional structure would face immediate reorganization. The magisterium would lose its primary source of institutional power; lay interpretation would proliferate, producing theological diversity (likely not as much as Protestantism's diversity, but substantially more than current magisterial uniformity); the priesthood's distinctive role would be renegotiated; and the Church's organizational unity would depend on different coordination mechanisms (perhaps synodal consensus, conciliar structure, or bottom-up doctrinal formation). The world rearranges because the constraint is constitutive of the institutional structure, not a side effect of it.
% FOUNDING_PROBLEM: Early Christian communities, separated by geography and slow communication, risked developing divergent understandings of Scripture and apostolic tradition. Without a centralized interpretive authority, local bishops or councils might declare different doctrines authoritative, leading to schism and loss of communion. The magisterial solution: a single supreme teaching authority (Rome, claiming Peter's succession) capable of settling disputed interpretations and declaring authoritatively what the deposit of faith contains.
% FOUNDING_PROBLEM_CORROBORATION: The magisterium and traditionalist Catholics attest the founding problem remains live: lay interpretation produces heresy, doctrinal confusion, and schism, as witnessed by Protestantism's denominational fragmentation. Liberal Catholics and Protestant scholars attest the founding problem is substantially dead: modern communication has solved the geographic/time-delay problem; doctrinal diversity persists in Catholicism despite magisterial enforcement (on contraception, divorce, clerical celibacy, women's ordination); and theological scholarship has shown that the early Church had more doctrinal diversity than magisterial claims to uniform apostolic tradition suggest. External scholarly consensus (Bultmann, Crossan, Sanders, and the Jesus Seminar on historical exegesis; Ehrman on early Christian scribal variation; Pelikan on doctrinal development) supports the 'founding problem is substantially solved' reading: the magisterium persists because it benefits the institutional hierarchy, not because doctrinal unity depends on it.
narrative_ontology:disappearance_verdict(biblical_authority__tradition_scripture_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__tradition_scripture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__tradition_scripture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__tradition_scripture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__tradition_scripture_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__tradition_scripture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__tradition_scripture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.68 over the interval, reflecting cumulative doctrinal definitions that expand the scope of magisterial authority (dogmas of Immaculate Conception, Papal Infallibility, Assumption, etc.) and the growth of theological expertise gatekeeping. Suppression holds steady at 0.70+ because the institutional structure does not waver: lay interpretive agency remains formally forbidden, even as cultural pressure increases. Theater rises from 0.25 to 0.42 because an increasing share of magisterial activity is devoted to defending its interpretive monopoly against internal dissent and external competitors (Protestantism, biblical criticism, modern exegesis) rather than actively developing doctrine from Scripture. The measurement grid is shared across all three metrics at six time points (t=0,4,8,12,16,20) within the interval [0,20], representing the period from late medieval consolidation through Counter-Reformation to early modern challenges. At each point, all three metrics are authored independently, not back-filled.
 *
 * PERSPECTIVAL GAP:
 *   The magisterium seat (agenda-setter) experiences this constraint as genuine coordination it maintains: it prevents doctrinal chaos, preserves apostolic continuity, and guards believers from error. The lay believer seat (payer) experiences it as enforced mediation with rising cost: as doctrinal definitions multiply, lay believers must accept more complex requirements without direct scriptural warrant; resistance to magisterial teaching meets suppression (excommunication, inquisition historically; institutional marginalization today). The boundary is not between different power levels discovering different aspects of the same constraint—it is between the beneficiary (magisterium) and the target (lay believers) experiencing structurally opposite directionalities. The engine computes this: the magisterium's d is near the beneficiary end (low extraction, high subsidy from lay compliance); the lay believer's d is near the target end (high extraction, high suppression of exit). The claim (tangled_rope) declares this asymmetry as structural; the metrics provide the evidence.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterium (institutional power atom, generational horizon, arbitrage exit) is the structural beneficiary: it collects interpretive monopoly, authority over grace-conferral, and doctrinal control. Its exit options are arbitrage-class because it can respond to pressure by reformulating doctrine (development of doctrine doctrine itself is a magisterial move to reframe new claims as continuous tradition). The lay believer (powerless to moderate power atoms, biographical horizon, identity_locked exit) is the structural target: they pay through suppressed interpretive agency, mediated sacramental access, and mandatory deference to magisterial teaching. Their exit is identity-locked because membership in the Church is constitutive of the believer's religious identity; leaving the magisterium means spiritual rupture. Non-magisterial interpreters (moderate to organized power, biographical horizon, constrained exit if historically within the Church, or mobile if external competitors like Protestantism) bear suppression and exclusion. The derived directionality places lay believers near d=0.85-0.95 (full targets) and the magisterium near d=0.05-0.15 (full beneficiaries), with institutional clergy near d=0.10-0.25 (beneficiary with enforcement costs). No overrides are needed; the structural data drive the computation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's founding problem is the prevention of doctrinal fragmentation in the early Church—a genuine coordination challenge when communities were geographically dispersed and communication was slow. Magisterial authority, grounded in apostolic succession and the protection of the deposit of faith, solved that problem. The question is whether the problem remains live or has shifted. In the contemporary period, doctrinal fragmentation has occurred despite magisterial enforcement: modern Catholicism shows wide lay disagreement with official teaching on contraception, divorced-remarriage, clerical celibacy, and women's ordination. The magisterium's response has been to increase theatrical activity (documents asserting authority, catechetical campaigns) without fundamentally altering the suppression level—suppression holds at 0.71 because the structural mechanism (lay interpretive prohibition) persists unchanged. This pattern (constant suppression, rising theater, persistent disagreement) flags the constraint as potentially mandatrophic: the founding problem may be dead (doctrinal unity is already fragmented), but the constraint persists due to institutional inertia and the magisterium's institutional interest in maintaining its authority. The constraint does not appear to fit piton (a former rope entirely atrophied), because the coordination function, though weakened, is still claimed and partially functional (many believers do defer to magisterial teaching, and the constraint does reduce doctrinal spread relative to purely lay-driven interpretation). The mandatrophy signal is moderate, not definitive—the founding_problem_status is 'contested' for exactly this reason.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_fork_sola_scriptura,
    'Is this reading a genuine commitment to apostolic tradition, or a cover for magisterial institutional power disguised as tradition preservation?',
    'Historical reconstruction of pre-magisterial tradition sources and comparison with contemporary magisterial claims to continuous tradition; examination of cases where magisterium explicitly rejected earlier patristic or conciliar readings.',
    'If cover-story reading confirmed, the constraint reclassifies from tangled_rope (real coordination via tradition + real extraction via mediation requirement) to snare (extraction with coordination narrative). If genuine, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_fork_sola_scriptura, empirical, 'Whether claimed tradition transmission is authentic or post-hoc institutional justification.').

omega_variable(
    magisterial_deposit_interpretive_closure,
    'What counts as ''the deposit of faith'' — is it a fixed textual/doctrinal boundary, or does the magisterium''s power to declare new dogmas (e.g., Immaculate Conception, Assumption) imply the deposit is open-ended and magisterially redefinable?',
    'Formal magisterial teaching documents addressing the status of development of doctrine and the relationship between the deposit''s closure and magisterial authority to define.',
    'If the deposit is truly closed, the magisterium''s role is guardianship within bounds. If magisterially redefinable, the constraint''s extractiveness rises (lay believers have no boundary against new requirements); the reading''s foundational claim (magisterium guards a fixed deposit) is partially overridden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_deposit_interpretive_closure, conceptual, 'Whether the deposit of faith is a fixed or magisterially expandable set.').

omega_variable(
    suppression_internalization_vs_structural,
    'Is the measured suppression (0.71) primarily structural (institutional barriers: lay absence from interpretation, sacramental access control) or internalized (lay believers have accepted as doctrine the claim that they cannot reliably interpret Scripture themselves)?',
    'Post-Reformation survey data on lay interpretive confidence; examination of Reformation-era documentation of when lay believers began claiming interpretive agency; contemporary interviews with lay members who retain the suppression claim after exposure to alternative readings.',
    'If primarily internalized, the constraint''s effective suppression is higher than the structural measure suggests — lay exit from magisterial Christianity would not automatically restore interpretive confidence. If primarily structural, removing institutional barriers would quickly restore lay agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_vs_structural, empirical, 'Whether suppression is structural barrier or internalized doctrinal belief.').

omega_variable(
    kernel_reading_conciliar_vs_magisterial_tradition,
    'Is ''tradition'' defined by the magisterium alone (this reading''s core), or is tradition the consensus of councils, patristic fathers, and living continuity that the magisterium must guard but cannot unilaterally redefine (the conciliar_reading''s core)?',
    'Historical cases where the magisterium claimed to speak for tradition against conciliar/patristic consensus (e.g., suppression of conciliarist councils in 15th century, magisterial dogmas lacking explicit patristic warrant); systematic comparison of magisterial claims to authority against the stated role of councils in doctrinal development.',
    'If magisterium is sole authority over tradition, this reading stands as written. If tradition constrains magisterium, the conciliar_reading''s pressure effectively influences this reading''s operational scope — the magisterium''s power to extract through interpretive monopoly is structurally limited. This is the primary reading_relations axis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_conciliar_vs_magisterial_tradition, conceptual, 'Whether magisterium creates tradition or guards tradition created by conciliar/patristic consensus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__tradition_scripture_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__tradition_scripture_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t4, biblical_authority__tradition_scripture_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement_basis(bibl_tr_t4, observed).
narrative_ontology:measurement(bibl_tr_t8, biblical_authority__tradition_scripture_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(bibl_tr_t8, observed).
narrative_ontology:measurement(bibl_tr_t12, biblical_authority__tradition_scripture_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement_basis(bibl_tr_t12, observed).
narrative_ontology:measurement(bibl_tr_t16, biblical_authority__tradition_scripture_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement_basis(bibl_tr_t16, observed).
narrative_ontology:measurement(bibl_tr_t20, biblical_authority__tradition_scripture_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(bibl_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__tradition_scripture_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t4, biblical_authority__tradition_scripture_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement_basis(bibl_be_t4, observed).
narrative_ontology:measurement(bibl_be_t8, biblical_authority__tradition_scripture_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement_basis(bibl_be_t8, observed).
narrative_ontology:measurement(bibl_be_t12, biblical_authority__tradition_scripture_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement_basis(bibl_be_t12, observed).
narrative_ontology:measurement(bibl_be_t16, biblical_authority__tradition_scripture_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement_basis(bibl_be_t16, observed).
narrative_ontology:measurement(bibl_be_t20, biblical_authority__tradition_scripture_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(bibl_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__tradition_scripture_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t4, biblical_authority__tradition_scripture_reading, suppression_requirement, 4, 0.61).
narrative_ontology:measurement_basis(bibl_su_t4, observed).
narrative_ontology:measurement(bibl_su_t8, biblical_authority__tradition_scripture_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement_basis(bibl_su_t8, observed).
narrative_ontology:measurement(bibl_su_t12, biblical_authority__tradition_scripture_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement_basis(bibl_su_t12, observed).
narrative_ontology:measurement(bibl_su_t16, biblical_authority__tradition_scripture_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement_basis(bibl_su_t16, observed).
narrative_ontology:measurement(bibl_su_t20, biblical_authority__tradition_scripture_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(bibl_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__tradition_scripture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(biblical_authority__tradition_scripture_reading, 0.12).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__conciliar_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, sacramental_mediation_grace_conferral).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the biblical_authority kernel. The sola_scriptura_reading and conciliar_reading are structurally distinct constraints with different ε values, different beneficiary/victim structures, and different types. They coexist as live positions in ongoing Reformation/Counter-Reformation dispute. The three stories form a constraint family linked by the kernel and by historical institutional competition. The network edges capture this: each reading influences and partly forecloses the others through institutional claims and doctrinal definitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__tradition_scripture_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
