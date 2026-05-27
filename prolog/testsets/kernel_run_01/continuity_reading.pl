% ============================================================================
% CONSTRAINT STORY: continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_continuity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: continuity_reading
 *   human_readable: Vatican II Continuity Reading: Organic Doctrinal Development
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   Vatican II (1962-1965) represents a turning point in Catholic
 *   self-understanding. The council produced 16 documents authorizing
 *   liturgical reform, episcopal collegiality, ecumenical openness, and
 *   ressourcement theology. The Church's official position is that these
 *   reforms represent 'organic development in continuity with tradition' —
 *   the deposit of faith remains unchanged; the council articulated what was
 *   always implicit. This is ONE reading of a contested kernel. The
 *   continuity reading claims that all reforms are legitimate expressions of
 *   unchanging doctrine, benefiting progressive reformers who can cite
 *   Vatican II as authoritative while reassuring traditionalists that
 *   substance is preserved. However, the claim of continuity-in-rupture
 *   suppresses the lived experience of pre-conciliar Catholics for whom the
 *   experiential discontinuity is profound. The constraint exhibits all six
 *   DR types from different structural positions, revealing the reading as a
 *   coordination mechanism that benefits some agents while suppressing
 *   others' phenomenology.
 *
 * KEY AGENTS:
 *   - Progressive Reformers: Primary beneficiary (powerful/mobile) — gain legitimacy for vernacular liturgy, episcopal collegiality, ecumenical openness; cite Vatican II as authoritative while advancing substantive innovations
 *   - Traditionalist Faithful: Primary victim (moderate/identity_locked) — structurally mobile (can attend TLM) but identity-constituted through pre-conciliar forms; continuity claim negates their lived discontinuity experience
 *   - Traditionalist Authority: Secondary actor (powerful/constrained) — experiences mixed extraction: gains authority from Vatican II's unchallengeable status but constrained by hermeneutic discipline to prove continuity
 *   - Reconciliation Projects: Organized agents (organized/constrained) — bridge communities treating continuity as temporary coordination mechanism with sunset; assume future hermeneutic work can make continuity workable in lived practice
 *   - Institutional Magisterium: Institutional actor (institutional/arbitrage) — maintains continuity narrative through performative theological labor; has arbitrage between citing reforms and citing tradition
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional coherence-maintenance as doctrinal immutability; continuity reading appears as natural law of tradition until false summit detection reveals beneficiaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(continuity_reading, 0.28).
domain_priors:suppression_score(continuity_reading, 0.35).
domain_priors:theater_ratio(continuity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(continuity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(continuity_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(continuity_reading, rope).
narrative_ontology:human_readable(continuity_reading, "Vatican II Continuity Reading: Organic Doctrinal Development").
narrative_ontology:topic_domain(continuity_reading, "theology/ecclesiology/religious_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(continuity_reading, 'c3e3f636-dea5-4fe6-9089-aad1a2decc86').
narrative_ontology:cs_created_at('c3e3f636-dea5-4fe6-9089-aad1a2decc86', '').
narrative_ontology:cs_kernel_codification('c3e3f636-dea5-4fe6-9089-aad1a2decc86', fixed_text).
narrative_ontology:cs_authority_grounding('c3e3f636-dea5-4fe6-9089-aad1a2decc86', lineage).
narrative_ontology:cs_interpretation_layer_present('c3e3f636-dea5-4fe6-9089-aad1a2decc86').
narrative_ontology:cs_kernel_id(continuity_reading, vatican_ii_authority).
narrative_ontology:cs_reading_relation('c3e3f636-dea5-4fe6-9089-aad1a2decc86', rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('c3e3f636-dea5-4fe6-9089-aad1a2decc86', composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('c3e3f636-dea5-4fe6-9089-aad1a2decc86', foundational, development_preserves_substance).
narrative_ontology:cs_axiom_status(development_preserves_substance, holdable).
narrative_ontology:cs_axiom('c3e3f636-dea5-4fe6-9089-aad1a2decc86', foundational, hermeneutic_unity_resolvable).
narrative_ontology:cs_axiom_status(hermeneutic_unity_resolvable, holdable).
narrative_ontology:cs_reference_frame('c3e3f636-dea5-4fe6-9089-aad1a2decc86', pre_conciliar_doctrinal_continuity).
narrative_ontology:cs_drift_state('c3e3f636-dea5-4fe6-9089-aad1a2decc86', contemporary_post_vatican_ii_reception, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(continuity_reading, progressive_reformers).
narrative_ontology:constraint_beneficiary(continuity_reading, bishops_embracing_aggiornamento).
narrative_ontology:constraint_beneficiary(continuity_reading, theological_modernists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROGRESSIVE REFORMER (ROPE) — Reads Vatican II as legitimate organic development. The 16 documents authorize vernacular liturgy, episcopal collegiality, ecumenical openness, and ressourcement theology. The reformer sees coordination: the council articulated long-developing doctrinal seeds into explicit form. No coercion experienced; the constraint is enabling. Effective extraction near zero — the constraint benefits this agent through legitimacy gained.
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: TRADITIONALIST AUTHORITY (TANGLED ROPE) — Experiences Vatican II as a constraint on tradition. The authority must enforce continuity claims (cost: interpretive labor, constant theological work to harmonize texts). Also benefits from the council's authority to override rival traditionalist factions (cost: some communities demand pre-1962 forms). Mixed extraction: the constraint enables authority (no rival council to challenge Vatican II) while binding the authority to a hermeneutic discipline that limits absolute discretion.
constraint_indexing:constraint_classification(continuity_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: TRADITIONALIST FAITHFUL (SNARE) — Structurally mobile (can attend FSSP or diocesan TLM) but identity-locked in traditional form. The continuity reading suppresses their lived experience: their identity is constituted through pre-conciliar liturgy, theology, and piety. The continuity argument ('the council changed nothing essential') negates the radical experienced discontinuity. They cannot exit without becoming a different person (abandoning the self constituted through tradition). Suppression is internalized: their own Church hierarchy claims nothing changed, so their profound experience of change is gaslit as misunderstanding.
constraint_indexing:constraint_classification(continuity_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 4: RECONCILIATION PROJECT / HERMENEUTIC BRIDGE (SCAFFOLD) — Organized initiatives (ressourcement scholars, liturgical both-and movements, intentional parishes modeling continuity-in-renewal) treat the continuity reading as a temporary coordination mechanism with a sunset. The bridge assumes that sufficient hermeneutic work, historical scholarship, and liturgical creativity can eventually render continuity and renewal compatible in lived practice. The scaffold has constraints (resource limits, aging demographics of bridge communities) but a real sunset: as Vatican II becomes historical (75+ years out) and as new liturgical forms mature (post-2007 normalization of TLM), the urgent need to prove continuity diminishes.
constraint_indexing:constraint_classification(continuity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INSTITUTIONAL MAGISTERIUM (PITON) — The Vatican's official line remains 'Vatican II represents organic development in continuity with tradition.' This is maintained through institutional inertia and performative theological work. The magisterium has arbitrage: it can cite Vatican II to justify reforms, cite tradition to resist radical revision, and claim harmonic continuity between both. The constraint's primary function (legitimating authority) has attenuated — most Catholics engage Vatican II pragmatically rather than studying continuity arguments. The continuity narrative persists through institutional repetition, not functional necessity. Theater ratio = 0.38: real hermeneutic labor exists, but much is theater maintaining the authority structure's unified appearance.
constraint_indexing:constraint_classification(continuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, doctrinal development always appears continuous to the inheritor — each generation experiences its received tradition as intact and unified, while making necessary adaptations to new contexts. This is a natural feature of how living traditions maintain authority and legitimacy: the appearance of continuity is structurally required for the tradition to function as authoritative. The continuity reading would be invisible as a constraint if it worked perfectly — it would just be how doctrine functions. However, the structural data contradicts the mountain classification. The baseline belief (development can be 'organic' yet look identical to revolutionary change) depends on a specific hermeneutic and metaphysical framing that benefits identifiable agents. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(continuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(continuity_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(continuity_reading, TR),
    TR >= 0.70.

:- end_tests(continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The continuity reading does not rely on high extraction because it is fundamentally a coordination mechanism. Progressive reformers do benefit (legitimacy for substantive changes) but the constraint also enables traditionalists by preserving the appearance of doctrinal unchangedness. The extraction is modest because much of the constraint's value is genuinely coordination: it allows divergent reformers and traditionalists to remain in communion while pursuing different pastoral visions. The rising extractiveness over time (0.18→0.28 over 20 years) reflects increasing institutional labor required to maintain continuity claims as actual practice diverges from pre-conciliar norms. Suppression (0.35): Moderate. The traditionalist faithful face suppression not through external barriers (they can attend TLM, which returned to wider availability post-2007) but through internalized identity-lock: the Church hierarchy's continuity claim gaslights their experience of change. The suppression is not total — traditionalist communities exist, networks function, TLM communities thrive — but the official narrative delegitimizes their phenomenology. Theater ratio (0.38): Moderate. Real hermeneutic labor exists (ressourcement scholars, doctrinal comparisons, development-of-doctrine theology), but an increasing proportion is theater: institutional maintenance of unified narrative rather than resolution of actual theological questions. The rising theater reflects that the main function (making reforms appear continuous) is now decoupled from the actual function (those reforms are accepted on their merits regardless of continuity claims).
 *
 * PERSPECTIVAL GAP:
 *   The progressive reformer sees coordination (Rope): Vatican II legitimates substantive innovations while preserving doctrinal authority. The traditionalist faithful sees suppression (Snare): the continuity claim negates their lived discontinuity, identity-locking them in a tradition they experience as transformed. The traditionalist authority sees mixed constraints (Tangled Rope): the council both enables authority (unified teaching authority post-Vatican II) and constrains it (hermeneutic discipline to prove continuity). The reconciliation projects see a temporary problem with a sunset (Scaffold): they assume future hermeneutic work will make continuity workable; the bridge has a deadline (when Vatican II becomes too historical for urgent continuity claims). The institutional magisterium sees its own degraded ritual (Piton): the continuity narrative persists through institutional repetition despite decoupling from pastoral reality. The analytical observer risks seeing natural law (Mountain): doctrinal development always appears continuous to the inheritor — but the structural data reveals this as false summit, not immutable law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural position relative to the continuity constraint. The progressive reformer benefits (low d, negative χ) because the constraint legitimates their agenda. The traditionalist faithful suffers (high d, high χ) because they experience suppression of their phenomenology; their identity-lock means they cannot exercise available structural mobility. The traditionalist authority has mixed positioning (moderate d) — they benefit from the council's unquestionable authority but bear the cost of hermeneutic labor. The reconciliation projects have constrained but non-trapped exit (lower d) because their vision has a structural sunset; they can exit by ceasing the bridge work. The magisterium has arbitrage (very low d) — they maintain the narrative by selectively citing sources. The analytical observer is positioned as analytical (canonical d ≈ 0.73) but the false summit detector will flag the naturalization move.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_rupture_empirical,
    'Are the 16 Vatican II documents sufficiently continuous with pre-conciliar magisterial teaching that a pre-1962 bishop transported to 1965 would recognize the post-conciliar Church as ''development'' rather than ''transformation''?',
    'Detailed lexical and doctrinal comparison: (a) frequency of continuity markers (''always'', ''immemorial'', ''ancient'') vs rupture markers (''new'', ''unprecedented'', ''adaptation to modern world'') in conciliar documents; (b) genealogical tracing of disputed reforms (collegiality, religious liberty, ecumenism) backward to pre-conciliar sources; (c) historical evidence of what bishops and periti intended vs what they produced.',
    'If high continuity detected: continuity reading confirmed structurally; other readings move to ''ideologically motivated reinterpretation.'' If substantial rupture detected: continuity reading becomes a coherence-maintenance fiction; snare and tangled_rope perspectives gain structural legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuity_vs_rupture_empirical, empirical, 'Empirical continuity assessment of Vatican II documents vs pre-conciliar teaching').

omega_variable(
    hermeneutic_circle_sufficiency,
    'Is the hermeneutic move (''the council must be read in light of tradition, and tradition must be read in light of Vatican II'') actually resolvable into a stable reading, or does it generate infinite regress where each interpreting group obtains the opposite conclusion?',
    'Systematic review of major post-conciliar debates (liturgy, collegiality, sexual ethics, religious liberty) where continuity hermeneutic was explicitly invoked. Measure: did invoking continuity resolve the dispute or simply reproduce it at a higher level of abstraction? Did both sides claim continuity-in-tradition while reaching opposite pastoral conclusions?',
    'If hermeneutic circle resolves stably: continuity reading is a genuine coordination mechanism (Rope confirmed). If it generates regress: continuity reading is a performative fig-leaf over irresolvable differences (Piton confirmed; rope/mountain claim invalid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutic_circle_sufficiency, empirical, 'Whether hermeneutic continuity circle resolves actual theological disputes or reproduces them').

omega_variable(
    traditional_identity_experiential_gap,
    'What proportion of pre-conciliar Catholics subjectively experience Vatican II reforms as continuous development vs discontinuous rupture? Does the continuity claim match lived Catholic phenomenology?',
    'Oral history and memoir analysis from bishops, priests, and lay faithful who lived through pre-1962 Catholicism and post-1965 reforms. Secondary: generational cohort studies measuring continuity/rupture perception across birth cohorts.',
    'If widespread lived discontinuity (>70% report rupture experience): identity_locked suppression mechanism is confirmed; traditionalist faithful (snare perspective) is structurally grounded. If widespread lived continuity: identity_locked framing is misapplied; suppression is lower than modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditional_identity_experiential_gap, empirical, 'Lived phenomenology of Vatican II: continuity vs rupture experience among participants').

omega_variable(
    reading_underdetermination_normative,
    'Which reading — continuity, rupture, or composite — should the Church have adopted as its official self-understanding? Is the continuity reading chosen because it is true, because it maintains institutional authority, or because both objectives align?',
    'This is fundamentally a normative/preference question, not empirically resolvable. What IS resolvable: did Church leaders choose continuity framing explicitly to maintain authority (evidence: internal memos, correspondence, conciliar debates about how to ''sell'' reforms), or did they arrive at continuity as the only honest assessment?',
    'If continuity was instrumentally chosen: reading becomes a snare (beneficiaries impose it; victims suppressed). If continuity was honest assessment that happened to also serve authority: reading remains rope/scaffold. If authority was indifferent to which reading was adopted: reading might be mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_underdetermination_normative, preference, 'Normative question: should this reading have been the adopted one?').

omega_variable(
    sibling_reading_logical_scope,
    'Can all three readings (continuity, rupture, composite overdetermination) coexist in a single magisterial framework, or does each reading require incompatible theological commitments?',
    'Analyze the core premise of each reading. Continuity: ''development can occur while leaving substance unchanged.'' Rupture: ''Vatican II represents real discontinuity in teaching or practice.'' Composite: ''Vatican II simultaneously develops and ruptures, depending on the doctrinal domain.'' Do these logically coexist within Catholic theology, or does accepting one require rejecting another?',
    'If logically coexistent: reading_relations should be coexists_with. If the continuity reading logically forecloses rupture (or vice versa): relation should be forecloses. If one influences but does not eliminate the other: influences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_logical_scope, conceptual, 'Logical compatibility of continuity, rupture, and composite readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(continuity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cont_tr_t0, continuity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cont_tr_t10, continuity_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(cont_tr_t20, continuity_reading, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(cont_be_t0, continuity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cont_be_t10, continuity_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(cont_be_t20, continuity_reading, base_extractiveness, 20, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(continuity_reading, rupture_reading).
narrative_ontology:affects_constraint(continuity_reading, composite_overdetermination_reading).
narrative_ontology:affects_constraint(continuity_reading, liturgical_form_authority).
narrative_ontology:affects_constraint(continuity_reading, episcopal_collegiality_implementation).

% DUAL FORMULATION NOTE:
% Vatican II kernel has three independent constraint stories: continuity_reading, rupture_reading, composite_overdetermination_reading. Each story has its own ε value, its own beneficiary/victim structure, and its own classification landscape. The three readings are linked by shared kernel (Vatican II's 16 documents) but structurally distinct constraints. Continuity reading (this file) assumes the hermeneutic can be made coherent and benefits are real coordination. Rupture reading assumes the hermeneutic is a cover story and benefits are extraction. Composite reading assumes both readings have legitimate domains and no synthesis is possible. They do not decompose by observable; they decompose by reading/interpretation of a shared contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(continuity_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
