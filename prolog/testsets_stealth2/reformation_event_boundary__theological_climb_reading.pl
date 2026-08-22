% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__theological_climb_reading, []).

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
 *   constraint_id: reformation_event_boundary__theological_climb_reading
 *   human_readable: Theological Climb Reading of the Reformation Event Boundary
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This story instantiates ONE reading - the theological climb reading - of
 *   the contested kernel reformation_event_boundary: the question of what
 *   kind of event the Reformation was and where its boundaries lie. On this
 *   reading the Reformation enters history as a climb: Luther's recovery of
 *   justification by faith alone is a genuine doctrinal breakthrough whose
 *   truth required institutional separation from Rome. The standing
 *   arrangement under contest - the arrangement this story is about - is the
 *   confessional-historiographical regime organized by that claim: a tight
 *   1517-1555 periodization, Wittenberg-centered narrative custody, the
 *   Catholic Church positioned as the corrected object, and evangelical
 *   believers positioned as the freed beneficiaries. Per the epsilon-referent
 *   rule, the referent is that standing arrangement as this reading sees it,
 *   never the reading's endorsed alternative. The sibling readings (political
 *   swap, composite overdetermination) are separate constraints in separate
 *   files; they appear here only as committer structure routed to omega
 *   variables and reading_relations. Claim and metrics are independent: the
 *   arrangement is CLAIMED as tangled_rope - genuine confessional
 *   coordination plus asymmetric extraction - and the metrics describe its
 *   actual operation as the historical record shows it. The expected
 *   structural delta is honored throughout: climb entry, Catholic
 *   victim-of-correction positioning, believers as beneficiaries, tight
 *   periodization.
 *
 * KEY AGENTS:
 *   - wittenberg_reformers: agenda-setting doctrinal authority (institutional/identity_locked) - produces the account and polices its boundaries against Rome and against radicals
 *   - evangelical_territorial_princes: primary enforcement beneficiary (powerful/constrained) - converts the theological claim into binding territorial law and collects jurisdiction
 *   - evangelical_lay_believers: declared beneficiaries (organized/identity_locked) - receive vernacular scripture and the liberation narrative; carry compulsory conformity
 *   - catholic_church_hierarchy: primary target (institutional/constrained) - recast as the corrected object; loses narrative standing and territory
 *   - radical_reformers_anabaptists: secondary target (powerless/trapped) - rival readers of the same kernel, defined as disorder and crushed
 *   - emperor_charles_v_imperial_estates: administering authority (institutional/constrained) - bans, diets, war, and finally the Augsburg settlement that freezes the arrangement into law
 *   - humanist_critic_circle: observer (moderate/mobile) - supplied the philological tools, declined the certitude
 *   - confessional_chroniclers: narrative beneficiaries (moderate/constrained) - write the event into the climb shape
 *   - german_peasantry_post_1525: excluded voice (powerless/trapped) - gospel-based grievances ruled out of order by both confessions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.62).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.7).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "Theological Climb Reading of the Reformation Event Boundary").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, '1f127683-bef1-4fd7-a4f5-bc256e6f18ce').
narrative_ontology:cs_kernel_codification('1f127683-bef1-4fd7-a4f5-bc256e6f18ce', fixed_text).
narrative_ontology:cs_authority_grounding('1f127683-bef1-4fd7-a4f5-bc256e6f18ce', lineage).
narrative_ontology:cs_interpretation_layer_present('1f127683-bef1-4fd7-a4f5-bc256e6f18ce').
narrative_ontology:cs_reading_relation('1f127683-bef1-4fd7-a4f5-bc256e6f18ce', reformation_event_boundary__political_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('1f127683-bef1-4fd7-a4f5-bc256e6f18ce', reformation_event_boundary__composite_overdetermination_reading, forecloses).
narrative_ontology:cs_axiom('1f127683-bef1-4fd7-a4f5-bc256e6f18ce', foundational, justification_by_faith_alone_is_gospel_core).
narrative_ontology:cs_axiom_status(justification_by_faith_alone_is_gospel_core, holdable).
narrative_ontology:cs_axiom_grounding('1f127683-bef1-4fd7-a4f5-bc256e6f18ce', justification_by_faith_alone_is_gospel_core, theological).
narrative_ontology:cs_axiom('1f127683-bef1-4fd7-a4f5-bc256e6f18ce', foundational, doctrinal_truth_requires_institutional_separation).
narrative_ontology:cs_axiom_status(doctrinal_truth_requires_institutional_separation, holdable).
narrative_ontology:cs_axiom_grounding('1f127683-bef1-4fd7-a4f5-bc256e6f18ce', doctrinal_truth_requires_institutional_separation, instrumental).
narrative_ontology:cs_reference_frame('1f127683-bef1-4fd7-a4f5-bc256e6f18ce', restored_apostolic_gospel_norm).
narrative_ontology:cs_drift_state('1f127683-bef1-4fd7-a4f5-bc256e6f18ce', post_augsburg_confessional_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1f127683-bef1-4fd7-a4f5-bc256e6f18ce', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, wittenberg_reformers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, evangelical_territorial_princes).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, evangelical_lay_believers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, confessional_chroniclers).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, catholic_church_hierarchy).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, radical_reformers_anabaptists).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, justification_by_faith_alone_doctrine).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, sola_scriptura_principle).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, priesthood_of_all_believers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% University theologians turned doctrinal authority: they produce the biblical translation, catechisms, and church orders that organize evangelical life, and they police the movement's boundaries in both directions - against Rome's teaching office and against radical readers who draw different conclusions from the same texts. Their scholarly and pastoral standing rests on the breakthrough account being true; abandoning it would unmake their life's work.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, wittenberg_reformers, agenda_setter,
    institutional, generational, identity_locked, continental).

% Territorial rulers who adopt the evangelical cause and receive, through the settlements that follow, legal cover for transferring church property and exercising jurisdiction over religion in their lands. They issue church ordinances, fund visitations, and defend the arrangement militarily when the emperor moves against it. Reverting to Rome would mean returning confiscated property and accepting renewed papal jurisdiction, so their room to maneuver narrows as their gains compound.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, evangelical_territorial_princes, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__theological_climb_reading, evangelical_territorial_princes, agenda_setter).

% Parishioners in evangelical territories: they receive vernacular scripture, catechetical instruction, married clergy, and an account of release from indulgence-driven fear. They also live under compulsory conformity - attendance, examination, discipline through visitations and consistories - and they inherit, in the second generation, an identity in which the old church is simply error. Open dissent risks condemnation from both confessions at once.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, evangelical_lay_believers, beneficiary,
    organized, biographical, identity_locked, continental).

% The papal and episcopal hierarchy whose teaching the evangelical movement repudiates. Inside the prevailing account it appears as the corrected object - the corrupt baseline against which the breakthrough is measured - which costs it narrative standing as well as territory, jurisdictions, and convent income. Its exits are narrow: it cannot abandon its doctrine, and its response runs through polemic, prohibition lists, and eventually the Council of Trent's internal reform.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, catholic_church_hierarchy, payer,
    institutional, generational, constrained, global).

% Movements that read the same scriptures to different conclusions - adult baptism, a voluntary gathered church, refusal of oaths and magistracy. The magisterial account defines them not as rival interpreters but as disorder to be suppressed; imperial mandates condemn them, the Munster episode of 1535 destroys their main concentration, and both confessions hunt them afterward. They hold no territory and have nowhere to go.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, radical_reformers_anabaptists, payer,
    powerless, biographical, trapped, continental).

% The emperor and the imperial estates who administer the legal envelope around the religious conflict: the Worms ban, the diets of Speyer and Augsburg, the war against the Schmalkaldic League, and finally the 1555 settlement letting each prince fix his territory's confession. Enforcement alternately suppresses the evangelical movement and, once concession comes, freezes its gains into imperial law.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, emperor_charles_v_imperial_estates, agenda_setter,
    institutional, generational, constrained, continental).

% Erasmus and allied scholars who supplied the philological tools - Greek text, patristic editions, satire on monastic abuse - that the evangelical argument drew on, then declined its certitude and its party discipline. They criticize both sides from a distance, moving between courts and universities; their mobility lets them opt out of the fight at comparatively little cost.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, humanist_critic_circle, observer,
    moderate, biographical, mobile, continental).

% Historians on both confessional sides - Sleidanus for the evangelicals, Cochlaeus and his counterparts for the old church - who write the conflict into durable narrative shape. Patronage and readership reward accounts serving their side's claim; their books become the record later generations consult.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, confessional_chroniclers, beneficiary,
    moderate, generational, constrained, continental).

% Rural commoners who appealed to the same scriptures against tithes, dues, and serfdom in 1524-1525 and were condemned by Luther as well as by their lords. After the defeat their grievances have no standing in any subsequent settlement: religion is allocated by princely decision, and the people who first raised a biblical claim against their economic burdens are absent from every table where it is discussed.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, german_peasantry_post_1525, excluded,
    powerless, immediate, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__theological_climb_reading, wittenberg_reformers).
narrative_ontology:fixing_cost_class(reformation_event_boundary__theological_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives scattered evangelical movements, territorial churches, and their sympathizers one organizing account: a doctrinal recovery (justification by faith alone) that explains why separation from Rome was necessary rather than rebellious. Around that spine it coordinates preaching, catechesis, church orders, Bible translation, and a shared calendar of commemoration, and it fixes a common periodization (1517-1555) for teaching and research.
% TRANSFER_FUNCTION: Moves interpretive authority and historical legitimacy from the Catholic Church's account of itself - recast inside the arrangement as the corrected object - to the Wittenberg-centered confessional complex; moves narrative custody of the event to the magisterial reformers and their chroniclers; transfers scholarly status from scholastic theology to the evangelical faculties; and, via the settlement the account legitimates, ratifies the transfer of church property and jurisdiction to evangelical princes.
% ABSENT_VOICES: German peasantry after 1525 - their appeal to the same scriptures against tithes and dues was ruled out of order by both sides, and they hold no seat in the settlement that allocates religion by principality. Radical reformers appear only as disorders to be policed, not as rival readers with standing. Eastern Orthodox churches are outside the conversation entirely. Catholic voices enter chiefly as the corrected party rather than as interlocutors.
% DISAPPEARANCE_RATIONALE: Confessional identity, church orders, seminary curricula, the legal logic of the Peace of Augsburg (confession fixed by territory), and the standard periodization of early modern Europe all presuppose the climb account; overnight removal would force wholesale reorganization of how the event is taught, commemorated, and legally administered.
% FOUNDING_PROBLEM: By what right do separated churches exist? The arrangement was built to answer that question: to show that separation from Rome followed from doctrinal necessity - the recovery of justification by faith alone - rather than from rebellion or princely ambition, and to give believers a coherent account of why the old teaching was false and the new one true.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties, with a stated limit: Catholic controversialists (Sadoleto, Contarini, the Council of Trent fathers) dispute the breakthrough's truth yet conduct the dispute as doctrinal argument, corroborating that the conflict's working currency was theological; imperial instruments (the Worms ban, the 1555 settlement) frame the matter as religion; and secular historians across the political and composite camps concede that theological vocabulary dominated contemporaries' self-understanding. What no outside source corroborates is the exclusivity claim - that theology was the PRIMARY driver - which is precisely this reading's distinguishing assertion.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_event_boundary__theological_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__theological_climb_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction ends at 0.62: the arrangement takes interpretive compliance from the historiographical field, imposes a fixed corrective role on the Catholic account, and absorbs the radical alternative into 'disorder,' while still delivering the confessional coordination that keeps it from being pure extraction. Suppression is higher (0.70) because persistence depended on coercive machinery - the Worms ban, the anti-Anabaptist mandates of 1528-1536, visitation discipline, and finally the Augsburg settlement's cuius-regio enforcement - not on participant preference. Theater stays low-to-moderate (0.34): disputation, confession-writing, and catechesis were functionally load-bearing; the performative share grows with the colloquies and settlement ceremonial but never dominates. Accessibility collapse is moderate (0.45): the arrangement narrowed the space of admissible accounts sharply but never collapsed it - political and composite accounts remained thinkable and were held, inside the interval by Catholic controversialists and worldly courtiers, after it by whole historiographical schools. Resistance is substantial (0.60): Catholic counter-mobilization, radical dissent, and humanist skepticism met the arrangement continuously. The measurement series share one eight-point grid (1517, 1521, 1525, 1530, 1535, 1541, 1547, 1555) so every metric is authored at every examined time point; the 1541 dips reflect the Regensburg Colloquy's genuine thaw - external diplomatic weather, not intermittent reinforcement - and the underlying trajectory is monotonic hardening from open debate to confessional police.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda/beneficiary seats compute differently. From Wittenberg the arrangement is providential clarification - the costly but necessary form of truth-telling. From the Catholic seat the same structure is a usurpation narrative that strips the Church of its own account of itself while casting it as the villain of the piece. From the radical seat it is the magisterial captivity of the gospel - the breakthrough betrayed by its own custodians within a decade of Worms. The princes experience a fourth thing: a title deed. The engine derives these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the Wittenberg complex, the princes, the chroniclers, and (with less force) the lay believers; victim declarations drive high directionality for the Catholic hierarchy and the radicals. Two nuances the declarations carry but cannot fully resolve. First, evangelical lay believers are declared beneficiaries and genuinely receive the arrangement's goods, but they also bear compulsory conformity and war burdens - their true position sits nearer symmetric than a pure beneficiary's, and the identity_locked exit data pulls their computed directionality upward accordingly; I did not author a directionality override because the available override granularity is per power atom, and an override at the believers' atom would misapply to other agents sharing it. Second, the vindicated propositions (justification by faith alone, sola scriptura, priesthood of all believers) collect no rents and are deliberately kept out of the beneficiary list - the arrangement vindicates them, but doctrines are not actors. On the receipt surface: the gains the arrangement generates - custody of the event's meaning, doctrinal authority, the right to define who counts as a faithful reader - demonstrably accrue to the Wittenberg center and its confessional successors, so gain_flow names that seat; the princes' gains run through the political channel that the swap sibling's story would record. Fixing the arrangement - unwriting the breakthrough account from confessional identity, church order, and the imperial settlement - is prohibitive for anyone positioned to attempt it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - by what right do separated churches exist - is live: confessional communities still run on the answer, and the historiographical contest over the event's nature is open. Nothing here is atrophied or theatrically maintained, so no mandatrophy is declared. The tangled_rope classification is what prevents mislabeling in both directions: reading the arrangement as pure extraction (snare) would erase the genuine confessional coordination - catechesis, translation, church order - that millions entered voluntarily and that outlasted enforcement decay; reading it as pure coordination (rope) would erase the suppressed alternatives, the corrected-object burden placed on the Catholic account, and the coercive machinery the settlement froze into law. The hybrid is the honest structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the reformation_event_boundary kernel; what would the structural delta be if the political_swap_reading or composite_overdetermination_reading were adopted instead?',
    'Cross-framing falsification: recompute beneficiary/victim assignments, periodization, and epsilon under each sibling reading and test whether the climb reading''s assignments survive the switch.',
    'Under political_swap_reading the victims become dispossessed Catholic institutions and the taxed laity, the beneficiaries become asset-acquiring princes, and the periodization widens; under composite_overdetermination_reading no single victim/beneficiary axis is admissible and the tight 1517-1555 window fails. Either outcome would rewrite this story''s structural data.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Committer structure: this story instantiates one reading of a contested kernel; sibling readings change the structural delta.').

omega_variable(
    tight_periodization_load_bearing,
    'Is the tight 1517-1555 periodization descriptive of the phenomenon or load-bearing for the primacy claim - does theological primacy survive a widened window (roughly 1450-1600) that admits Wycliffe and Hus precursors, the radical continuation past 1555, and the longer Catholic Reformation?',
    'Re-run the causal-attribution analysis under widened windows; test whether the breakthrough narrative retains primacy when medieval dissident lineages and post-1555 confessional consolidation are admitted inside the boundary.',
    'If primacy holds only inside the tight window, the periodization is doing causal work rather than reporting the event''s extent - raising measured extraction (suppressed continuities) and pushing the classification toward the extractive end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tight_periodization_load_bearing, empirical, 'Whether the reading''s periodization reports the event''s extent or manufactures it.').

omega_variable(
    breakthrough_recovery_vs_synthesis,
    'Was justification by faith alone a genuine exegetical recovery of prior apostolic teaching, or a novel synthesis assembled from Augustinian, mystical, and humanist-philological materials and retrospectively sacralized as rediscovery?',
    'Intellectual-history tracing of the argument''s genealogy: Augustine''s anti-Pelagian texts, German mysticism (Tauler, the Theologia Deutsch), Erasmus''s Greek New Testament, versus the actual novelty of Luther''s 1515-1518 lectures and the 1520 treatises.',
    'If synthesis rather than recovery, the climb metaphor weakens - the reading drifts from discovered truth toward constructed arrangement, and the authority claim''s naturalness comes into question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(breakthrough_recovery_vs_synthesis, empirical, 'Authenticity of the doctrinal breakthrough on which the reading''s authority rests.').

omega_variable(
    believer_benefit_symmetry,
    'Are evangelical lay believers net beneficiaries of the arrangement, as the beneficiary declaration asserts, or nearer symmetric bearers who received vernacular scripture and catechesis while carrying compulsory conformity, visitation discipline, and war burdens?',
    'Parish-level comparison of devotional uptake against compliance costs across evangelical territories 1525-1555; visitation records set against consistory penalty rolls.',
    'If symmetric, the believer seat''s effective directionality rises toward 0.5, reducing the coordination credit the arrangement earns from its beneficiary structure and shifting weight toward its extractive side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(believer_benefit_symmetry, empirical, 'Net position of the declared beneficiary seat inside the arrangement.').

omega_variable(
    internalized_confessional_suppression,
    'How much of the arrangement''s suppressive force after the settlement is structural (consistories, visitations, princely mandate) versus internalized (second-generation confessional identity for which dissent is unthinkable)?',
    'Post-1555 trajectory: if dissent remains absent in territories where coercive capacity declined, the residual silence indicates internalized suppression carried by the population itself.',
    'If substantially internalized, the arrangement''s effective suppression exceeds its structural measure and persists beyond enforcement decay - relevant to any post-interval extension of this story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_confessional_suppression, empirical, 'Structural versus internalized mechanism of the arrangement''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reformation_climb_tr_t1517, reformation_event_boundary__theological_climb_reading, theater_ratio, 1517, 0.15).
narrative_ontology:measurement_basis(reformation_climb_tr_t1517, observed).
narrative_ontology:measurement(reformation_climb_tr_t1521, reformation_event_boundary__theological_climb_reading, theater_ratio, 1521, 0.2).
narrative_ontology:measurement_basis(reformation_climb_tr_t1521, observed).
narrative_ontology:measurement(reformation_climb_tr_t1525, reformation_event_boundary__theological_climb_reading, theater_ratio, 1525, 0.22).
narrative_ontology:measurement_basis(reformation_climb_tr_t1525, observed).
narrative_ontology:measurement(reformation_climb_tr_t1530, reformation_event_boundary__theological_climb_reading, theater_ratio, 1530, 0.27).
narrative_ontology:measurement_basis(reformation_climb_tr_t1530, observed).
narrative_ontology:measurement(reformation_climb_tr_t1535, reformation_event_boundary__theological_climb_reading, theater_ratio, 1535, 0.29).
narrative_ontology:measurement_basis(reformation_climb_tr_t1535, observed).
narrative_ontology:measurement(reformation_climb_tr_t1541, reformation_event_boundary__theological_climb_reading, theater_ratio, 1541, 0.33).
narrative_ontology:measurement_basis(reformation_climb_tr_t1541, observed).
narrative_ontology:measurement(reformation_climb_tr_t1547, reformation_event_boundary__theological_climb_reading, theater_ratio, 1547, 0.31).
narrative_ontology:measurement_basis(reformation_climb_tr_t1547, observed).
narrative_ontology:measurement(reformation_climb_tr_t1555, reformation_event_boundary__theological_climb_reading, theater_ratio, 1555, 0.34).
narrative_ontology:measurement_basis(reformation_climb_tr_t1555, observed).

% Extraction over time
narrative_ontology:measurement(reformation_climb_be_t1517, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1517, 0.42).
narrative_ontology:measurement_basis(reformation_climb_be_t1517, observed).
narrative_ontology:measurement(reformation_climb_be_t1521, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1521, 0.48).
narrative_ontology:measurement_basis(reformation_climb_be_t1521, observed).
narrative_ontology:measurement(reformation_climb_be_t1525, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1525, 0.53).
narrative_ontology:measurement_basis(reformation_climb_be_t1525, observed).
narrative_ontology:measurement(reformation_climb_be_t1530, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1530, 0.57).
narrative_ontology:measurement_basis(reformation_climb_be_t1530, observed).
narrative_ontology:measurement(reformation_climb_be_t1535, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1535, 0.6).
narrative_ontology:measurement_basis(reformation_climb_be_t1535, observed).
narrative_ontology:measurement(reformation_climb_be_t1541, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1541, 0.58).
narrative_ontology:measurement_basis(reformation_climb_be_t1541, observed).
narrative_ontology:measurement(reformation_climb_be_t1547, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1547, 0.64).
narrative_ontology:measurement_basis(reformation_climb_be_t1547, observed).
narrative_ontology:measurement(reformation_climb_be_t1555, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1555, 0.62).
narrative_ontology:measurement_basis(reformation_climb_be_t1555, observed).

% Suppression requirement over time
narrative_ontology:measurement(reformation_climb_su_t1517, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement_basis(reformation_climb_su_t1517, observed).
narrative_ontology:measurement(reformation_climb_su_t1521, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1521, 0.45).
narrative_ontology:measurement_basis(reformation_climb_su_t1521, observed).
narrative_ontology:measurement(reformation_climb_su_t1525, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1525, 0.6).
narrative_ontology:measurement_basis(reformation_climb_su_t1525, observed).
narrative_ontology:measurement(reformation_climb_su_t1530, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1530, 0.62).
narrative_ontology:measurement_basis(reformation_climb_su_t1530, observed).
narrative_ontology:measurement(reformation_climb_su_t1535, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1535, 0.72).
narrative_ontology:measurement_basis(reformation_climb_su_t1535, observed).
narrative_ontology:measurement(reformation_climb_su_t1541, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1541, 0.65).
narrative_ontology:measurement_basis(reformation_climb_su_t1541, observed).
narrative_ontology:measurement(reformation_climb_su_t1547, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1547, 0.75).
narrative_ontology:measurement_basis(reformation_climb_su_t1547, observed).
narrative_ontology:measurement(reformation_climb_su_t1555, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1555, 0.7).
narrative_ontology:measurement_basis(reformation_climb_su_t1555, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__theological_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Reformation' decomposes, per the epsilon-invariance principle, into three structurally distinct claims with different epsilon values, victim sets, and periodizations. This file is the theological climb member; reformation_event_boundary__political_swap_reading and reformation_event_boundary__composite_overdetermination_reading are the siblings. The climb reading is historically upstream - its archive, its periodization, and its role assignments are what the sibling readings argue against - so its edges point to both siblings. Both edges are typed forecloses: a holder of the climb reading is logically committed to denying that the theology was post-hoc rationalization (the swap reading's core premise) and to denying that no single driver or periodization captures the event (the composite reading's core premise); the siblings therefore survive across parties, not within any climb-held framework. The composite member is additionally reflexive - it is a reading about the contest among readings - which is why it persists as a scholarly position despite the foreclosure edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
