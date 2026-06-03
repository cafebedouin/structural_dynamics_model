% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reformation_event_boundary__theological_climb_reading
 *   human_readable: The Reformation as Theological Climb (1517–1555)
 *   domain: religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the theological_climb_reading of the
 *   contested kernel 'reformation_event_boundary.' The reading frames the
 *   Reformation as primarily a theological innovation event: Luther's
 *   rediscovery of justification by faith alone (sola fide) constitutes a
 *   genuine doctrinal breakthrough that reinterprets the authority structure
 *   grounding Christian practice. In this reading, the theological innovation
 *   is the primary causal force; institutional separation (Protestant vs.
 *   Catholic churches) is a necessary downstream consequence of theological
 *   incompatibility rather than a political choice rationalized post-hoc by
 *   religious language. The constraint operates within the period 1517–1555
 *   (Luther's 95 Theses through the Peace of Augsburg), during which the
 *   theological innovation crystallizes into institutional differentiation.
 *   The beneficiaries are believers freed from false doctrine (the constraint
 *   coordinates authentic access to God's grace); the victim is the Catholic
 *   institutional authority whose doctrinal monopoly is broken and whose
 *   sacramental mediation is declared unnecessary. The constraint exhibits
 *   tangled_rope structure: it both coordinates genuine spiritual goods
 *   (direct faith access) AND extracts from the Catholic institutional
 *   authority whose legitimacy is delegitimized. The theater_ratio increases
 *   over the interval (0.35 → 0.45) because as the Reformation becomes
 *   institutionalized, the theological narrative increasingly serves as
 *   pedagogical frame and identity marker rather than active theological
 *   dispute—by 1555, reformed theology is transmitted as settled doctrine
 *   rather than as lived interpretive breakthrough.
 *
 * KEY AGENTS:
 *   - Martin Luther and Reformed Exegetes: Primary innovators (organized/constrained) — authors of the theological breakthrough; beneficiaries of new intellectual framework
 *   - Protestant Believers: Primary beneficiaries (powerless/trapped) — experience the constraint as liberation from false doctrine and access to divine grace; victim of doctrinal displacement if formerly Catholic
 *   - Catholic Institutional Authority: Primary victim (institutional/trapped) — sacramental authority and clerical mediation delegitimized; responds with maximum suppression (excommunication, doctrinal prohibition)
 *   - Secular Rulers: Secondary beneficiary (powerful/mobile) — exploit the doctrinal split to seize church assets and break papal authority; experience the constraint as temporary structural opening (scaffold perspective)
 *   - Scholastic Theological Orthodoxy: Institutional victim (analytical/analytical) — entire framework of theological method (dialectical synthesis, natural law reasoning) displaced by reformed emphasis on scriptural literalism and sola scriptura
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.38).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.62).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "The Reformation as Theological Climb (1517–1555)").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, 'b1429a59-a81c-4fda-b6b7-396158f34bcd').
narrative_ontology:cs_kernel_codification('b1429a59-a81c-4fda-b6b7-396158f34bcd', fixed_text).
narrative_ontology:cs_authority_grounding('b1429a59-a81c-4fda-b6b7-396158f34bcd', lineage).
narrative_ontology:cs_interpretation_layer_present('b1429a59-a81c-4fda-b6b7-396158f34bcd').
narrative_ontology:cs_reading_relation('b1429a59-a81c-4fda-b6b7-396158f34bcd', reformation_event_boundary__political_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('b1429a59-a81c-4fda-b6b7-396158f34bcd', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('b1429a59-a81c-4fda-b6b7-396158f34bcd', foundational, theological_innovation_genuine).
narrative_ontology:cs_axiom_status(theological_innovation_genuine, holdable).
narrative_ontology:cs_axiom_grounding('b1429a59-a81c-4fda-b6b7-396158f34bcd', theological_innovation_genuine, empirically_contingent).
narrative_ontology:cs_axiom('b1429a59-a81c-4fda-b6b7-396158f34bcd', foundational, institutional_separation_necessary).
narrative_ontology:cs_axiom_status(institutional_separation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('b1429a59-a81c-4fda-b6b7-396158f34bcd', institutional_separation_necessary, deontological).
narrative_ontology:cs_reference_frame('b1429a59-a81c-4fda-b6b7-396158f34bcd', patristic_scriptural_authority).
narrative_ontology:cs_drift_state('b1429a59-a81c-4fda-b6b7-396158f34bcd', scholastic_mediation_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b1429a59-a81c-4fda-b6b7-396158f34bcd', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, protestant_believers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, reformed_exegetes).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, catholic_institutional_authority).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, scholastic_theological_orthodoxy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CATHOLIC BELIEVER (TANGLED ROPE) — Faces extraction through doctrinal displacement: the believer's faith framework (sacramental mediation, clergy intercession) is declared false, delegitimizing their lived religious practice. Yet the constraint also coordinates genuine spiritual goods: direct access to God's grace through faith alone genuinely answers the question of assurance that scholastic theology obscured. Trapped by geography and social structure but experiencing the constraint as both extraction and authentic theological correction.
constraint_indexing:constraint_classification(reformation_event_boundary__theological_climb_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: PROTESTANT EXEGETE (ROPE) — Pure coordination mechanism: the constraint enables collaborative reinterpretation of scripture according to the breakthrough insight (sola fide). Benefits from shared intellectual framework, access to like-minded scholars, and the legitimacy of defending a coherent doctrinal position. Modest extraction through doctrinal policing (enforcement of reformed theology against competing interpretations) but primarily experienced as liberation and intellectual coordination. Constrained by risk of persecution but benefiting from organizational support.
constraint_indexing:constraint_classification(reformation_event_boundary__theological_climb_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CATHOLIC INSTITUTIONAL AUTHORITY (SNARE) — Maximum extraction and suppression. The theological climb delegitimizes the institutional Church's entire authority structure (sacramental mediation, clerical hierarchy, papal supremacy). The institutional response is pure suppression: doctrinal prohibition, excommunication, enforcement of Latin Vulgate authority, suppression of vernacular scripture reading. Exit is impossible for the institution — survival requires closing all exits to the competing framework. The institutional Church experiences this constraint as existential threat and responds with maximum coercive force.
constraint_indexing:constraint_classification(reformation_event_boundary__theological_climb_reading, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 4: SECULAR RULER (SCAFFOLD) — Experiences the theological constraint as a temporary structural opening with sunset logic: the doctrinal dispute creates a window in which secular rulers can seize church assets, break papal authority, and establish territorial churches without appearing to drive the change (theology is the motive force). The constraint has inherent sunset: once denominational settlement is achieved (Peace of Augsburg, 1555), the structural opening closes and secular rulers no longer need the theological framework to justify asset seizure. Mobile agents can exit once the territorial settlement is complete.
constraint_indexing:constraint_classification(reformation_event_boundary__theological_climb_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: HISTORICAL INTERPRETATION (PITON) — The theological climb reading itself becomes performative at historical timescale: historians use 'Luther's breakthrough' as an explanatory placeholder for complex overdetermined causality (economic upheaval, printing technology, political fragmentation, theological ferment). The narrative persists through institutional inertia (it is the dominant pedagogical frame in historical survey texts) despite the recognition among specialists that monocausal theological explanation is insufficient. Theater ratio indicates that the reading is sustained more by narrative coherence and textbook reproduction than by robust causal evidence.
constraint_indexing:constraint_classification(reformation_event_boundary__theological_climb_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: THEOLOGICAL TRUTH VIEW (MOUNTAIN) — From a theological perspective internal to the reformed tradition, the doctrine of justification by faith alone represents a recovered truth (sola scriptura, sola fide, sola gratia) that was always binding regardless of historical institutional distortion. This perspective treats the theological claim as a mountain: unchangeable truth independent of social context, emerging necessarily once scripture is read correctly. However, this perspective risks naturalizing a contingent historical reading as eternal doctrine.
constraint_indexing:constraint_classification(reformation_event_boundary__theological_climb_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reformation_event_boundary__theological_climb_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reformation_event_boundary__theological_climb_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, TR),
    TR >= 0.70.

:- end_tests(reformation_event_boundary__theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from the Catholic institutional authority (whose monopoly is broken) but coordinates genuine theological goods for believers. The extractiveness is not low (rope-level, ≤0.35) because the beneficiaries gain their theological clarity partly through the delegitimization of the Catholic alternative—there is an asymmetric distribution of benefit and cost. Extractiveness is not high (snare-level, ≥0.46) because the theology genuinely answers theological questions (assurance, grace, justification) that the scholastic framework left unresolved, so the coordination function is not merely performative. Suppression (0.62): High. The Catholic institutional response is maximum suppression: doctrinal prohibition of sola fide theology, excommunication of reformers, Latin Vulgate authority enforcement, suppression of vernacular scripture reading, mobilization of secular rulers to crush reform movements (Diet of Worms, execution of reformers). The suppression is not because the theology is weak but because the institutional threat is existential—the Catholic Church's entire authority structure (sacramental mediation, clerical hierarchy, papal supremacy) depends on rejecting sola fide. Theater ratio (0.45): Moderate-low and increasing. At the initial moment (1517), the constraint is primarily functional: Luther is engaged in active theological dispute and scriptural reinterpretation. As the Reformation becomes institutionalized, the theological narrative increasingly serves as identity marker and pedagogical frame rather than lived dispute. By 1555 (the interval endpoint), reformed theology is transmitted as settled doctrine (theater_ratio 0.45) rather than as active breakthrough.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the contested nature of the kernel. The reformed exegete (rope perspective) sees the constraint as pure coordination: shared intellectual breakthrough enabling collaborative reinterpretation. The Catholic institutional authority (snare perspective) sees existential extraction: delegitimization of the entire authority structure requiring maximum suppressive response. The secular ruler (scaffold perspective) sees a temporary structural opening that sunset once denominational settlement is achieved. The Catholic believer (tangled_rope perspective) experiences both extraction (delegitimization of their lived faith practice) and coordination (genuine theological clarity on assurance). The historical interpretation (piton perspective) sees a degraded narrative frame sustained by textbook reproduction despite recognition that monocausal theological explanation is insufficient. The theological truth view (mountain perspective) treats sola fide as an eternal recovered doctrine independent of historical institutional contingency. These perspectival gaps reveal that the theological_climb_reading privileges the viewpoint of the theological innovators and their beneficiaries; it marginalizes the perspective of those whose doctrinal authority is delegitimized and whose lived practices are displaced. The sibling political_swap_reading would privilege the secular rulers' perspective; the composite_overdetermination_reading would dissolve the perspectival gaps by treating all causal factors as irreducibly multiple.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives its directionality from the agent's structural position relative to the theological constraint. Beneficiaries (protestant_believers, reformed_exegetes) have low d-values (0.1–0.3): they are the targets of the innovation's benefits and experience low or negative effective extraction. The Catholic institutional authority has the highest d-value (0.95): it is the primary target bearing the cost of doctrinal delegitimization; it has no exit option (trapped) and experiences maximum extraction. Secular rulers have moderate d-values (0.55–0.65): they benefit from the structural opening the doctrinal dispute creates but are not the primary agents driving the innovation—they are opportunistic beneficiaries rather than core drivers. The analytical observer derives d from the framework of analysis: if analyzing as a theological historian, d is driven by the historian's commitment to a particular causal narrative; if analyzing as a structural analyst, d reflects the observer's position relative to competing interpretive traditions. The piton and mountain perspectives have d-values consistent with analytical power (0.72–0.75).
 *
 * MANDATROPHY ANALYSIS:
 *   The theological_climb_reading resolves mandatrophy by grounding the beneficiary/victim asymmetry in the genuine theological innovation. The Catholic institutional authority is the victim not because of extraction dynamics (pure snare logic) but because its doctrinal monopoly is broken by a genuine theological innovation that the framework itself acknowledges as superior (within the reformed tradition) or at least coherent (from the analytical perspective). This avoids the mandatrophy of claiming that the Reformation is simultaneously (1) a genuine theological breakthrough answering unsolved questions and (2) pure extraction. The tangled_rope classification captures this: the constraint both coordinates theological goods (sola fide answers the question of assurance) AND extracts from the institutional authority (its mediation claim is delegitimized). The sibling readings would resolve mandatrophy differently: the political_swap_reading would classify the entire event as snare (pure extraction using theology as cover story); the composite_overdetermination_reading would maintain that theological innovation, political extraction, institutional change, and denominational proliferation are all necessary factors, none reducible to the others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_novelty_vs_recovery,
    'Did Luther discover/recover an eternal theological truth, or did he innovate a new doctrinal reading that reinterpreted existing sources?',
    'Comparative textual analysis: identification of whether patristic sources (Augustine, Aquinas, Ockham) actually support sola fide interpretation or whether Luther''s reading is genuinely novel; historical-comparative study of contemporary reformers'' exegetical methods',
    'If recovery: the constraint is a correction mechanism moving toward truth (beneficiary-victim framing valid). If innovation: the constraint is a perspectival shift that delegitimizes an alternative interpretive tradition (victims'' perspective becomes analytically central). Classification type remains tangled_rope in both cases, but the axiom grounding shifts from theological_truth_recovered to theological_perspectival_innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_novelty_vs_recovery, empirical, 'Whether sola fide is doctrinal recovery or innovation').

omega_variable(
    institutional_separation_necessity,
    'Did the theological innovation logically require institutional separation, or was institutional separation a contingent political choice?',
    'Counterfactual historical analysis: examination of whether reform movements within the Catholic framework (prior reform councils, conciliarism, Observant movements) could have institutionalized sola fide reinterpretation; comparative study of non-separatist reform traditions (Erasmianism, mysticism) that held similar theological insights without schism',
    'If logically required: separation is downstream of genuine theological incompatibility (climb reading strengthened). If contingent: separation was a political choice rationalized by theology (composite_overdetermination reading strengthened; political_swap reading gains credibility). Extractiveness would shift from 0.38 to 0.55+ if separation is reframed as political rather than theological necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_separation_necessity, conceptual, 'Whether institutional separation was logically required by the theological innovation').

omega_variable(
    periodization_tightness,
    'Is the Reformation best understood as a discrete event (1517–1555) or as a multi-generational process of gradual institutional differentiation extending into the 17th and 18th centuries?',
    'Historiographical analysis of when ''the Reformation'' is treated as complete (Council of Trent? Peace of Augsburg? End of Wars of Religion? Enlightenment?); examination of whether tight periodization (1517–1555) corresponds to observable institutional crystallization or is a narrative convenience',
    'If tight periodization holds: the theological climb reading captures a bounded historical event with clear causality. If gradual: the constraint extends across centuries and becomes overdetermined (composite reading gains strength). The tight periodization assumed by the theological_climb_reading implies that the primary work of institutional separation was completed by 1555; if the work extends further, this reading captures only the initial phase.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(periodization_tightness, conceptual, 'Whether the Reformation is a discrete 1517–1555 event or multi-generational process').

omega_variable(
    beneficiary_class_scope,
    'Who are the true beneficiaries of the theological innovation: individual believers freed from false doctrine, or educated urban elites (clergy, merchants, nobles) who gain cultural authority through reformed theology?',
    'Sociological analysis of Reformation adoption patterns: correlation of reform acceptance with literacy, urbanization, and elite status; examination of whether peasants and rural populations experienced sola fide as liberation or as cultural displacement; analysis of whose lives materially improved under reformed regimes',
    'If individual believers universally: the beneficiary framing is correct and the constraint coordinates genuine spiritual goods. If primarily elite: the constraint is better understood as an elite power consolidation using theological language (extraction increases, victims expand to include non-elite populations displaced by reform); composite reading gains strength.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_class_scope, empirical, 'Whether theological innovation benefited all believers or primarily educated elites').

omega_variable(
    kernel_reading_contest_irreducibility,
    'Are the theological_climb_reading, political_swap_reading, and composite_overdetermination_reading genuinely distinct readings of a single kernel, or do they represent independent competing historical claims?',
    'Clarification of the kernel itself: what is the stabilized commitment under contest? If the kernel is ''the nature and causes of the Reformation,'' then all three readings compete. If the kernel is ''the authority structure of the Christian church,'' then readings may decompose into distinct constraints. Determination requires committer-level analysis of what ground of legitimacy each reading invokes and whether they share a common adjudicatory framework.',
    'If genuinely a single kernel: all three readings should be linked as sibling readings with reading_relations populated accurately. If competing independent claims: the kernel framing is incorrect and each reading should be authored as a separate constraint family. This omega documents the committer-frame uncertainty built into the kernel context itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_irreducibility, conceptual, 'Whether three readings represent a single contested kernel or independent historical claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_event_boundary__theological_climb_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(refo_tr_t1, reformation_event_boundary__theological_climb_reading, theater_ratio, 1, 0.4).
narrative_ontology:measurement(refo_tr_t2, reformation_event_boundary__theological_climb_reading, theater_ratio, 2, 0.45).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_event_boundary__theological_climb_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(refo_be_t1, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1, 0.32).
narrative_ontology:measurement(refo_be_t2, reformation_event_boundary__theological_climb_reading, base_extractiveness, 2, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__theological_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__theological_climb_reading, 0.15).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The kernel reformation_event_boundary decomposes into three structurally distinct readings: theological_climb_reading (this constraint), political_swap_reading, and composite_overdetermination_reading. Each reading instantiates a different causal narrative with different ε values (theological_climb 0.38, political_swap estimated 0.55+, composite estimated 0.72+). This constraint links to its sibling readings via network.affects_constraints to document the kernel contest structure. The decomposition reflects the ε-invariance principle: if measuring the Reformation's primary causal driver (theology vs. politics vs. composite) changes the extractiveness value, then the constraints are structurally distinct and should be authored separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
