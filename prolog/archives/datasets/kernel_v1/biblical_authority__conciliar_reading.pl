% ============================================================================
% CONSTRAINT STORY: biblical_authority__conciliar_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: biblical_authority__conciliar_reading
 *   human_readable: Biblical Authority via Ecumenical Councils and Patristic Consensus (Conciliar Reading)
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   The conciliar reading of biblical authority interprets Scripture through
 *   the lens of ecumenical councils and patristic consensus, treating the
 *   Fathers' collective voice (not papal decree, not individual study) as the
 *   authoritative mediator between text and doctrine. This reading emerged
 *   from the early Christian epistolary tradition and crystallized at the
 *   Council of Nicaea (325 CE) and subsequent ecumenical councils. It
 *   represents one of three major institutional readings of biblical
 *   authority within Christianity: sola scriptura (Scripture alone, no
 *   magisterial mediation), tradition-and-scripture (Scripture in continuity
 *   with living teaching authority, primarily Catholic/papal), and
 *   conciliar-reading (Scripture interpreted through councils and patristic
 *   consensus, primarily Orthodox and some Anglican/Reformed traditions). The
 *   conciliar reading is a genuine commitment-system constraint: it grounds
 *   ecclesiastical authority in a stabilized kernel (the corpus of patristic
 *   writings and council canons) and maintains legitimacy through
 *   interpretive continuity narratives (living tradition, organic
 *   development, apostolic succession through episcopal collegiality). The
 *   constraint exhibits tangled-rope characteristics: it provides genuine
 *   coordination function (harmonizing doctrine across autocephalous
 *   churches, preserving doctrinal continuity, solving the collective action
 *   problem of interpreting Scripture without a single magisterial center)
 *   while simultaneously extracting authority from lay faithful (who cannot
 *   propose doctrine) and from rapidly innovating theological movements
 *   (which councils can condemn as heretical). The theater ratio has risen
 *   over time (0.38 → 0.58) as academic patristics has become autonomous from
 *   ecclesiastical authority, creating performative continuity with
 *   historical conciliar readings while actual theological content diverges
 *   from those readings. The suppression requirement has also risen (0.42 →
 *   0.50) as churches maintain doctrinal uniformity despite increasing
 *   intellectual exposure to alternatives and increasing lay literacy.
 *
 * KEY AGENTS:
 *   - Episcopal Collegiality: Primary beneficiary (institutional/arbitrage) — maintains authority over doctrinal interpretation through collective conciliar mechanism; experiences constraint as coordination of their own institutional interests
 *   - Lay Faithful: Primary victim (powerless/identity_locked) — structurally mobile but identity-locked through sacramental participation and confessional belonging; cannot propose or challenge doctrine without exclusion
 *   - Theological Scholars: Secondary actor (moderate/constrained) — gain expertise-based authority to interpret patristic corpus but only within bishop-endorsed boundaries; constrained by council participation requirements and doctrinal gatekeeping
 *   - Autocephalous Churches: Institutional actor (institutional/constrained) — benefit from conciliar coordination (maintaining communion across jurisdictions) but constrained by council authority and slow consensus-building due to fragmentation
 *   - Rapid Doctrinal Innovation: Victim (powerless/trapped) — novel theological movements (Arianism, Nestorianism, Monophysitism, Iconoclasm, Hesychasm variants) face council condemnation and suppression; no institutional mechanism for legitimizing innovation outside council frameworks
 *   - Academic Patristics Discipline: Piton-perspective actor (institutional/analytical) — maintains performative continuity with historical conciliar authority while actual scholarly consensus diverges; constraint persists through theater, not enforcement
 *   - Ecumenical Movement: Organized actor (powerful/arbitrage) — sees conciliar reading as temporary bottleneck to be transcended through inter-confessional convergence on patristic hermeneutics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__conciliar_reading, 0.38).
domain_priors:suppression_score(biblical_authority__conciliar_reading, 0.48).
domain_priors:theater_ratio(biblical_authority__conciliar_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__conciliar_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__conciliar_reading, "Biblical Authority via Ecumenical Councils and Patristic Consensus (Conciliar Reading)").
narrative_ontology:topic_domain(biblical_authority__conciliar_reading, "theology/religious_studies/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__conciliar_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__conciliar_reading, '3ac06cb4-9c4d-451d-919b-eb68e45931f7').
narrative_ontology:cs_kernel_codification('3ac06cb4-9c4d-451d-919b-eb68e45931f7', fixed_text).
narrative_ontology:cs_authority_grounding('3ac06cb4-9c4d-451d-919b-eb68e45931f7', lineage).
narrative_ontology:cs_interpretation_layer_present('3ac06cb4-9c4d-451d-919b-eb68e45931f7').
narrative_ontology:cs_reading_relation('3ac06cb4-9c4d-451d-919b-eb68e45931f7', biblical_authority__sola_scriptura_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ac06cb4-9c4d-451d-919b-eb68e45931f7', biblical_authority__tradition_scripture_reading, coexists_with).
narrative_ontology:cs_axiom('3ac06cb4-9c4d-451d-919b-eb68e45931f7', foundational, episcopal_collegiality_constitutive).
narrative_ontology:cs_axiom_status(episcopal_collegiality_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('3ac06cb4-9c4d-451d-919b-eb68e45931f7', episcopal_collegiality_constitutive, conventional).
narrative_ontology:cs_axiom('3ac06cb4-9c4d-451d-919b-eb68e45931f7', foundational, patristic_consensus_preserves_apostolic_doctrine).
narrative_ontology:cs_axiom_status(patristic_consensus_preserves_apostolic_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('3ac06cb4-9c4d-451d-919b-eb68e45931f7', patristic_consensus_preserves_apostolic_doctrine, deontological).
narrative_ontology:cs_reference_frame('3ac06cb4-9c4d-451d-919b-eb68e45931f7', ecumenical_council_authority).
narrative_ontology:cs_drift_state('3ac06cb4-9c4d-451d-919b-eb68e45931f7', contemporary_scholarly_autonomy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3ac06cb4-9c4d-451d-919b-eb68e45931f7', '').
narrative_ontology:cs_kernel_id(biblical_authority__conciliar_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, episcopal_collegiality).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, orthodox_churches).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, lay_theological_agency).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, rapid_doctrinal_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAY FAITHFUL (SNARE) — Structurally mobile (could study theology independently) but identity-locked through confessional belonging and sacramental participation. Cannot propose doctrinal innovation without exclusion; bishops alone mediate conciliar interpretation. Maximum experienced extraction — doctrinal agency is foreclosed, and the mechanism is internalized identity fusion with hierarchical tradition.
constraint_indexing:constraint_classification(biblical_authority__conciliar_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: THEOLOGICAL SCHOLARS (TANGLED ROPE) — Moderate power through expertise; constrained by episcopal gatekeeping on council participation and doctrinal legitimacy. Coordinate interpretive activity within patristic corpus; extract value through access to authoritative tradition. Can propose readings but only within bishop-endorsed framework. Genuine coordination function (preserving consistency across centuries) mixed with asymmetric extraction (bishops hold final interpretive authority).
constraint_indexing:constraint_classification(biblical_authority__conciliar_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: EPISCOPAL COLLEGIALITY (ROPE) — Net beneficiary; experiences conciliar reading as pure coordination mechanism. Bishops solve the collective action problem of doctrinal consensus through conciliar assembly. Collective authority is maintained through patristic continuity narratives. Low extraction experienced because bishops see the constraint as legitimate coordination of their own institutional interest.
constraint_indexing:constraint_classification(biblical_authority__conciliar_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: AUTOCEPHALOUS CHURCHES (TANGLED ROPE) — Institutionally powerful but fragmented across jurisdictions (Constantinople, Alexandria, Antioch, Jerusalem, Moscow, etc.). Conciliar reading provides legitimacy for coordination (Ecumenical Councils) but enforces episcopal authority that constrains local adaptation. Moderate extraction: councils enable communion-maintenance but restrict unilateral doctrinal evolution. Different autocephalous churches experience different extractiveness depending on council representation and historical dominance patterns.
constraint_indexing:constraint_classification(biblical_authority__conciliar_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: ECUMENICAL MOVEMENT (SCAFFOLD) — Modern movement toward inter-confessional dialogue sees conciliar reading as a sunset mechanism. If separated confessions converge on shared patristic hermeneutics, ecumenical councils could be rebuilt with broader participation (Protestant, Catholic, Orthodox). Low effective extraction because this perspective sees the bottleneck as temporary — shared linguistic and interpretive frameworks could dissolve the episcopal monopoly on conciliar authority. Sunset clause is real but depends on convergence conditions that may never materialize.
constraint_indexing:constraint_classification(biblical_authority__conciliar_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ACADEMIC PATRISTICS DISCIPLINE (PITON) — Modern scholarly reconstruction of patristic consensus operates independently of ecclesiastical authority but maintains performative continuity with the conciliar reading. Scholars study the Fathers as if conciliar authority still governs; in practice, academic debate is unconstrained by episcopal gatekeeping. The constraint persists through theater: the ritualistic reference to patristic consensus legitimates academic work, but actual theological claims diverge from that consensus. Theater ratio is high because scholarly reconstruction often produces readings the historical Fathers would reject.
constraint_indexing:constraint_classification(biblical_authority__conciliar_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, conciliar reading appears as an immutable principle of theological authority: collective episcopal wisdom is inherently superior to individual interpretation, Scripture requires authoritative interpretive mediation, doctrinal stability necessitates hierarchical gatekeeping. This perspective sees the constraint as inevitable, unrevisable, and natural to how Christian tradition works. However, the structural data contradicts this — identifiable beneficiaries (episcopal collegiality, orthodox churches) and victims (lay agency, innovation) reveal this as a false summit: the 'inevitable' framing naturalizes what is contingent institutional architecture.
constraint_indexing:constraint_classification(biblical_authority__conciliar_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__conciliar_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biblical_authority__conciliar_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biblical_authority__conciliar_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(biblical_authority__conciliar_reading, TR),
    TR >= 0.70.

:- end_tests(biblical_authority__conciliar_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The conciliar reading extracts doctrinal authority from lay faithful and constrains theological innovation, but not as severely as a papal system (which would be ~0.50+). The extraction is modulated by: (1) the genuine coordination function of councils in maintaining communion across autocephalous churches, (2) the theological agency available to scholarly interpreters within the patristic tradition, and (3) the slower pace of enforcement compared to centralized magisterial systems. The rising trajectory (0.28 → 0.38 over 1000 years) reflects increasing extraction as councils harden into fixed doctrine and lay literacy increases the tension between accessible Scripture and restricted interpretation authority. Suppression (0.48): Moderate-high. Significant barriers to doctrinal innovation include: (1) the necessity of council convocation for legitimacy (slow process, requires broad episcopal consensus), (2) anathematization of heterodox positions (social/sacramental exclusion), (3) identity-lock preventing lay faithful from conceiving themselves as legitimate doctrinal agents, and (4) the authority of patristic precedent over contemporary reasoning. But suppression is not total because: (1) autocephalous fragmentation means different churches can resist council decisions, (2) scholarly interpretation within patristic bounds is permitted, and (3) councils themselves can revise prior decisions (rare but precedented). Theater ratio (0.52): Moderate-high. The conciliar reading increasingly operates as performance: modern academic patristics reconstructs historical Fathers' positions independently of ecclesiastical authority; churches perform conciliar continuity while actual doctrinal development proceeds through theology faculties rather than councils; councils themselves are invoked more for legitimacy than for actual doctrinal adjudication in contemporary Orthodoxy. The rising trajectory (0.38 → 0.58) reflects this widening gap between claimed conciliar authority and actual locus of theological innovation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates fundamental misalignment between how institutional actors (bishops, churches) experience it and how victims (lay faithful, theological innovators) experience it. Bishops see pure coordination (Rope) — solving the legitimate problem of maintaining doctrinal communion across jurisdictions. Lay faithful see extraction with no exit (Snare) — identity-locked doctrinal subjects forbidden from agency. Scholars see mixed coordination and constraint (Tangled Rope) — they can interpret but only within narrow bounds. Autocephalous churches see moderate extraction (Tangled Rope) — the councils enable but also inhibit institutional adaptation. The ecumenical movement sees temporary institutional bottleneck (Scaffold) — presuming convergence on shared patristic hermeneutics could eventually allow broader conciliar participation. Academic patristics sees degraded ritual (Piton) — the machinery of conciliar authority persists through theater while actual theological work happens elsewhere. The civilizational analytical observer risks seeing natural law (Mountain) — conciliar authority presented as inherent to how tradition works — but the structural data (identifiable beneficiaries extracting from identifiable victims) reveals this as a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from each agent's structural position. Beneficiaries (episcopal collegiality, orthodox churches) with arbitrage exit options derive d ≈ 0.15 (low extraction experienced). Victims (lay faithful) with identity-locked exit options derive d ≈ 0.88 (high extraction experienced despite structural mobility). Scholars (moderate power, constrained exit) derive d ≈ 0.55 (mixed experience). The lay faithful's identity_locked status is crucial: they could theoretically study theology independently (constrained exit → d ~0.60), but their identity is constituted through sacramental participation and confessional belonging. Exit from the church is exit from themselves. This internalized lock drives d upward toward trapped-level experienced extraction (d ≈ 0.88) despite structural mobility. The piton perspective (academic patristics) uses analytical exit options and derives d ≈ 0.72 (analytical observer's canonical fallback), but the commentary notes that the academic discipline is partially captured by the constraint's legacy — the performance of patristic authority persists even as actual scholarly work diverges.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that conciliar reading is a genuine Tangled Rope: it provides real coordination function (maintaining doctrinal communion, solving the multi-jurisdictional coherence problem) AND asymmetric extraction (from lay faithful who cannot participate in councils, from innovators who face suppression). The constraint is not misclassified as Rope because the extraction is structural and non-negotiable: bishops must enforce doctrinal boundaries to maintain conciliar coherence. The constraint is not Snare because the coordination function is real — councils do solve problems that individual Scripture study cannot (how do scattered churches maintain shared doctrine?). The Tangled Rope classification protects against both the institutional beneficiary's narrative (pure coordination, no extraction) and the radical critic's narrative (pure extraction, no coordination). The actual mechanism combines both. The rising theater ratio and extractiveness over time (0.38 → 0.58 and 0.28 → 0.38) suggest slow drift toward Piton (performative continuity) as the coordination function weakens under modern conditions (global communication enabling coordination without councils; academic theology proceeding independently of ecclesiastical authority) and the extraction mechanism persists through institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    patristic_consensus_epistemic_status,
    'What counts as genuine patristic consensus versus selective reading of available texts? Did the historical Fathers actually agree, or does ''consensus'' require retroactive harmonization?',
    'Systematic textual analysis comparing patristic positions on contested doctrines (e.g., theosis, merit, icons); identification of genuine uniformity vs. systematic disagreement harmonized by later editors',
    'If genuine consensus: conciliar reading is legitimate coordination around objective doctrine. If retroactive harmonization: conciliar reading is constructed authority backed by misrepresentation of sources. If partial consensus with real disagreement: conciliar reading is tangled_rope (coordination of the consensus fragments + extraction from those disagreeing with consensus).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patristic_consensus_epistemic_status, empirical, 'Epistemic status of claimed patristic consensus').

omega_variable(
    council_authority_vs_individual_church_doctrine,
    'Do ecumenical councils actually constrain doctrinal development in Orthodox churches, or do churches treat council canons as authoritative only when convenient?',
    'Historical comparison of announced council doctrine vs. subsequent church practice; identification of doctrinal divergence despite council condemnation; tracking of which councils gained binding authority and which were selectively ignored',
    'If councils are binding: suppression is structural (churches cannot innovate against council decrees). If selective compliance: suppression is weaker and depends on political power rather than doctrinal mechanism. Extractiveness may be lower if the constraint is theater (councils exist for legitimacy) rather than enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(council_authority_vs_individual_church_doctrine, empirical, 'Binding force of council authority over individual churches').

omega_variable(
    conciliar_vs_papal_extraction_asymmetry,
    'Does conciliar reading (collective episcopal authority) extract less from lay faithful than papal reading (singular magisterial authority), or is the extraction mechanism equivalent and only distributed across more bishops?',
    'Comparative analysis of doctrinal innovation rates in Catholic vs. Orthodox contexts; measurement of lay theological agency and scope for non-approved interpretation; identity-lock intensity across confessions',
    'If conciliar extracts less: the conciliar reading is genuinely a Rope-like pure coordination system. If extraction is equivalent: conciliar is distributed snare, and the contrast with papal authority is superficial. If papal is substantially higher: conciliar reading is partially justified on extraction-reduction grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conciliar_vs_papal_extraction_asymmetry, empirical, 'Comparative extraction: conciliar vs. papal authority structures').

omega_variable(
    living_tradition_boundary_condition,
    'What constitutes ''living continuity'' vs. ''rupture'' in the patristic tradition? When does innovation become heresy under the conciliar reading?',
    'Analysis of doctrinal evolution that councils endorsed (e.g., Marian doctrines, theosis theology) vs. innovations they condemned; identification of the decision procedure for distinguishing continuous development from doctrinal corruption',
    'If boundary is objective: conciliar reading has defensible epistemic content. If boundary is contestable: the constraint''s enforcement mechanism relies on appeals to authority rather than objective doctrinal criteria. Extractiveness increases if the boundary is subjective because bishops retain unaccountable discretion over what counts as faithful development.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(living_tradition_boundary_condition, conceptual, 'Boundary between living tradition and doctrinal rupture').

omega_variable(
    kernel_reading_committer_structure,
    'Is conciliar reading one legitimate reading of the biblical-authority kernel, or does it foreclose alternative readings (sola_scriptura, tradition_scripture) within Christian framework?',
    'Theological analysis of whether each reading''s foundational axioms logically entail rejection of sibling readings'' core premises, or whether they represent different institutional emphases compatible with shared Christian commitments',
    'If forecloses: the readings are incompatible, and different confessions are making logically exclusive claims about biblical authority. If coexists: all three readings can be held in different ecclesial traditions without mutual logical contradiction. If influences: conciliar reading creates structural pressure favoring some interpretations and inhibiting others without logical foreclosure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Logical compatibility of conciliar reading with sibling readings of biblical-authority kernel').

omega_variable(
    autocephalous_fragmentation_cost,
    'Does the conciliar reading''s dependence on autocephalous church cooperation (absence of central enforcement) increase the latency and incompleteness of doctrinal consensus compared to a papal or synodal system?',
    'Historical measurement of time-to-council from initial controversy, number of unresolved disputes (schisms, parallel confessions), rate of divergence between autocephalous churches on doctrinal matters',
    'If fragmentation cost is high: conciliar reading trades coordination completeness for broader institutional buy-in. If cost is low: conciliar reading achieves both coordination and distributed authority. High cost could justify reclassification toward Scaffold (temporary coordination) if bishops recognize the cost and are building alternative structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autocephalous_fragmentation_cost, empirical, 'Efficiency cost of conciliar authority distributed across autocephalous churches').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__conciliar_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bib_conc_tr_t0, biblical_authority__conciliar_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(bib_conc_tr_t500, biblical_authority__conciliar_reading, theater_ratio, 500, 0.52).
narrative_ontology:measurement(bib_conc_tr_t1000, biblical_authority__conciliar_reading, theater_ratio, 1000, 0.58).

% Extraction over time
narrative_ontology:measurement(bib_conc_be_t0, biblical_authority__conciliar_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bib_conc_be_t500, biblical_authority__conciliar_reading, base_extractiveness, 500, 0.35).
narrative_ontology:measurement(bib_conc_be_t1000, biblical_authority__conciliar_reading, base_extractiveness, 1000, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(bib_conc_su_t0, biblical_authority__conciliar_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(bib_conc_su_t500, biblical_authority__conciliar_reading, suppression_requirement, 500, 0.48).
narrative_ontology:measurement(bib_conc_su_t1000, biblical_authority__conciliar_reading, suppression_requirement, 1000, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__conciliar_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, episcopal_authority_and_papal_alternatives).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, doctrinal_innovation_suppression).

% DUAL FORMULATION NOTE:
% The biblical-authority kernel is instantiated by three distinct constraint stories with different ε values and structural relationships. conciliar_reading (ε=0.38, Tangled Rope) emphasizes distributed episcopal authority and patristic continuity. sola_scriptura_reading (upstream; ε≈0.25-0.30, Rope/Mountain candidate) emphasizes individual study and interpretive transparency. tradition_scripture_reading (ε≈0.45-0.55, Tangled Rope/Snare candidate) emphasizes active magisterial development and papal authority. These are not three perspectives on one constraint but three structurally distinct constraints sharing the contested kernel. The conciliar reading influences both siblings: it establishes the precedent that councils can determine doctrine, which sola_scriptura opposes and tradition_scripture converts into active magisterial development.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__conciliar_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
