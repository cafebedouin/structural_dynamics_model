% ============================================================================
% CONSTRAINT STORY: episcopal_collegiality_tension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_episcopal_collegiality_tension, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: episcopal_collegiality_tension
 *   human_readable: Episcopal Collegiality vs. Papal Primacy Tension
 *   domain: religious/institutional/doctrinal
 *
 * SUMMARY:
 *   Vatican II (1962-1965) introduced episcopal collegiality as a corrective
 *   to centuries of papal centralization, teaching that bishops collectively
 *   with the Pope (not under him) constitute the supreme authority of the
 *   Church. Yet the post-conciliar era reveals a structural tension: the
 *   language of collegiality has been used to justify both genuine
 *   decentralization (episcopal conferences, pastoral autonomy) and
 *   sophisticated re-centralization (Rome's control of conference procedures,
 *   doctrinal micro-management, selective papal primacy in implementation).
 *   This constraint story examines whether Vatican II represents a single
 *   coherent reinterpretation of Catholic doctrine or an overdetermined
 *   composite of multiple incompatible shifts (collegiality vs. primacy,
 *   local autonomy vs. doctrinal unity, episcopal dignity vs. curial control)
 *   that were papered over by ambiguous conciliar language. The constraint
 *   exhibits high theater ratio because Vatican II documents are invoked
 *   selectively by competing actors to justify contradictory practices —
 *   progressive episcopates cite Lumen Gentium's collegial passages; Rome
 *   cites Pastor Aeternus's primacy affirmations from the same council. The
 *   extractiveness has increased over the 60-year post-conciliar period as
 *   Rome has developed mechanisms (Apostolos Suos, curial reorganizations,
 *   selective episcopal appointment) to operationalize collegiality in ways
 *   that preserve curial authority while using collegial language as
 *   legitimation.
 *
 * KEY AGENTS:
 *   - Local Bishops: Primary victims (powerless/trapped) — face conflicting directives about their authority and autonomy; cannot exit the structure without renouncing priesthood
 *   - National Episcopal Conferences: Secondary victims and constrained beneficiaries (moderate/constrained) — coordinate pastoral work but require Rome's approval; face removal/reassignment for defiance
 *   - Papal Curia: Primary beneficiaries (institutional/arbitrage) — maintain structural control under new collegial language; can reinterpret collegiality doctrine and set approval timelines
 *   - Progressive Episcopal Movements: Organized challengers (organized/constrained) — see collegiality as temporary scaffold enabling eventual dismantle of centralization; face Vatican intervention
 *   - Post-Conciliar Theological Establishment: Institutional custodians (institutional/arbitrage) — maintain Vatican II's authority while managing contradictions between texts; arbitrage between different reading communities
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the authority tension as theological paradox rather than institutional power distribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(episcopal_collegiality_tension, 0.52).
domain_priors:suppression_score(episcopal_collegiality_tension, 0.48).
domain_priors:theater_ratio(episcopal_collegiality_tension, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(episcopal_collegiality_tension, extractiveness, 0.52).
narrative_ontology:constraint_metric(episcopal_collegiality_tension, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(episcopal_collegiality_tension, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(episcopal_collegiality_tension, tangled_rope).
narrative_ontology:human_readable(episcopal_collegiality_tension, "Episcopal Collegiality vs. Papal Primacy Tension").
narrative_ontology:topic_domain(episcopal_collegiality_tension, "religious/institutional/doctrinal").

domain_priors:requires_active_enforcement(episcopal_collegiality_tension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(episcopal_collegiality_tension, 'dc85d81a-4888-42ec-939a-7fa8eb44238f').
narrative_ontology:cs_created_at('dc85d81a-4888-42ec-939a-7fa8eb44238f', '').
narrative_ontology:cs_kernel_codification('dc85d81a-4888-42ec-939a-7fa8eb44238f', fixed_text).
narrative_ontology:cs_authority_grounding('dc85d81a-4888-42ec-939a-7fa8eb44238f', lineage).
narrative_ontology:cs_interpretation_layer_present('dc85d81a-4888-42ec-939a-7fa8eb44238f').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(episcopal_collegiality_tension, papal_curia).
narrative_ontology:constraint_beneficiary(episcopal_collegiality_tension, episcopates_integrated_into_structure).
narrative_ontology:constraint_victim(episcopal_collegiality_tension, local_episcopal_autonomy).
narrative_ontology:constraint_victim(episcopal_collegiality_tension, doctrinal_coherence_of_church).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCAL BISHOP (SNARE) — Trapped between Vatican II's affirmation of episcopal collegiality and persistent Roman centralization. The bishop cannot exit the Church's institutional structure; carries full cost of conflicting directives. Experiences extraction as forced deference to increasingly granular Rome-issued norms despite theoretical collegial authority. No alternative ecclesiology available within Catholic framework.
constraint_indexing:constraint_classification(episcopal_collegiality_tension, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NATIONAL EPISCOPAL CONFERENCE (TANGLED ROPE) — Constrained by Rome's requirement for approval of conference documents (Apostolos Suos, 1998) but also beneficiaries of coordinated pastoral strategy and doctrinal unity guarantees. Faces career risk (removal/reassignment) for direct resistance. Coordination function genuine (shared moral authority, collective witness); extraction also genuine (Rome retains veto over collegial decisions).
constraint_indexing:constraint_classification(episcopal_collegiality_tension, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PAPAL CURIA (ROPE) — Experiences the constraint as coordination: Vatican II legitimized decentralization they feared, but curial structures now coordinate global episcopate more efficiently than pre-Vatican II patrimonial administration. Net beneficiary of the constraint system — collegiality rhetoric justifies continued Roman structural control under new language. High arbitrage options (can reinterpret collegiality doctrine, control implementation timelines).
constraint_indexing:constraint_classification(episcopal_collegiality_tension, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROGRESSIVE EPISCOPAL MOVEMENTS (SCAFFOLD) — CELAM (Latin America), base ecclesial communities, liberation theology episcopates see Vatican II as temporary opening with sunset: active collegiality as a transitional structure enabling local autonomy that will eventually dismantle centralized authority altogether. Constrained by Vatican intervention (Medellín → Puebla → Aparecida pattern shows persistent pushback) but organized collectively and see the scaffold as necessarily temporary. Extraction experienced but declining as alternative structures mature.
constraint_indexing:constraint_classification(episcopal_collegiality_tension, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: VATICAN II INSTITUTIONAL MEMORY (PITON) — Vatican II is invoked rhetorically to justify both collegial decentralization AND Roman centralization, depending on which council texts are cited. The council documents themselves function as theater — Lumen Gentium's collegial vision coexists with Pastor Aeternus's primacy affirmation in the same text. Post-council development shows degradation: the conciliar consensus existed only because ambiguous language papered over real disagreements. Theater ratio high because Vatican II's very invocation has become detached from its actual force.
constraint_indexing:constraint_classification(episcopal_collegiality_tension, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / THEOLOGICAL NECESSITY (MOUNTAIN) — From a civilizational perspective, the tension between papal primacy and episcopal collegiality is presented as an irreducible theological paradox: both are taught as dogmatic, both derive from Scripture and Tradition, and no theological synthesis has fully resolved the tension. This view naturalizes the contradiction as inherent to Catholic doctrine itself — a permanent structural feature that cannot be reformed away. However, this risks false summitry: the paradox framing obscures that the tension is contingent on institutional power distribution, not theological necessity.
constraint_indexing:constraint_classification(episcopal_collegiality_tension, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(episcopal_collegiality_tension_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(episcopal_collegiality_tension, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(episcopal_collegiality_tension, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(episcopal_collegiality_tension, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(episcopal_collegiality_tension, TR),
    TR >= 0.70.

:- end_tests(episcopal_collegiality_tension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and increasing. The base extraction reflects curial capture of the collegiality mechanism itself — Rome requires episcopal conference approval procedures that it controls, maintaining veto power while using collegial language. The measurement trajectory (0.28 → 0.52 over 20 years) shows extraction accumulating as Rome develops increasingly sophisticated mechanisms to make collegiality operationally hollow. The extractiveness is not as high as a pure snare (0.72+) because genuine coordination does occur — episcopal conferences do improve pastoral coordination and Rome does consult them, generating real collective goods. But the asymmetry is real: Rome retains final authority while bishops bear the cost of ambiguous directives. Suppression (0.48): Moderate and structural. Bishops cannot exit without renouncing priesthood and identity; removal/reassignment threatens career; no alternative ecclesiology available within Church framework. But suppression is not total (0.60+) because bishops retain some interpretive space and some episcopal conferences have mobilized countervailing power (CELAM, German episcopate, Belgian episcopate). Theater ratio (0.65): High and increasing. Vatican II documents function as scripture for competing factions — cited selectively to justify contradictory practices. The ritual of collegiality (conference votes, synods, consultations) increasingly performs legitimacy without transferring actual authority. The measurement trajectory (0.35 → 0.65) reflects that the theater has accumulated: Vatican II's ambiguous language has spawned layers of interpretive infrastructure (curial theological commissions, post-synodal exhortations, implementation guidelines) that operate at increasing distance from the conciliar text itself.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The papal curia sees coordination (Rope) — collegiality language enables structural integration of episcopates without surrendering primacy. Progressive episcopal movements see a temporary opening (Scaffold) — Vatican II is a transitional space before genuine decentralization; synodality is the next phase. Vatican II itself sees its own degradation (Piton) — the conciliar consensus existed only via ambiguous language; post-conciliar development has exhausted the text without resolving the underlying conflicts. National episcopal conferences see mixed coordination and extraction (Tangled Rope) — they do better pastoral work collectively, but Rome's veto power is real. Local bishops trapped in the system see pure extraction (Snare) — they receive conflicting authority signals with no way out except leaving the priesthood. The analytical observer risks seeing a theological paradox (Mountain) — an irreducible tension between primacy and collegiality that cannot be reformed — but the structural data reveals this as potential false summitry: the 'paradox' may be a contingent institutional power distribution dressed in theological language.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation for each perspective is driven by structural position, not by theological position. The curia's d ≈ 0.15 derives from: beneficiary status (curial functions are justified and sustained by the apparatus), arbitrage exit options (curia can reinterpret doctrine, control implementation timelines, reshape the apparatus itself without leaving it). The bishop's d ≈ 0.88 derives from: victim status (bishops bear the cost of conflicting directives — they must obey Rome while theoretically being Rome's colleagues), trapped exit options (renouncing priesthood is an exit, but an identity-destroying one, suggesting the bishop's exit should be classified as identity_locked rather than trapped — the structural mobility exists but is identity-barred). The national episcopal conference derives d ≈ 0.62 from: mixed status (beneficiary of coordinated pastoral authority, victim of Rome's veto power), constrained exit (can organize collectively and resist, but resistance carries career risk). The progressive episcopal movements derive d ≈ 0.55 from: victim status (Rome intervenes in their pastoral choices), constrained exit (organized and mobilizing alternative structures, but Rome retains formal authority). The scaffold perspective's d ≈ 0.35 reflects: organized power (alternative structures are developing), constrained rather than trapped exit (synodality, base communities, national episcopate coordination are building real alternatives), and a sunset mechanism (the scaffold is meant to be temporary, declining in extractiveness as new structures mature). The mountain perspective's d ≈ 0.72 is anomalous — it suggests analytical observation sees the tension as nearly maximal extraction, which is the false summit signal. The mountain classification naturalizes what is actually an institutional power distribution.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that 'collegiality' serves as both a coordination mechanism (genuine shared responsibility for Church governance, legitimate pastoral consultation, collective moral authority) AND an extraction mechanism (Rome's veto power over collegial decisions, control of implementation timelines, micro-management via doctrine). The Tangled Rope classification captures both functions. The mandatrophy would emerge if we tried to classify this as pure Rope (dismissing the extraction as coordination cost) or as pure Snare (dismissing the coordination as theater). The intermediate classification reflects that the constraint genuinely coordinates episcopal work — Vatican II's call to collegiality did improve pastoral effectiveness and reduced some forms of curial arbitrariness — while also extracting authority that was genuinely decentralized. The extraction mechanism is sophisticated: Rome doesn't prohibit collegiality, it operationalizes it in ways that preserve curial control. The increasing theater ratio over time suggests that the extraction mechanism is becoming less effective at hiding its own operation — the performative layer is thickening precisely because the real authority transfer that Vatican II promised is not occurring. The mandatrophy is resolved not by choosing one type, but by recognizing that the tension between coordination and extraction IS the constraint, and Tangled Rope is the only classification that captures both dimensions without collapsing either one into theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vatican_ii_unity_or_overdetermination,
    'Does Vatican II represent a single coherent reinterpretation of Catholic doctrine, or multiple simultaneous doctrinal shifts that happened to coincide temporally and were later narrated as unified?',
    'Historical discourse analysis: trace the competing formulations in conciliar debates (Gasser Reports, intervention transcripts); compare pre-council and post-council theological literature to identify which positions were genuinely resolved vs. merely deferred; analyze implementation divergence by episcopate (progressive Latin American reading vs. conservative European reading vs. curial reading) to determine if these are faithful readings of one text or incompatible readings of an ambiguous text.',
    'If unified: the constraint is institutional execution of a coherent vision (lower extractiveness, higher coordination). If overdetermined: the constraint is the structural mechanism by which contradictory positions are enforced simultaneously, enabling selective citation by different power centers (higher extractiveness, lower coordination). Classification could shift from Tangled Rope to Snare if overdetermination is established.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vatican_ii_unity_or_overdetermination, empirical, 'Whether Vatican II is unified reinterpretation or overdetermined composite').

omega_variable(
    collegiality_as_cover_for_centralization,
    'Does the collegiality framework (especially Apostolos Suos and subsequent Rome-controlled ''collegial'' mechanisms) actually decentralize authority, or does it provide legitimating language for more sophisticated centralization?',
    'Structural analysis of decision-making timelines: track which decisions by episcopal conferences have been overruled, delayed, or reinterpreted by Rome post-approval; compare autonomy metrics (financial, doctrinal, personnel) for episcopates before Vatican II vs. after; measure the latency between collegial decision and Roman implementation/modification.',
    'If genuine decentralization: the constraint is coordination mechanism (Rope from curial perspective justified). If sophisticated centralization: the constraint is a snare disguised in collegial language (high extractiveness, high theater, high suppression). This is the primary FSM candidate — the theological paradox naturalizes what may be pure institutional power consolidation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collegiality_as_cover_for_centralization, empirical, 'Whether collegiality decentralizes or legitimizes centralization').

omega_variable(
    bishops_exit_capacity,
    'Can bishops of conscience exit the institutional structure? What fraction have been removed, reassigned, or marginalized for defying Rome, and what does this reveal about formal collegiality vs. actual authority structure?',
    'Historical record of episcopal removals/reassignments (Hunthausen, Gumbleton, Weakland, etc.); analysis of patterns — are removals correlated with collegiality-championing positions? Can a bishop openly support married clergy, contraception reform, or synodal governance without facing career termination?',
    'If removal is rare: bishops have constrained but real exit options (Tangled Rope confirmed). If removal is pattern response to collegiality assertion: bishops are trapped by identity lock (Church service identity prevents exit even when structurally mobile), suggesting higher suppression and identity_locked exit status for bishop perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bishops_exit_capacity, empirical, 'Whether bishops can actually exit without termination').

omega_variable(
    conciliar_document_interpretation_variance,
    'How much variance exists in interpretations of Vatican II among the council fathers, and did the council documents intentionally preserve this variance as a feature rather than a bug?',
    'Compare Relatio finalis text analysis with recorded interventions, minority reports, and post-council theological commentary; identify which formulations represent compromises that satisfied no one fully; trace the subsequent splitting of Vatican II interpretation into ''hermeneutic of reform'' vs. ''hermeneutic of continuity'' and ask whether this split existed inchoately in the conciliar debates themselves.',
    'If intentional variance: Vatican II is a kernel (multiple legitimate readings), and the tension is inherent to the document''s design. If unintentional: the tension reveals the council''s inability to resolve substantive disagreements about authority, and later ''readings'' are actually distinct constraints being falsely unified under the council''s name.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conciliar_document_interpretation_variance, conceptual, 'Whether Vatican II intentionally preserved doctrinal variance').

omega_variable(
    identity_lock_mechanism,
    'For bishops, how much of the suppression is structural (removal threat, career dependency) versus internalized (identity fused with hierarchical Church, inability to imagine oneself as non-episcopal priest or layperson)?',
    'Post-exit analysis of former bishops: those who have left the priesthood and retained faith — do they report that exit was possible under different self-conception? Interview data on bishops'' identity formation — how early does hierarchical identity lock in? Comparative analysis with bishops who do challenge Rome vs. those who don''t — do the challengers have different identity structures (lay involvement, regional autonomy experience, etc.)?',
    'If largely structural: bishops are trapped/constrained, snare classification holds. If largely internalized: bishops are identity_locked, perceive the structure as mountain when structurally it is mobile. This would shift the bishop perspective to identity_locked exit option and potentially change the classification trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether episcopal suppression is structural or identity-internalized').

omega_variable(
    synodality_as_constraint_sunset,
    'Is the current synodality movement (Synod on Synodality, 2021-2023) evidence that collegiality is maturing into genuine shared governance (scaffold sunset confirmed), or is synodality itself being captured by Rome in the same way collegiality was?',
    'Track synodality implementation over next 5-10 years: Are synodal decisions binding on bishops? Can synods overrule papal doctrine? Does Rome establish pre-approval gates on synodal outcomes? Compare synodality rhetoric with post-synod papal documents to identify whether curial reinterpretation is occurring.',
    'If synodality is genuine sunset to collegiality: scaffold perspective confirmed, theater_ratio should decline as real authority transfer occurs. If synodality is being captured: the constraint intensifies (higher theater as successive language layers are added), and classification shifts toward Piton or Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(synodality_as_constraint_sunset, empirical, 'Whether synodality represents collegiality sunset or its recapture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(episcopal_collegiality_tension, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(episco_tr_t0, episcopal_collegiality_tension, theater_ratio, 0, 0.35).
narrative_ontology:measurement(episco_tr_t10, episcopal_collegiality_tension, theater_ratio, 10, 0.55).
narrative_ontology:measurement(episco_tr_t20, episcopal_collegiality_tension, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(episco_be_t0, episcopal_collegiality_tension, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(episco_be_t10, episcopal_collegiality_tension, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(episco_be_t20, episcopal_collegiality_tension, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(episcopal_collegiality_tension, enforcement_mechanism).
narrative_ontology:affects_constraint(episcopal_collegiality_tension, papal_infallibility_scope).
narrative_ontology:affects_constraint(episcopal_collegiality_tension, episcopal_appointment_authority).

% DUAL FORMULATION NOTE:
% Episcopal collegiality is downstream of Vatican II's larger reinterpretation of Church authority structure and upstream of specific doctrinal constraints (infallibility scope, appointment authority). The collegiality constraint itself contains nested subordinate constraints: episcopal conference authority, synodal governance mechanisms, curial reform. This story models collegiality as the primary constraint; sibling stories in the family would decompose the council's authority reinterpretation into collegiality, infallibility scope redefinition, and local episcopal autonomy separately, each with its own ε and perspective set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(episcopal_collegiality_tension, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
