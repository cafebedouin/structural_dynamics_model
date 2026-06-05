% ============================================================================
% CONSTRAINT STORY: biblical_authority__sola_scriptura_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: biblical_authority__sola_scriptura_reading
 *   human_readable: Scripture Alone (Sola Scriptura) as Authoritative and Self-Interpreting
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   Sola scriptura ('scripture alone') emerged in 16th-century Protestant
 *   Reformation as a claim that the biblical canon is the sole authoritative
 *   source for Christian doctrine and practice, requiring no supplement from
 *   church tradition or magisterial authority. This reading instantiates one
 *   interpretation of how biblical authority operates. The constraint
 *   exhibits structural tension between its emancipatory function
 *   (decentralizing interpretive authority from clergy to lay believers and
 *   local congregations) and its practical limitations (interpretation itself
 *   requires framework, lay capacity is variable, institutional churches
 *   adopting the doctrine have layered confessional standards above it). Over
 *   300+ years of institutional evolution, sola scriptura has shifted from a
 *   revolutionary constraint challenging clerical monopoly to a performative
 *   doctrine maintained in theological language while institutional
 *   Protestant churches operate via confessional standards and ordained
 *   interpretive authority. The measurement trajectory shows rising
 *   theater_ratio (35% → 58%) as the doctrine's operational function decays,
 *   rising base_extractiveness (18% → 38%) as doctrinal fragmentation and lay
 *   exclusion accumulate, and rising suppression_requirement (25% → 42%) as
 *   institutional churches must actively enforce confessional standards
 *   despite the formal claim of scriptural sufficiency.
 *
 * KEY AGENTS:
 *   - Lay Believers: Primary beneficiary in doctrine (mobile/moderate) — promised interpretive autonomy and direct scriptural access; actual capacity varies by literacy and linguistic resources
 *   - Local Congregations: Primary beneficiary (moderate/mobile) — experience scriptural authority as enabling congregational self-governance without hierarchical clerical mediation
 *   - Doctrinal Coherence Across Communities: Primary victim (powerless/trapped) — no trans-community adjudicative mechanism; absent clerical monopoly leaves fragmentation unresolved
 *   - Ecclesiastical Authority Structures: Secondary victim and beneficiary (institutional/constrained) — formal authority claim undermined by sola scriptura's egalitarian premise, but confessional gatekeeping concentrates actual interpretive power
 *   - Established Protestant Churches: Institutional actor (institutional/constrained) — maintain sola scriptura in doctrine while operating via confessional standards and seminary-trained authority; sustained through theater
 *   - Marginal Believers: Secondary victim (powerless/trapped) — promised autonomy suppressed by epistemic barriers (literacy, language, theological training); highest experienced extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.38).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.42).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Scripture Alone (Sola Scriptura) as Authoritative and Self-Interpreting").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theology/religious_studies/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__sola_scriptura_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, 'a19212ae-bb6c-4dea-a273-5b13bf71d1a7').
narrative_ontology:cs_kernel_codification('a19212ae-bb6c-4dea-a273-5b13bf71d1a7', fixed_text).
narrative_ontology:cs_authority_grounding('a19212ae-bb6c-4dea-a273-5b13bf71d1a7', lineage).
narrative_ontology:cs_interpretation_layer_present('a19212ae-bb6c-4dea-a273-5b13bf71d1a7').
narrative_ontology:cs_reading_relation('a19212ae-bb6c-4dea-a273-5b13bf71d1a7', biblical_authority__tradition_scripture_reading, coexists_with).
narrative_ontology:cs_reading_relation('a19212ae-bb6c-4dea-a273-5b13bf71d1a7', biblical_authority__conciliar_reading, influences).
narrative_ontology:cs_axiom('a19212ae-bb6c-4dea-a273-5b13bf71d1a7', foundational, scriptural_sufficiency_for_doctrine).
narrative_ontology:cs_axiom_status(scriptural_sufficiency_for_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('a19212ae-bb6c-4dea-a273-5b13bf71d1a7', scriptural_sufficiency_for_doctrine, theological).
narrative_ontology:cs_axiom('a19212ae-bb6c-4dea-a273-5b13bf71d1a7', foundational, lay_interpretive_capacity).
narrative_ontology:cs_axiom_status(lay_interpretive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('a19212ae-bb6c-4dea-a273-5b13bf71d1a7', lay_interpretive_capacity, theological).
narrative_ontology:cs_reference_frame('a19212ae-bb6c-4dea-a273-5b13bf71d1a7', protestant_scriptural_autonomy).
narrative_ontology:cs_drift_state('a19212ae-bb6c-4dea-a273-5b13bf71d1a7', contemporary_institutional_protestantism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a19212ae-bb6c-4dea-a273-5b13bf71d1a7', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, local_congregations).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, ecclesiastical_authority_structures).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCAL CONGREGATION / AUTONOMY (ROPE) — Experiences sola scriptura as genuine coordination mechanism. Scripture provides shared reference without requiring hierarchical interpretive authority. Congregational autonomy to interpret is a benefit; burden of interpretation is shared cost, not extraction. Net beneficiary with agency — genuine access to sacred text enables self-governance.
constraint_indexing:constraint_classification(biblical_authority__sola_scriptura_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: DOCTRINAL COMMUNITY / FRAGMENTATION (TANGLED ROPE) — Experiences sola scriptura as mixed: genuine coordination benefit (shared canon, scriptural authority as common reference) alongside asymmetric extraction (doctrinal fragmentation, interpretive license tilted toward charismatic or numerically dominant groups, loss of trans-community adjudicative authority). High suppression of unified doctrine; constrained options because exit requires abandoning scripture's authority itself.
constraint_indexing:constraint_classification(biblical_authority__sola_scriptura_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MARGINAL BELIEVER / INTERPRETIVE EXCLUSION (SNARE) — Experiences maximum extraction despite sola scriptura's egalitarian framing. Lay believers without theological training, non-native language speakers, or members of communities with limited access to interpretation resources face high barriers to meaningful participation in scriptural reading. The doctrine promises autonomy but suppresses the epistemic capacity to exercise it. Theater high: the teaching is 'everyone can read,' but actual interpretive power concentrates among educated interpreters.
constraint_indexing:constraint_classification(biblical_authority__sola_scriptura_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 4: ESTABLISHED PROTESTANT CHURCH / INSTITUTIONAL INERTIA (PITON) — Sola scriptura began as a revolutionary constraint challenging Catholic clerical monopoly, but in institutional Protestant churches it has become largely performative. Official doctrine claims scripture-alone authority, but lived practice relies on confessional standards, seminary-trained interpretive authority, hymnal traditions, and liturgical formulae that function as binding tradition-within-tradition. Theater high (0.58+): the institutional church maintains sola scriptura in doctrine while operating via confessional and clerical authority in practice. The constraint's original function (decentralize authority) has atrophied; it persists through inertia.
constraint_indexing:constraint_classification(biblical_authority__sola_scriptura_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: REFORMED ECCLESIASTICAL AUTHORITY / CONFESSIONAL CAPTURE (TANGLED ROPE) — Institutional perspective on sola scriptura operating within denominational structures. Genuine coordination function (shared canon and interpretive principles enable scale); asymmetric extraction (confessional standards, ordained clergy interpretation, seminary gatekeeping concentrate effective doctrinal authority despite formal scripture-alone claim). Suppression high: dissent from confessional interpretation is constrained by institutional discipline. Enforcement active: confessional catechisms, pastoral credentialing, and pulpit control enforce scriptural readings aligned with institutional doctrine. Theater moderate: some ambiguity between stated (scripture alone) and practiced (confessional authority) doctrine.
constraint_indexing:constraint_classification(biblical_authority__sola_scriptura_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / HERMENEUTIC LIMIT (MOUNTAIN) — From a civilizational/universal epistemic perspective, sola scriptura appears to face an immutable hermeneutic constraint: any text requires interpretive framework to be meaningful. Scripture without tradition or community context is logically incomplete — pure text reading is impossible. From this view, claiming self-sufficiency naturalizes what may be a structural impossibility. The engine's false-summit detector will evaluate whether this 'immutable hermeneutic limit' is genuine natural law or naturalization of a contingent institutional choice (what counts as legitimate interpretive framework).
constraint_indexing:constraint_classification(biblical_authority__sola_scriptura_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__sola_scriptura_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biblical_authority__sola_scriptura_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biblical_authority__sola_scriptura_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, TR),
    TR >= 0.70.

:- end_tests(biblical_authority__sola_scriptura_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits genuine coordination benefit (shared scriptural canon, common reference for doctrine) but also significant asymmetric extraction. At the reformation launch (t=0, ε=0.18), extraction was minimal — sola scriptura genuinely decentralized authority from Catholic clerical monopoly, and lay access to vernacular scripture was emancipatory. By contemporary time (t=300, ε=0.38), extractiveness has risen as doctrinal fragmentation accumulated and lay interpretive capacity proved difficult to sustain without institutional gatekeeping. The modern institutional Protestant church maintains sola scriptura doctrinally while concentrating effective authority via confessional standards and pastoral credentialing. Suppression (0.42): Moderate-high. Mechanisms include limited lay linguistic capacity (biblical Greek/Hebrew), assumed theological background knowledge, literacy requirements, and institutional suppression of interpretive readings outside confessional norms. Active enforcement via pastoral discipline, pulpit control, and catechetical instruction. Theater ratio (0.58): Moderate-high. At reformation (t=0, theater=0.35), sola scriptura functioned relatively clearly — scripture was the stated authority and was genuinely appealed to. By contemporary (t=300, theater=0.58), significant gap exists between stated doctrine (scripture alone) and practice (confessional standards, ordained authority, seminary gatekeeping). The rising theater reflects the constraint's original function (decentralize authority) decaying while the doctrine persists.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. At the local congregational level (moderate/mobile/biographical), sola scriptura appears as genuine coordination (Rope) enabling congregational autonomy. At the institutional Protestant level (institutional/constrained/generational), the doctrine appears as tangled rope — genuine coordination benefit (shared canon) with significant extraction (confessional capture, clerical authority). For doctrinal coherence as a community good (powerless/trapped), the constraint appears as pure snare — no binding adjudicative authority means fragmentation is unresolved. For established churches maintaining sola scriptura in doctrine while operating via confessional authority, the constraint is piton (performative theater covering institutional inertia). For marginal believers without epistemic resources, the constraint is snare despite its egalitarian promise. The analytical observer faces temptation to see an immutable hermeneutic limit (mountain) — the claim that pure scripture without interpretive framework is impossible — but this naturalizes what may be a contingent choice about which frameworks are legitimate. The structural data reveals sola scriptura as a false summit when viewed from the hermeneutic necessity angle: the 'immutability' of the interpretive limit depends on narrowing what counts as legitimate framework (rejecting tradition, community practice, institutional guidance) — a contingent institutional choice, not a law of interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position: power level, exit options, and relationship to extraction flow. Lay believers as beneficiaries with mobile exit options generate low d (around 0.15-0.25), producing negative or low chi — they experience sola scriptura as beneficial coordination. Doctrinal coherence as a powerless agent with trapped exit generates high d (around 0.90), producing high chi — experiencing maximum extraction. Ecclesiastical authority structures as institutional actors with constrained exit (bound by doctrinal claim while operating via tradition) generate moderate d (around 0.50-0.60). The perspectival gap arises because different agents' exit capacity, power level, and relationship to the extraction flow are structurally distinct. A lay believer can exit (mobile) by changing churches; doctrinal coherence cannot exit without ceasing to be a community good. An established Protestant church cannot exit without repudiating sola scriptura (constrained) despite operating via confessional authority. These structural differences produce the spanning classification set.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not resolve mandatrophy in the sense of converging on a single true classification. Rather, it demonstrates that mandatrophy is perspectival and reveals what the different perspectives are actually measuring. The local congregation genuinely experiences rope (coordination function, low extraction). The institutional church genuinely experiences tangled rope (coordination + extraction + confessional gatekeeping). Doctrinal coherence genuinely experiences snare (fragmentation, no adjudication). The established church genuinely experiences piton (performative doctrine, function decay). These are not conflicting claims about a single property; they are descriptions of different structural relationships to the same constraint. The mandatrophy resolves when framed correctly: sola scriptura operates as different constraint types depending on the agent's position in its structure. The analytical observer's temptation to see mountain (hermeneutic necessity) is itself a structural phenomenon — the observer risks naturalizing institutional choices about legitimate frameworks — which is exactly what the false-summit detector is designed to flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scriptural_self_sufficiency_boundary,
    'At what interpretive depth does scripture cease being ''self-interpreting'' and require external framework? Is that boundary intrinsic (hermeneutic necessity) or contingent (institutional gatekeeping)?',
    'Comparative analysis of non-institutional scriptural reading communities (housechurch movements, autodidact believers) vs institutional interpretations; documentation of minimal interpretive apparatus sufficient for meaningful doctrinal reading; analysis of what ''self-interpreting'' claims across traditions actually presuppose.',
    'If intrinsic (immutable): mountain classification confirmed; sola scriptura faces irreducible limits. If contingent: mountain is false summit; the ''sufficiency'' claim naturalizes institutional choices about which frameworks are legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_self_sufficiency_boundary, conceptual, 'Whether scriptural self-sufficiency is hermeneutically intrinsic or contingent on interpretive framework').

omega_variable(
    doctrinal_fragmentation_causation,
    'Does sola scriptura doctrine itself cause doctrinal fragmentation (by removing adjudicative authority), or does fragmentation arise from other causes (cognitive diversity, cultural context, literacy variation) that sola scriptura merely fails to suppress?',
    'Historical comparison: fragmentation rates in pre-Reformation Catholic tradition (unified clerical authority) vs post-Reformation communities; analysis of which doctrinal disputes correlate with sola scriptura adoption vs those arising independently; controlled comparison of communities adopting vs rejecting sola scriptura while holding other factors constant.',
    'If sola scriptura causes fragmentation: constraint is a driver of doctrinal diversity (snare for coherence-bearing agents). If sola scriptura fails to suppress pre-existing diversity: it is a passive enabler rather than active extractor; classification shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_fragmentation_causation, empirical, 'Whether sola scriptura causes or merely permits doctrinal fragmentation').

omega_variable(
    lay_interpretive_capacity_enablement,
    'Does sola scriptura actually enable lay interpretive capacity, or does it performatively promise autonomy while suppressing the epistemic resources (languages, historical context, theological training) required for meaningful interpretation?',
    'Measurement of interpretive variance in lay vs ordained communities; documentation of institutional gatekeeping mechanisms (language standardization, seminary credentialing, pulpit control); analysis of which doctrinal readings are suppressed vs permitted in lay contexts; comparison of interpretive autonomy in literacy-constrained vs literacy-abundant communities.',
    'If enabled: sola scriptura functions as rope (genuine coordination benefit). If performative: sola scriptura is snare for lay believers (autonomy promised but suppressed). If mixed: tangled rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lay_interpretive_capacity_enablement, empirical, 'Whether lay believers gain actual interpretive capacity under sola scriptura').

omega_variable(
    confessional_capture_depth,
    'To what extent does institutional Protestant reliance on confessional standards (Heidelberg Catechism, Westminster Confession, Augsburg Confession) represent a return to tradition-mediated authority that contradicts sola scriptura in practice?',
    'Textual analysis of doctrinal authority weight in Reformed and Lutheran communities: scriptural citation vs confessional citation in pulpits, catechesis, and discipline; historical documentation of how confessional standards function as binding interpretation; comparison of hermeneutic freedom when scriptural reading aligns vs conflicts with confessional standard.',
    'If deep capture: institutional Protestantism is operating via tradition-mediated authority despite sola scriptura claim; piton and tangled-rope classifications confirmed. If limited capture: confessions function as summary aids rather than binding authority; rope classification gains support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(confessional_capture_depth, empirical, 'Extent of confessional standard capture of scriptural interpretation in institutional Protestantism').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading of the contested ''biblical authority'' kernel. What structural relationship holds between the sola scriptura reading instantiated here and the tradition+scripture reading and conciliar reading held by other communities?',
    'Theological analysis of core premises: sola scriptura claims scripture is sufficient without tradition; tradition+scripture reading claims both are authoritative; conciliar reading claims ecumenical council constitutes binding authority. These are distinct epistemological commitments. The operative distinctions are doctrinal (what counts as authoritative) not empirical, though they have institutional consequences.',
    'If forecloses: sola scriptura''s claim of scriptural sufficiency logically rules out tradition+scripture in any unified framework (rare, requires careful analysis). If coexists: different ecclesial communities hold different readings simultaneously without logical contradiction within their own frameworks. If influences: sola scriptura reading creates structural pressure on tradition and conciliar readings (changes cost of claiming traditional authority) without ruling them out.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural relationship between sola scriptura and sibling biblical authority readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibauth_ss_theater_reformation, biblical_authority__sola_scriptura_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bibauth_ss_theater_institutional, biblical_authority__sola_scriptura_reading, theater_ratio, 150, 0.48).
narrative_ontology:measurement(bibauth_ss_theater_contemporary, biblical_authority__sola_scriptura_reading, theater_ratio, 300, 0.58).

% Extraction over time
narrative_ontology:measurement(bibauth_ss_extract_reformation, biblical_authority__sola_scriptura_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(bibauth_ss_extract_institutional, biblical_authority__sola_scriptura_reading, base_extractiveness, 150, 0.31).
narrative_ontology:measurement(bibauth_ss_extract_contemporary, biblical_authority__sola_scriptura_reading, base_extractiveness, 300, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(bibauth_ss_suppress_reformation, biblical_authority__sola_scriptura_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(bibauth_ss_suppress_institutional, biblical_authority__sola_scriptura_reading, suppression_requirement, 150, 0.38).
narrative_ontology:measurement(bibauth_ss_suppress_contemporary, biblical_authority__sola_scriptura_reading, suppression_requirement, 300, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_authority__sola_scriptura_reading, 0.12).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__conciliar_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, protestant_confessional_authority).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, lay_epistemic_autonomy).

% DUAL FORMULATION NOTE:
% Sola scriptura is one reading of the biblical authority kernel. The tradition+scripture and conciliar readings are sibling constraints with different ε values and structural profiles. Sola scriptura (ε=0.38) decentralizes authority but fragments doctrine; tradition+scripture (hypothetical sibling, higher ε) centralizes authority but maintains coherence; conciliar (hypothetical sibling, moderate ε) formalizes adjudication. These are distinct constraints, not alternative measurements of the same thing. Network links show dependency: sola scriptura's claim depends on scriptural sufficiency (mountain-ish under hermeneutic necessity view) and influences how institutional churches can implement authority structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__sola_scriptura_reading, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
