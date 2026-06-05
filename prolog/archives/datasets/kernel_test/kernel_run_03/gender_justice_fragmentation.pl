% ============================================================================
% CONSTRAINT STORY: gender_justice_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gender_justice_fragmentation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gender_justice_fragmentation
 *   human_readable: Gender Justice Fragmentation in India's Personal Law Regimes
 *   domain: constitutional_law/legal_pluralism/comparative_family_law
 *
 * SUMMARY:
 *   India's coexisting personal-law regimes (Hindu, Muslim, Christian, Parsi,
 *   secular) governing marriage, divorce, inheritance, and family property
 *   represent a 75-year-old institutional compromise between constitutional
 *   gender equality (Articles 14-15) and religious pluralism (Articles
 *   25-28). The system has achieved no zero-sum resolution — neither toward a
 *   uniform civil code nor toward complete religious autonomy — creating a
 *   fragmented legal landscape where a woman's rights depend on her religious
 *   community classification. Article 44 explicitly aspires to a uniform
 *   civil code, yet political resistance, institutional inertia, and genuine
 *   complexity about enforcing equality across diverse traditions have
 *   prevented implementation. The constraint exhibits classical tangled-rope
 *   structure: genuine coordination function (accommodating religious
 *   diversity in a multi-faith state) coexists with asymmetric extraction
 *   (women bear cascading costs of legal fragmentation). The mechanism is
 *   maintained partly through state enforcement (courts adjudicate under each
 *   regime, registration recognizes multiple marriage regimes) and partly
 *   through community and family structures that make exit costly across
 *   material and identity dimensions. The theater ratio (0.64) reflects that
 *   much public discourse about 'protecting religious freedom' and
 *   'respecting community identity' performs accommodation while actually
 *   administering patriarchy — the state presents itself as neutral arbiter
 *   when it is actually an active enforcer of fragmented gendered authority.
 *
 * KEY AGENTS:
 *   - Woman Litigant: Primary victim (powerless/trapped) — faces compounding barriers from legal fragmentation, community pressure, and economic dependency; cannot exit without cascading social cost
 *   - Women's Rights Activists Embedded in Community: Secondary victim / activist (moderate/identity_locked) — structurally mobile but identity-fused with community; pursuit of uniform rights risks perceived betrayal; experience both coordination benefit and extraction
 *   - Religious Institutional Authorities (Hindu, Muslim, Christian, Parsi): Primary beneficiaries (institutional/arbitrage) — maintain interpretive authority over family governance; experience constraint as legitimate community coordination; can arbitrage between religious law and secular law
 *   - State Secular Apparatus: Institutional mediator (institutional/arbitrage) — administers fragmentation through courts and registration; experiences as manageable compromise; benefits from avoiding uniform civil code political cost
 *   - Constitutional Reform Coalition: Organized agents (organized/constrained) — work toward uniform civil code and gender-equal family law; see fragmentation as solvable through reform; constrained by political resistance and genuine pluralism concerns
 *   - Cross-Religious Women's Movement: Potential but blocked (moderate/constrained) — shared grievances exist but institutional fragmentation and identity commitment block coalition formation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices (state enforcement of personal law) as immutable features of pluralism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gender_justice_fragmentation, 0.58).
domain_priors:suppression_score(gender_justice_fragmentation, 0.68).
domain_priors:theater_ratio(gender_justice_fragmentation, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gender_justice_fragmentation, extractiveness, 0.58).
narrative_ontology:constraint_metric(gender_justice_fragmentation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gender_justice_fragmentation, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gender_justice_fragmentation, tangled_rope).
narrative_ontology:human_readable(gender_justice_fragmentation, "Gender Justice Fragmentation in India's Personal Law Regimes").
narrative_ontology:topic_domain(gender_justice_fragmentation, "constitutional_law/legal_pluralism/comparative_family_law").

domain_priors:requires_active_enforcement(gender_justice_fragmentation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gender_justice_fragmentation, '24b5ca76-80b7-4d6d-8aa3-1c22c993d408').
narrative_ontology:cs_created_at('24b5ca76-80b7-4d6d-8aa3-1c22c993d408', '').
narrative_ontology:cs_kernel_codification('24b5ca76-80b7-4d6d-8aa3-1c22c993d408', distributed).
narrative_ontology:cs_authority_grounding('24b5ca76-80b7-4d6d-8aa3-1c22c993d408', distributed).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gender_justice_fragmentation, religious_institutional_authorities).
narrative_ontology:constraint_beneficiary(gender_justice_fragmentation, male_household_heads).
narrative_ontology:constraint_beneficiary(gender_justice_fragmentation, state_secular_apparatus).
narrative_ontology:constraint_victim(gender_justice_fragmentation, women_litigants).
narrative_ontology:constraint_victim(gender_justice_fragmentation, gender_equality_principle).
narrative_ontology:constraint_victim(gender_justice_fragmentation, constitutional_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMAN LITIGANT (SNARE) — Trapped by her religious community's personal law regime with no structural exit. If she seeks divorce under Hindu law, she faces different property rights and child custody standards than under Muslim law; if she seeks to escape via civil law, she may be cast out by community and lose kinship support. Exit options are constrained not by legal prohibition alone but by cascading social and economic consequences. The constraint extracts from her across multiple axes: property rights, guardianship, inheritance, divorce access. Maximum suppression because alternatives are presented as illegitimate within her community frame.
constraint_indexing:constraint_classification(gender_justice_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WOMEN'S RIGHTS ACTIVIST IN COMMUNITY (TANGLED ROPE) — Often structurally mobile (education, income, legal literacy, social networks) but identity_locked: their activist identity is constituted through engagement WITH their community, not outside it. Pursuing uniform gender equality law risks perceived betrayal of community integrity. Genuine coordination function exists (protecting women from violence, ensuring inheritance rights) but embedded in the same regime that extracts from them. They experience both the benefit of community belonging AND the extraction of constrained choice.
constraint_indexing:constraint_classification(gender_justice_fragmentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: RELIGIOUS INSTITUTIONAL AUTHORITY (ROPE) — Benefits from maintenance of personal law regime; experiences the constraint as legitimate coordination of community affairs. The authority deploys the rhetoric of 'protecting community identity' and 'religious freedom,' experiencing the constraint as necessary to coordinate marriage, divorce, inheritance within the faith community. Effective extraction: they maintain interpretive authority over gender relations, property rules, and family structure. But from their perspective this is coordination, not extraction — they are solving the problem of 'how does a faith community preserve itself?' Exit options are arbitrage: they can always invoke secular law as backup or redefine doctrine if challenged, maintaining flexibility.
constraint_indexing:constraint_classification(gender_justice_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM COALITION (SCAFFOLD) — Organized political and legal actors (women's movements, rights organizations, sympathetic legislators) work toward uniform civil code and gender-equal family law. They see the personal law fragmentation as a temporary coordination failure solvable through constitutional reform, judicial reinterpretation, and legislative change. The constraint exhibits scaffold properties: coordination function (managing religious pluralism) embedded with a visible sunset (uniform civil code movements, incremental court decisions narrowing personal law scope, international human rights pressure). Theater is moderate-high: much of the reform debate is public performance and legal theater; actual implementation remains incomplete across 75+ years.
constraint_indexing:constraint_classification(gender_justice_fragmentation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE SECULAR APPARATUS (PITON) — The Indian state maintains personal law regimes through institutional inertia despite constitutional tension (Article 44 aspires to uniform civil code, Articles 14-15 guarantee equal protection). The state apparatus experiences this as a manageable compromise: accommodating religious diversity while maintaining secular credentials. But the mechanism has largely degraded: the state itself is now the enforcer of religious law (registering divorces under multiple codes, adjudicating under incompatible standards, training judges in each regime) without solving the underlying conflict. Theater is high — the state performs 'religious accommodation' while actually administering religious justice, often at women's expense. The constraint persists because alternatives (uniform civil code) are politically costly; the state arbitrages between religious constituencies rather than resolving the tension.
constraint_indexing:constraint_classification(gender_justice_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INTER-INSTITUTIONAL COMPETITION AMONG RELIGIOUS AUTHORITIES (TANGLED ROPE) — Different religious institutions (Hindu, Muslim, Christian, Parsi, etc.) coordinate by coexisting in parallel rather than competing for unified authority. This solves one coordination problem (how to govern family law in a multi-religious state without civil war) but creates extraction through the incompatibility itself: a woman's rights or obligations differ based on her religious classification, not on secular principles. The authorities benefit from preserving boundaries; women lose from institutional fragmentation. Constrained exit because if one authority loosens gender rules, it risks losing adherents to other traditions or secular law.
constraint_indexing:constraint_classification(gender_justice_fragmentation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational frame, the fragmentation appears as an immutable constraint of religious pluralism: any state containing multiple faith traditions must either (a) impose uniform law (violating religious autonomy) or (b) permit personal law regimes (violating gender equality). The dilemma is presentedas inherent to the structure of pluralism itself — a false summit that naturalizes what is actually a contingent institutional choice.
constraint_indexing:constraint_classification(gender_justice_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gender_justice_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gender_justice_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gender_justice_fragmentation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gender_justice_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gender_justice_fragmentation, TR),
    TR >= 0.70.

:- end_tests(gender_justice_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The fragmentation creates compounding extraction across property rights, guardianship standards, divorce access, and inheritance rules. Women cannot shop for favorable law because community and family pressure constrain exit. The constraint has accumulated extractiveness over 75 years: initial accommodation (1950s, ε ≈ 0.38) reflected genuine compromise; subsequent decades saw judicial narrowing of some personal law abuses but continued state enforcement of others, along with rising court theater (more elaborate review procedures, more elaborate justifications, less actual substantive protection). The measurement shows modest accumulation from 0.38 to 0.60, stabilizing around 0.58 in recent years, suggesting the system has reached a stable but high-extraction equilibrium. The rise from 0.38 to 0.60 reflects both increasing awareness of extraction (measured by litigation) and increasing stratification as secular alternatives become available to urban women while rural and economically dependent women remain locked in fragmented personal law. Suppression (0.68): High. Multiple suppression mechanisms operate simultaneously: (1) structural — economic dependency, housing insecurity, childcare dependency, limited alternative legal access; (2) institutional — fragmented courts, fragmented bar associations, separate personal law regimes create procedural barriers to unified advocacy; (3) internalized — community pressure, identity fusion, normative acceptance of traditional gender roles, epistemic closure about alternatives, fear of family rejection. The measurement at suppression = 0.68 reflects that most suppression mechanisms are intact across 75 years, with modest erosion from increasing education and economic participation. Theater ratio (0.64): Moderate-high. The state and religious authorities perform elaborate public narratives about 'protecting religious freedom,' 'respecting community identity,' 'constitutional accommodation,' while actually administering patriarchal extraction. Judicial opinions cite sophisticated constitutional principles while enforcing traditional gender rules. Women's rights movements perform symbolic victories (court decisions narrowing personal law scope) with limited practical change. The measurement shows theater increasing from 0.42 (1950s: straightforward personal law enforcement with minimal rhetorical cover) to 0.64 (contemporary: elaborate constitutional performance masking limited substantive change). The stabilized theater at 0.64 suggests the system has found optimal balance between legitimacy performance and substantive extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces radically divergent classifications across structural positions. The woman litigant trapped in fragmented personal law sees snare: no exit, maximum extraction, full suppression. The women's activist embedded in community sees tangled rope: genuine coordination function (community preservation) alongside extraction; identity-locked exit because challenging personal law risks community expulsion. The religious authority sees rope: legitimate community coordination with no extraction — their perspective frames personal law as coordination benefit, not extraction tax. The reform coalition sees scaffold: temporary solvable problem with sunset (uniform civil code movements, judicial reform, generational attitude change). The state apparatus sees piton: a degraded but manageable compromise maintained through inertia, not function. The inter-institutional competition among religious authorities sees tangled rope from below: they coordinate horizontally by coexisting, but women lose from institutional fragmentation. The analytical observer risks seeing mountain: pluralism as immutable natural law — but structural data reveals the mountain as a false summit (state-enforced institutional choice, not natural necessity). The perspectival gap reveals that the constraint's classification depends almost entirely on the agent's structural position and whether they experience the fragmentation as a benefit (authorities), a workable compromise (state), or an extraction mechanism (women).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim declarations and exit options. Religious institutional authorities are beneficiaries with arbitrage exit (low d, low/negative χ) — they experience the constraint as coordination. Women litigants are victims with trapped exit (high d, high χ) — they experience maximum extraction. Women activists are victims with identity_locked exit (high d but modulated by perceived coordination function; approximately 0.89 → 1.28 via sigmoid) — they experience substantial extraction but with embedded coordination benefit. State apparatus is declared beneficiary with arbitrage exit (low d) — they arbitrage between religious constituencies. The reform coalition is organized victims with constrained exit (moderate d) — they work toward change but face political barriers. The inter-institutional authorities are beneficiaries with constrained exit (moderate d) — they benefit from fragmentation but worry about losing adherents to other traditions. The engine derives these d values from the structural declarations; the commentary explains the logical pathway from benefit/victim status + exit options to experienced extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for gender_justice_fragmentation is resolved by recognizing that the apparent zero-sum choice between gender equality and religious pluralism is itself contingent on institutional design. The false summit (mountain perspective) naturalizes the state's choice to enforce personal law regimes as an immutable requirement of pluralism. But structural analysis reveals this as a designed choice: the state could (a) enforce uniform civil code (violating some claims to religious autonomy), (b) permit voluntary religious arbitration without state enforcement (preserving autonomy while protecting exit), (c) enforce gender-reformed versions of each tradition (requiring state re-interpret religious law), or (d) continue current fragmentation (violating gender equality while performing neutrality). The tangled-rope classification captures that genuine coordination function (religious community preservation) coexists with asymmetric extraction (unequal gender rules). The mandatrophy is resolved by: (1) rejecting the false mountain (pluralism ≠ mandatory state-enforced patriarchy); (2) identifying the real coordination function (religious community identity) and distinguishing it from the extraction mechanism (state enforcement of fragmented gendered law); (3) recognizing that women activists who appear identity-locked are in fact constrained by institutional fragmentation, not by immutable community identity. The key insight is that the constraint is maintained by institutional inertia (piton properties) in its performance layer (state accommodation rhetoric, elaborate judicial theater) combined with genuine coordination function (religious pluralism) in its substrate, alongside real extraction mechanism (fragmented gender rights). Reform requires either (a) resolving toward uniform civil code with gender equality (imposing one legal framework but preserving religious practice), (b) shift toward voluntary arbitration (preserving religious autonomy without state enforcement), or (c) require gender-reformed religious law (state-enforced gender equality within each religious tradition). Each pathway has trade-offs; none is costless. The mandatrophy is resolved by naming the trade-offs explicitly and making the institutional choice transparent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_multiplicity_vs_fragmentation,
    'Is this one contested kernel (marriage/family law) read through multiple religious lenses, or genuinely distinct kernels coexisting without unified normative foundation?',
    'If unified kernel: cross-religious doctrinal analysis would reveal shared deep structure beneath surface variation. If distinct kernels: each religious tradition would have incommensurable foundational premises about family authority, property, gender role, and exit conditions. Examine whether reform movements cite shared principles or appeal to distinct values.',
    'If unified kernel: the system is theoretically solvable through interpretive convergence or authoritative reinterpretation. If distinct kernels: current fragmentation may be the only stable equilibrium — unified civil code would require imposing one reading''s premises on all, violating pluralism. Classification changes from tangled_rope (resolvable hybrid) toward piton (degraded but stable) or distributed snare (incommensurable systems producing cascading extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_multiplicity_vs_fragmentation, conceptual, 'Whether fragmentation reflects one contested kernel or multiple incommensurable kernels').

omega_variable(
    gender_equality_vs_religious_autonomy_false_dilemma,
    'Is the tension between gender equality and religious autonomy genuinely zero-sum, or does it reflect artificial institutional choices (e.g., state enforcement of personal law vs. voluntary religious arbitration)?',
    'Comparative analysis: jurisdictions where religious family law is voluntary (not state-enforced) vs. mandatory. Study rates of women''s exit and satisfaction in opt-in vs. opt-out frameworks. Examine whether gender-reformed versions of each religious tradition exist (e.g., gender-equal Islamic family codes in some countries) and what blocks adoption in India.',
    'If genuinely zero-sum: current fragmentation may be least-bad option. If artificial: the constraint is a choice to state-enforce personal law, not an immutable feature of pluralism. Reclassification from mountain (natural limit) to snare (extractive enforcement choice). High impact on reform strategy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_equality_vs_religious_autonomy_false_dilemma, empirical, 'Whether gender equality / religious autonomy tension is inherent or institutional').

omega_variable(
    solidarity_blocking_in_cross_religious_movements,
    'Why have women''s rights movements not bridged across religious communities to demand uniform protections, despite shared grievances?',
    'Network analysis of women''s organizations; historical timeline of attempted cross-religious coalitions and their breakdown points. Examine whether institutional fragmentation (separate personal law courts, separate bar associations, separate civil society organizations) prevents coalition formation or whether ideological commitment to religious community identity prevents it.',
    'If institutional fragmentation blocks coalition: organized power (aggregate women''s movement) is suppressed by structural separation. Reclassify as higher suppression, piton-toward-snare. If identity commitment blocks coalition: identity_locked exit is the binding mechanism; the constraint is maintained partly through internalized framing, not material barriers. Affects vulnerability assessment and reform pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(solidarity_blocking_in_cross_religious_movements, empirical, 'Whether institutional fragmentation or identity commitment blocks cross-religious women''s solidarity').

omega_variable(
    state_neutrality_vs_state_enforcement_paradox,
    'Does the state''s claim to ''neutral accommodation'' of personal law regimes mask active enforcement of religious patriarchy, or does state non-enforcement of uniform law constitute genuine neutrality?',
    'Examination of state role: does the state enforce personal law through courts, registration, law enforcement, or do personal law authorities operate independently? Where state enforces (India does), catalog extractive mechanisms the state administers (e.g., state-enforced religious divorce bars, state recognition of differential inheritance, state registration of marriages under different regimes). Compare to jurisdictions where religious law is voluntary arbitration.',
    'If state actively enforces: the constraint is not pluralism but state-administered patriarchy. Reclassify toward snare (state is institutional beneficiary/enforcer). If state is genuinely neutral (no enforcement): some extraction shifts to community and family level, reducing state''s role from piton to facilitator. Changes whether reform target is state law or community norms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_neutrality_vs_state_enforcement_paradox, empirical, 'Whether state neutrality claim masks active patriarchy enforcement').

omega_variable(
    generational_persistence_mechanism,
    'Why has this fragmented personal law system persisted for 75+ years without either resolution toward uniform civil code or toward complete religious autonomy? What stabilizes the unstable equilibrium?',
    'Political economy analysis: which constituencies benefit from fragmentation? Which bear costs? What blocks uniform civil code adoption? Examine whether persistence reflects (a) genuine multi-party preference for status quo, (b) veto-player dynamics where no coalition can impose reform, (c) path dependency (reform costs exceed stability costs), or (d) institutional inertia with low political will.',
    'If (a): fragmentation may be stable preferred equilibrium, not pathology. If (b): reform requires coalition-building across constituencies. If (c) or (d): fragmentation is sticky but changeable. Affects classification ceiling: whether system can improve or is locked in place.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_persistence_mechanism, empirical, 'Mechanism stabilizing fragmentation without resolution').

omega_variable(
    identity_locked_vs_material_trapped_suppression,
    'For women in fragmented personal law regimes, is suppression primarily structural (material barriers to exit: economic dependency, housing, childcare, legal access) or primarily internalized (identity fusion with community, normative acceptance, epistemic closure about alternatives)?',
    'Post-exit analysis: women who exit their personal law regime and adopt secular law or move to different religious framework — do they report persistent suppression (internalized), or does suppression disappear once material barriers are removed? Longitudinal interviews tracking changes in agency perception after exit. Examine whether women cite material barriers or identity/cultural barriers as primary constraint.',
    'If structural: reform focus is material support (economic independence, legal access, alternative housing). If internalized: reform focus is identity-work (alternative narratives, community re-engagement, epistemic liberation). If mixed: both required. Affects omega_suppression_decomposition and informs intervention strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_material_trapped_suppression, empirical, 'Decomposition of suppression into structural and internalized components').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gender_justice_fragmentation, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gjf_tr_t0, gender_justice_fragmentation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gjf_tr_t15, gender_justice_fragmentation, theater_ratio, 15, 0.51).
narrative_ontology:measurement(gjf_tr_t30, gender_justice_fragmentation, theater_ratio, 30, 0.61).
narrative_ontology:measurement(gjf_tr_t50, gender_justice_fragmentation, theater_ratio, 50, 0.64).
narrative_ontology:measurement(gjf_tr_t75, gender_justice_fragmentation, theater_ratio, 75, 0.64).

% Extraction over time
narrative_ontology:measurement(gjf_be_t0, gender_justice_fragmentation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gjf_be_t15, gender_justice_fragmentation, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(gjf_be_t30, gender_justice_fragmentation, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(gjf_be_t50, gender_justice_fragmentation, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(gjf_be_t75, gender_justice_fragmentation, base_extractiveness, 75, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gender_justice_fragmentation, identity_coordination).
narrative_ontology:affects_constraint(gender_justice_fragmentation, uniform_civil_code_aspiration).
narrative_ontology:affects_constraint(gender_justice_fragmentation, constitutional_article_44_dormancy).

% DUAL FORMULATION NOTE:
% Gender justice fragmentation is the constraint imposed BY the personal law system. It is distinct from but causally linked to (1) the uniform civil code aspiration (downstream: uniform civil code would resolve the fragmentation constraint, though at the cost of imposing one legal framework), (2) the constitutional article 44 dormancy (upstream: the constitutional aspiration for uniformity creates pressure but insufficient political force for implementation). These three constraints form a family where the middle constraint (gender justice fragmentation) is maintained by the tension between the aspirational constraint above (article 44, pushing toward uniform law) and the entrenched institutional constraints below (religious communities, each maintaining their personal law). The fragmentation is the actual, functioning constraint that absorbs the pressure from article 44's aspirational force without breaking.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
