% ============================================================================
% CONSTRAINT STORY: muslim_uncodified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_muslim_uncodified_reading, []).

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
 *   constraint_id: muslim_uncodified_reading
 *   human_readable: Uncodified Shariat Authority in Muslim Family Law
 *   domain: constitutional_law/legal_pluralism/family_law
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'marriage/family law authority' — specifically, the reading that derives
 *   marriage authority from Shariat as interpreted by religious authorities,
 *   resisting state codification. Under this reading, family law is not a
 *   state function but a religious/community function. The Shariat
 *   Application Act (1937) codified this resistance to codification: it
 *   established that family law disputes involving Muslims would be
 *   adjudicated according to Islamic law as interpreted by religious
 *   authorities, not state statute. This constraint exhibits high
 *   perspectival variance across the six observer positions. From the
 *   perspective of Muslim women trapped by patriarchal provisions (unilateral
 *   talaq, inheritance asymmetry, guardianship subordination), it is a snare.
 *   From the perspective of religious interpretation authorities, it is pure
 *   coordination (rope). From the state's perspective, it is a mixed
 *   coordination-extraction hybrid (tangled rope) that constrains state
 *   capacity while accommodating religious freedom. From women's rights
 *   advocates, it is a scaffold with a generational sunset as women's
 *   autonomy increases. From the civilizational view, it is either a piton
 *   (institutional inertia maintaining a degraded colonial compromise) or a
 *   snare (extractive patriarchal authority masked by religious legitimacy).
 *   The constraint's theater_ratio (0.55) reflects that religious legitimacy
 *   itself is partly performative — the sacred text interpretation is real,
 *   but its selective use to preserve patriarchal authority while resisting
 *   women's rights reform reveals the theater mechanism. The extractiveness
 *   increase from 0.52 to 0.58 over 30 years reflects accumulating
 *   suppression as women's education and economic participation increase but
 *   patriarchal provisions persist.
 *
 * KEY AGENTS:
 *   - Muslim Women Under Patriarchal Provisions: Primary victim (powerless/trapped) — subject to unilateral talaq, inheritance subordination, guardianship restrictions; high suppression from social costs of non-compliance and internalized religious legitimacy
 *   - Male Household Heads: Primary beneficiary (moderate/constrained) — extract unilateral authority through talaq, inheritance priority, guardianship; experience constraint as coordination and entitlement
 *   - Religious Interpretation Authorities (Ulema, Mufti Councils): Institutional beneficiary (institutional/arbitrage) — derive legitimacy, institutional power, resource flows, and enforcement capacity from interpreting/enforcing Shariat; voluntary maintenance of uncodified system
 *   - The State Legal System: Institutional victim-beneficiary (institutional/constrained) — constrained by constitutional pluralism; cannot unilaterally impose secular law; carries costs of managing dual jurisdiction; extracts institutional power by deferring family law
 *   - Women's Rights Coalition: Organized reform agents (organized/constrained) — pursuing generational pathways: feminist Quranic interpretation, talaq registration, inheritance reform, women's educational/economic autonomy; building scaffold exit mechanism
 *   - Colonial Administrative Legacy: Institutional framework (institutional/arbitrage) — Shariat Application Act (1937) was colonial compromise to minimize state burden; now functions as inertial institutional theater
 *   - Analytical Observer: Cross-position vantage (analytical/analytical) — sees the full perspectival range and identifies the contestation between religious-legitimacy narratives and patriarchal-extraction structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(muslim_uncodified_reading, 0.58).
domain_priors:suppression_score(muslim_uncodified_reading, 0.68).
domain_priors:theater_ratio(muslim_uncodified_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(muslim_uncodified_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(muslim_uncodified_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(muslim_uncodified_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(muslim_uncodified_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(muslim_uncodified_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(muslim_uncodified_reading, tangled_rope).
narrative_ontology:human_readable(muslim_uncodified_reading, "Uncodified Shariat Authority in Muslim Family Law").
narrative_ontology:topic_domain(muslim_uncodified_reading, "constitutional_law/legal_pluralism/family_law").

domain_priors:requires_active_enforcement(muslim_uncodified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(muslim_uncodified_reading, '2822310d-5113-49b9-bd6c-e0ca51d303ae').
narrative_ontology:cs_created_at('2822310d-5113-49b9-bd6c-e0ca51d303ae', '').
narrative_ontology:cs_kernel_codification('2822310d-5113-49b9-bd6c-e0ca51d303ae', formalized).
narrative_ontology:cs_authority_grounding('2822310d-5113-49b9-bd6c-e0ca51d303ae', lineage).
narrative_ontology:cs_interpretation_layer_present('2822310d-5113-49b9-bd6c-e0ca51d303ae').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(muslim_uncodified_reading, male_household_heads).
narrative_ontology:constraint_beneficiary(muslim_uncodified_reading, religious_interpretation_authorities).
narrative_ontology:constraint_victim(muslim_uncodified_reading, muslim_women_in_patriarchal_provisions).
narrative_ontology:constraint_victim(muslim_uncodified_reading, state_codification_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MUSLIM WOMEN UNDER PATRIARCHAL PROVISIONS (SNARE) — Trapped by religious family law provisions that permit unilateral talaq (pre-2017), restrict inheritance, and subordinate women's authority within marriage. Exit options are material and psychological: divorce carries severe social stigma, economic dependency is enforced through inheritance asymmetry, and the constraint is legitimated through religious authority that the victim themselves may internalize. Maximum suppression and maximum experienced extraction from this perspective.
constraint_indexing:constraint_classification(muslim_uncodified_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MUSLIM MEN IN PATRIARCHAL PROVISIONS (TANGLED ROPE) — Constrained but benefiting. The constraint genuinely coordinates marriage, inheritance, and family obligation norms; it provides a framework for legitimate family organization and social standing. However, it also extracts asymmetric authority: men hold unilateral talaq, superior inheritance, and household decision-making. The extraction is real but mixed with coordination function — men experience the constraint as both coordination and entitlement.
constraint_indexing:constraint_classification(muslim_uncodified_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RELIGIOUS INTERPRETATION AUTHORITIES (ROPE) — Institutional beneficiaries with arbitrage options. Religious authorities (ulema, mufti councils, religious courts) derive legitimacy, institutional power, and resource flows from interpreting and enforcing Shariat family law. The constraint is experienced as pure coordination from their perspective: they are coordinating family law according to recognized religious principles. Exit options are strong (they could adopt state codification but choose not to) — classification as rope reflects their voluntary maintenance of the uncodified system.
constraint_indexing:constraint_classification(muslim_uncodified_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE STATE LEGAL SYSTEM (TANGLED ROPE) — Constrained by constitutional recognition of legal pluralism and the Shariat Application Act (1937). The state experiences this as both coordination and extraction: genuine coordination of religious family law for minority populations who recognize Shariat authority, but also extraction of state codification capacity and jurisdictional authority. The state cannot unilaterally impose secular family law without violating religious freedom guarantees; cannot fully codify Shariat without entrenching patriarchal provisions in state law. Constrained exit — the state bears costs without full control.
constraint_indexing:constraint_classification(muslim_uncodified_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WOMEN'S RIGHTS ADVOCACY COALITION (SCAFFOLD) — Organized agents (women's rights groups, secular civil society, progressive religious voices) see the uncodified Shariat reading as a temporary structural problem with a generational sunset. They pursue gradual reformation pathways: reinterpreting Shariat through feminist hermeneutics, building alternative religious authorities that derive legitimacy from sacred texts while reforming patriarchal provisions, codifying protections (talaq registration, mahr enforcement, inheritance rights) within the legal pluralism framework. These are exit mechanisms with sunset logic: as women's educational attainment, economic participation, and political voice increase, patriarchal enforcement becomes harder to sustain. Constrained exit but visible pathway.
constraint_indexing:constraint_classification(muslim_uncodified_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: COLONIAL INSTITUTIONAL ARCHITECTURE (PITON) — From the civilizational perspective, the uncodified Shariat framework is a degraded institutional residue of colonial-era compromise. The Shariat Application Act (1937) was designed to manage religious minorities within imperial administration, deferring family law to religious authorities to minimize state administrative burden. The theater_ratio is high (0.55): performative religious legitimacy masks what is actually a state governance delegation. The framework persists through institutional inertia — it is easier to maintain plural interpretation authorities than to reform family law through democratic codification. The original coordination function (managing religious heterogeneity) has atrophied; what remains is theater (religious authority as legitimating mechanism for patriarchal extraction).
constraint_indexing:constraint_classification(muslim_uncodified_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From the global/civilizational analytical perspective, the uncodified Shariat reading instantiates a structural snare: it uses religious legitimacy to suppress state codification pathways that would otherwise expand women's legal protections. The constraint suppresses alternative readings (Hindu Succession Act codification, secular opt-in reform, gender-neutral inheritance), suppresses women's exit options (reinforces social costs of non-compliance), and extracts patriarchal authority across a population. The religious framing naturalizes what is actually institutional extraction. High suppression, high extractiveness, minimal coordination function. Classification as snare reflects that the constraint's primary function is extractive (enforcing patriarchal authority), not coordinative.
constraint_indexing:constraint_classification(muslim_uncodified_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(muslim_uncodified_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(muslim_uncodified_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(muslim_uncodified_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(muslim_uncodified_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(muslim_uncodified_reading, TR),
    TR >= 0.70.

:- end_tests(muslim_uncodified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts patriarchal authority from women and state codification capacity from the state. However, it is not maximum extraction (0.72+) because the coordinate function is partially genuine — family law does coordinate marriage formation, inheritance, and guardianship across a population of believers who recognize Shariat authority. The extraction is real but layered beneath coordination. The value reflects mixed function: genuine coordination of religious family law + asymmetric extraction of women's autonomy + institutional extraction from the state. Suppression (0.68): High. Multiple suppression mechanisms: (1) Social stigma/economic cost of divorce (unilateral talaq suppresses women's exit). (2) Inheritance asymmetry suppresses women's economic independence. (3) Guardianship restrictions suppress women's legal agency. (4) Religious legitimacy suppresses women's consciousness of the constraint as constructed rather than natural/divine. (5) State codification is suppressed by constitutional pluralism. Theater ratio (0.55): Moderate. Religious legitimacy provides genuine interpretive authority but is selectively applied to preserve patriarchal provisions while resisting women's rights reform. The theater has increased as women's education rose and reform movements emerged — the performative maintenance of patriarchal provisions against contrary evidence of women's capacity has become more visible.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival variance across the seven perspectives. The trapped Muslim woman sees a snare (pure extraction, no exit, full suppression). The beneficiary male sees tangled rope (mixing coordination and authority). The religious authority sees rope (pure coordination). The state sees tangled rope (constrained by pluralism). The women's rights coalition sees scaffold (temporary, with sunset). The civilizational inertia perspective sees piton (degraded theater). The analytical observer sees snare (extraction masked by legitimacy). The gaps are not measurement artifacts but reflect genuinely different structural positions: beneficiaries and coordinators experience the constraint as less extractive than victims do. The religious framing is experienced as legitimating by authorities and believers, but as suppressive by women subjected to patriarchal provisions. The state experiences constraints from both directions: constrained by pluralism from above, constrained by patriarchal suppression from below.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Male household heads are beneficiaries (unilateral talaq, inheritance, authority) with constrained exit — d ≈ 0.35. Religious authorities are institutional beneficiaries with arbitrage (voluntary maintenance) — d ≈ 0.10. Muslim women are victims with trapped exit (high social/economic cost) — d ≈ 0.88. The state is a constrained victim of pluralism (forced to accommodate) yet institutional beneficiary (derives power from administering pluralism) — dual directionality produces the tangled rope classification. Women's rights advocates are organized agents with constrained exit but building alternative pathways — d ≈ 0.45, producing scaffold perspective. These directionality values feed f(d) to compute experienced extractiveness (chi) for each perspective. The wide spread in d across perspectives (0.10 to 0.88) reflects the fundamental asymmetry in the constraint: beneficiaries experience low extraction, victims experience high extraction, the state experiences a middle bind.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL SITE MANDATROPHY: The constraint resolves the mandatrophy by showing that the six classification types capture genuinely different structural positions within a single contested kernel reading. The dispute is not 'which type is correct' but 'whose perspective is this?' The beneficiary's rope is honest — religious authorities do experience the constraint as coordination. The victim's snare is honest — trapped women do experience maximum extraction. The scaffold is honest — women's rights advocates genuinely build generational pathways out. The piton is honest — the institutional inertia of colonial compromise persists. The analytical observer's snare is honest — the global/civilizational view reveals that patriarchal extraction is the primary function masked by religious framing. NO SINGLE TYPE is 'the answer.' The presheaf of types over the observation site is the answer. The mandatrophy is resolved by recognizing that different agents in structurally different positions experience the same constraint as different types, and all those experiences are real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contested_kernel_reading,
    'Is the Muslim uncodified Shariat reading a genuine alternative instantiation of marriage/family law authority, or a false naturalization of patriarchal extraction?',
    'Comparative analysis with sibling readings (Hindu Codified, Secular Opt-in): Do alternative readings show structurally equivalent coordination functions with different distributional consequences? Or do they reveal that ''religious authority'' is itself a cover story for patriarchal governance?',
    'If genuine alternative: classification shifts toward Rope (pure coordination) or Tangled Rope with lower extraction emphasis. If false naturalization: classification confirmed as Snare from analytical perspective, FSM candidate (mountain naturalized). The constraint''s legitimacy claim rests on this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contested_kernel_reading, conceptual, 'Whether uncodified Shariat is a genuine authority reading or patriarchal naturalization').

omega_variable(
    religious_legitimacy_sufficiency,
    'Does religious legitimacy constitute genuine coordination function, or is it a legitimating mechanism for extraction?',
    'Structural decomposition: (1) Does Shariat family law coordinate genuine collective action problems (marriage formation, inheritance, guardianship) that secular law could not solve? (2) Or does it primarily organize asymmetric authority (unilateral talaq, male inheritance priority) that benefits male household heads and religious authorities at women''s cost? (3) Cross-domain comparison: Do other legitimated systems (caste, feudal hierarchy, monarchical authority) show similar structure?',
    'If coordination-primary: Rope or Tangled Rope (mixed) classification appropriate. If extraction-primary: Snare classification appropriate. Religious framing does not determine whether the underlying function is coordinative or extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_legitimacy_sufficiency, empirical, 'Whether religious legitimacy indicates coordination or extraction').

omega_variable(
    reform_pathway_viability,
    'Can progressive religious hermeneutics and feminist Quranic interpretation create a structural exit pathway (Scaffold sunset), or is patriarchal enforcement too deeply institutionalized?',
    'Longitudinal tracking of reform outcomes: (1) Talaq registration laws and mahr enforcement (post-2017 changes). (2) Women''s educational attainment and economic participation trends. (3) Adoption rates of reformed religious interpretations vs. traditional patriarchal provisions. (4) State capacity to enforce codified protections within legal pluralism framework. (5) Generational shift in women''s willingness to exit vs. reformulate.',
    'If viable: Scaffold perspective is structural. Patriarchal extraction is genuinely degrading as women''s structural autonomy increases. If not viable: reform pathways are theater (performative without structural change); the snare perspective is dominant. Determines whether the constraint is temporary or persistent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_pathway_viability, empirical, 'Whether feminist religious reform can sustainably exit patriarchal provisions').

omega_variable(
    legal_pluralism_extraction,
    'Does legal pluralism itself (deferring family law to religious authorities) constitute an extraction mechanism, separate from the specific patriarchal provisions of Shariat?',
    'Comparative constitutional analysis: Does legal pluralism coupled with religious authority produce higher extraction rates across different religious traditions (Hindu, Islamic, Christian, customary)? Is the pluralism framework used strategically to shield patriarchal provisions from democratic reform pressure?',
    'If pluralism-as-extraction: The constraint''s suppression value increases; state codification capacity itself is the victim. The snare classification is reinforced. If pluralism-as-genuine-accommodation: The Rope/Tangled Rope classification gains strength. Distinguishes between bad pluralism (extraction cover) and pluralism-in-principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_pluralism_extraction, empirical, 'Whether legal pluralism itself functions as extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(muslim_uncodified_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(musl_tr_t0, muslim_uncodified_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(musl_tr_t15, muslim_uncodified_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(musl_tr_t30, muslim_uncodified_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(musl_be_t0, muslim_uncodified_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(musl_be_t15, muslim_uncodified_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(musl_be_t30, muslim_uncodified_reading, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(muslim_uncodified_reading, identity_coordination).
narrative_ontology:affects_constraint(muslim_uncodified_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(muslim_uncodified_reading, secular_opt_in_reading).

% DUAL FORMULATION NOTE:
% The marriage/family law authority constraint decomposes into three readings instantiating three distinct structural constraints with different ε values, victim sets, and beneficiary structures. Muslim uncodified reading (ε=0.58) generates high extraction through patriarchal provisions masked by religious legitimacy. Hindu codified reading (ε=0.35) reduces extraction by codifying statutory protections. Secular opt-in reading (ε=0.22) further reduces extraction by expanding choice. Each reading is a separate constraint story; they are linked via network.affects_constraints and should be analyzed as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(muslim_uncodified_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
