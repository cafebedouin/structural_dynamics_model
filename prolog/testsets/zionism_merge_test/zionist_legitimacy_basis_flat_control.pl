% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis_flat_control, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: zionist_legitimacy_basis_flat_control
 *   human_readable: Zionist Historical Right Legitimacy Basis
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   The Zionist historical right claim emerged in the late 19th century as a
 *   legitimacy basis for Jewish territorial sovereignty in Palestine,
 *   grounding the claim in three pillars: ancient Jewish presence in the land
 *   (Kingdom of Israel/Judah, Second Temple period), biblical covenant
 *   (divine promise of the land to Abraham's descendants), and two millennia
 *   of diaspora longing for return (liturgical references, cultural memory,
 *   messianic hope). This constraint coordinates Jewish collective identity
 *   and mobilizes diaspora support for the state-building project, but also
 *   extracts from the indigenous Palestinian Arab population whose continuous
 *   presence and territorial claims are delegitimized by the historical right
 *   narrative. The constraint exhibits tangled rope structure: genuine
 *   coordination function (solving statelessness for a persecuted diaspora,
 *   providing collective identity framework) combined with substantial
 *   extraction (displacement of indigenous population, suppression of
 *   alternative territorial claims, continuous military enforcement). The
 *   theater_ratio (0.35) is moderate rather than high because the historical
 *   claim is not purely performative — it genuinely structures identity,
 *   mobilizes resources, and grounds state legitimacy, even though its
 *   logical coherence is contested. The constraint's extractiveness and
 *   suppression have increased substantially over the interval as the claim
 *   transitioned from diaspora aspiration (1880s) to state ideology backed by
 *   military power (post-1948), with a sharp inflection at 1947-1948 (UN
 *   partition, Nakba, state establishment) and stabilization after 1967
 *   (occupation of remaining Palestinian territories).
 *
 * KEY AGENTS:
 *   - Jewish Diaspora Communities: Mixed beneficiary/victim (moderate/constrained for non-Zionists) — benefit from collective identity framework and refuge option; non-Zionist traditions bear costs from suppression of alternative Jewish politics
 *   - Zionist Movement Institutions: Primary beneficiary (institutional/arbitrage) — coordinate diaspora mobilization, state-building, and international advocacy; net beneficiary of the legitimacy narrative
 *   - Israeli State Apparatus: Primary beneficiary (institutional/arbitrage) — the historical right claim grounds state legitimacy and territorial control
 *   - Palestinian Arab Population: Primary victim (powerless/trapped) — displaced by the constraint's operation; indigenous territorial claims delegitimized; cannot exit or organize effective counter-narrative against civilizational-scale historical framing
 *   - Alternative Jewish Political Traditions: Secondary victim (moderate/constrained) — Bundism, diasporism, cosmopolitan Judaism suppressed by Zionist hegemony within Jewish institutional life
 *   - International Human Rights Coalition: Organized observer (organized/constrained) — sees both legitimate self-determination claim and extractive displacement; constrained by diplomatic costs of challenging the legitimacy basis
 *   - Biblical Covenant Literalist: Theological observer (institutional/analytical) — sees immutable divine covenant; false summit perspective that naturalizes contested political claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis_flat_control, 0.78).
domain_priors:suppression_score(zionist_legitimacy_basis_flat_control, 0.82).
domain_priors:theater_ratio(zionist_legitimacy_basis_flat_control, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis_flat_control, extractiveness, 0.78).
narrative_ontology:constraint_metric(zionist_legitimacy_basis_flat_control, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(zionist_legitimacy_basis_flat_control, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis_flat_control, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(zionist_legitimacy_basis_flat_control, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis_flat_control, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis_flat_control, "Zionist Historical Right Legitimacy Basis").
narrative_ontology:topic_domain(zionist_legitimacy_basis_flat_control, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis_flat_control, '3aad5a65-d4f5-4ea0-a6c0-95c1bb35c8c6').
narrative_ontology:cs_kernel_codification('3aad5a65-d4f5-4ea0-a6c0-95c1bb35c8c6', distributed).
narrative_ontology:cs_authority_grounding('3aad5a65-d4f5-4ea0-a6c0-95c1bb35c8c6', distributed).
narrative_ontology:cs_created_at('3aad5a65-d4f5-4ea0-a6c0-95c1bb35c8c6', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(zionist_legitimacy_basis_flat_control, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis_flat_control, jewish_diaspora_communities).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis_flat_control, zionist_movement_institutions).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis_flat_control, israeli_state_apparatus).
narrative_ontology:constraint_victim(zionist_legitimacy_basis_flat_control, palestinian_arab_population).
narrative_ontology:constraint_victim(zionist_legitimacy_basis_flat_control, indigenous_land_claims).
narrative_ontology:constraint_victim(zionist_legitimacy_basis_flat_control, alternative_jewish_political_traditions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED PALESTINIAN (SNARE) — Trapped by the constraint's operation: cannot exit the territorial claim that delegitimizes indigenous presence, cannot organize effective counter-narrative against civilizational-scale historical framing backed by state power. The historical right claim functions as pure extraction — it provides no coordination benefit to the displaced population and actively suppresses alternative territorial claims. Maximum experienced extraction.
constraint_indexing:constraint_classification(zionist_legitimacy_basis_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NON-ZIONIST JEWISH SCHOLAR (TANGLED ROPE) — Constrained by identity and professional context: benefits from Jewish communal solidarity and institutional access, but also bears costs from the constraint's suppression of alternative Jewish political traditions (Bundism, diasporism, cosmopolitanism). The historical right narrative coordinates Jewish collective identity while extracting from those who hold non-nationalist Jewish commitments. Mixed experience: some coordination function, significant extraction.
constraint_indexing:constraint_classification(zionist_legitimacy_basis_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ZIONIST MOVEMENT INSTITUTION (ROPE) — Primary beneficiary with arbitrage-level exit options. The historical right claim coordinates diaspora mobilization, fundraising, political advocacy, and state legitimacy. Experiences the constraint as pure coordination: it solves the collective action problem of building and maintaining a nation-state project. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(zionist_legitimacy_basis_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL HUMAN RIGHTS COALITION (TANGLED ROPE) — Organized agents (UN bodies, NGOs, international law frameworks) see both coordination and extraction. The constraint coordinates a legitimate claim to self-determination for a historically persecuted people, but also extracts from indigenous populations and suppresses competing territorial claims. The coalition has agency but faces high costs to challenge the legitimacy basis directly (accusations of antisemitism, diplomatic isolation). Mixed extraction.
constraint_indexing:constraint_classification(zionist_legitimacy_basis_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: BIBLICAL COVENANT LITERALIST (MOUNTAIN) — From a theological literalist position, the historical right is an immutable divine covenant, not a contingent political claim. This perspective sees the constraint as unchangeable natural law grounded in theological authority. However, this is a false summit: the constraint's operation depends on active enforcement (state apparatus, military control, legal frameworks), has identifiable beneficiaries and victims, and meets substantial resistance. The 'divine covenant' framing naturalizes what is structurally a contested territorial claim.
constraint_indexing:constraint_classification(zionist_legitimacy_basis_flat_control, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational analytical perspective, the constraint exhibits both genuine coordination (mobilizing a dispersed diaspora, providing collective identity framework, solving statelessness for a persecuted population) and substantial extraction (displacing indigenous population, suppressing alternative claims, requiring continuous military enforcement). The historical right claim is not a natural law but a constructed legitimacy narrative that coordinates some agents while extracting from others. Tangled Rope classification reflects the irreducible hybrid structure.
constraint_indexing:constraint_classification(zionist_legitimacy_basis_flat_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(zionist_legitimacy_basis_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(zionist_legitimacy_basis_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The historical right claim extracts substantially from the Palestinian Arab population through displacement, delegitimization of indigenous territorial claims, and suppression of alternative narratives. The extraction is not total (0.78 rather than 0.95) because the constraint also provides genuine coordination benefits to Jewish diaspora communities (refuge from persecution, collective identity, self-determination) — the tangled rope structure means extraction and coordination coexist. The extractiveness increased sharply from early Zionist settlement (0.45 in 1880s, when the claim was aspirational and the Palestinian population was not yet substantially displaced) to state establishment (0.75 by 1947-1948, when displacement became systematic) and stabilized at high levels post-1967 (0.78, when remaining Palestinian territories came under Israeli control). Suppression (0.82): Very high. The constraint suppresses alternative territorial claims through military control, legal frameworks that privilege Jewish claims, and international diplomatic pressure. Palestinian counter-narratives face systematic suppression (Nakba denial, criminalization of commemoration, control of historical sites and archives). Alternative Jewish political traditions (non-Zionist, anti-Zionist, diasporist) face institutional marginalization within Jewish communal life. Suppression increased from moderate (0.25 in 1880s, when the claim was one among many Jewish political visions) to very high (0.82 post-1967, when state power consolidated the Zionist narrative as hegemonic). Theater ratio (0.35): Moderate. The historical right claim is not primarily performative — it genuinely structures collective identity, mobilizes resources, and grounds state legitimacy. However, some theatrical elements exist: selective historical memory (emphasis on ancient Jewish presence, de-emphasis of Palestinian continuity), ritualized commemoration (Independence Day vs. Nakba Day), and diplomatic performances of historical victimhood. The theater ratio increased modestly over the interval as the claim became more institutionalized and ritualized, but remains moderate because the constraint's primary function is substantive coordination and extraction, not performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence. The Zionist movement institution sees pure coordination (Rope) — the historical right solves the collective action problem of diaspora mobilization and state legitimacy. The displaced Palestinian sees pure extraction (Snare) — the claim delegitimizes indigenous presence and provides no coordination benefit. The non-Zionist Jewish scholar sees mixed coordination and extraction (Tangled Rope) — benefits from Jewish solidarity while bearing costs from suppression of alternative traditions. The international human rights coalition also sees Tangled Rope — legitimate self-determination claim combined with extractive displacement. The biblical covenant literalist sees immutable natural law (Mountain) — but this is a false summit that naturalizes a contested political claim. The analytical observer sees the irreducible hybrid structure (Tangled Rope at the analytical level) — the constraint coordinates some agents while extracting from others, and no single type captures the full structure. The perspectival gap is not a measurement error but the structural reality: the same constraint genuinely appears as coordination to beneficiaries and extraction to victims, and the analytical task is to map the presheaf rather than collapse it to a single type.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. The Zionist movement institutions and Israeli state apparatus are primary beneficiaries with arbitrage-level exit options — they experience low or negative effective extraction (the constraint subsidizes them). The Palestinian Arab population is the primary victim with trapped exit options — they experience maximum effective extraction (high d → high f(d) → high chi). Jewish diaspora communities are split: Zionist-aligned members are beneficiaries (low d), while non-Zionist traditions are victims of suppression (high d). The international human rights coalition is organized with constrained exit — they experience moderate extraction (they can observe and critique but face high costs for direct challenge). The biblical covenant literalist perspective is institutional/analytical but represents a false summit — the theological framing naturalizes what is structurally a contested political claim with identifiable beneficiaries and victims. The analytical observer perspective sees the tangled rope structure: genuine coordination for some agents, substantial extraction from others, with no single 'correct' type but rather a presheaf of perspectival classifications over the observation site.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that Tangled Rope is the analytically correct classification when a constraint exhibits both genuine coordination and substantial extraction. The historical right claim is not 'really' a Rope (pure coordination) that critics mischaracterize as extraction, nor is it 'really' a Snare (pure extraction) that beneficiaries rationalize as coordination. It is structurally both: it coordinates Jewish diaspora collective action and provides a framework for self-determination (solving the genuine problem of statelessness for a persecuted population), AND it extracts from the Palestinian Arab population through displacement and delegitimization of indigenous claims. The Tangled Rope classification captures this irreducible hybrid structure. The mandate (provide refuge and self-determination for Jewish diaspora) has not outlived its function, but the constraint's operation has accumulated substantial extraction beyond what the coordination function requires. The mandatrophy question is not 'has the mandate expired?' but 'does the extraction exceed what the coordination function justifies?' The analytical observer's Tangled Rope classification reflects this: the constraint is neither pure coordination nor pure extraction, but a hybrid where both functions coexist and the extraction cannot be eliminated without dismantling the coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ancient_presence_threshold,
    'What temporal threshold of historical presence generates legitimate territorial claims? Does 2000 years of diaspora absence negate ancient presence, or does continuous cultural memory preserve the claim?',
    'Comparative analysis of international law precedents for indigenous land claims; examination of how other post-diaspora populations (Armenians, Tibetans, Crimean Tatars) are treated in territorial disputes; philosophical analysis of what grounds territorial legitimacy (continuous occupation vs. historical connection vs. self-determination).',
    'If ancient presence alone suffices: many other diaspora populations gain territorial claims. If continuous occupation required: the Zionist claim loses its historical legitimacy basis and must rest on other grounds (self-determination, refuge from persecution, international recognition). If cultural memory suffices: the threshold becomes subjective and contestable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ancient_presence_threshold, conceptual, 'Temporal threshold for historical presence to generate territorial claims').

omega_variable(
    competing_indigenous_claims,
    'How should competing indigenous claims be adjudicated when both populations have documented historical presence in the same territory? Palestinians have continuous presence since at least the 7th century; Jews have ancient presence but 2000-year diaspora gap.',
    'International law frameworks for resolving overlapping indigenous claims; historical demographic analysis of population continuity; examination of whether Palestinian Arabs are descendants of earlier populations (including ancient Israelites who converted or remained) vs. later Arab settlers.',
    'If continuous presence trumps ancient presence: Palestinian claim is stronger. If ancient presence trumps continuous presence: Jewish claim is stronger. If both are legitimate: the constraint must be reframed as a coordination problem (two-state solution, binational state) rather than a zero-sum legitimacy contest. If demographic continuity analysis shows Palestinian Arabs are partly descended from ancient populations: the ''return'' narrative becomes more complex.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_indigenous_claims, empirical, 'Adjudication framework for competing indigenous territorial claims').

omega_variable(
    diaspora_longing_operationalization,
    'Does diaspora longing for return constitute a legitimate basis for territorial claims? If so, what threshold of longing (liturgical references, cultural memory, active return movements) is required?',
    'Comparative analysis of how other diaspora populations'' longing is treated in international law (Armenian diaspora and historical Armenia, Greek diaspora and Anatolia, African diaspora and Africa); philosophical analysis of whether subjective longing generates objective territorial rights; examination of whether Zionist return represents majority diaspora sentiment or organized minority movement.',
    'If longing alone suffices: many diaspora populations gain territorial claims. If longing must be operationalized through active return movements: the threshold becomes organizational capacity rather than cultural memory. If longing is insufficient without other grounds: the Zionist claim must rest on self-determination or refuge rather than historical right.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diaspora_longing_operationalization, preference, 'Whether diaspora longing generates legitimate territorial claims').

omega_variable(
    biblical_covenant_secular_legitimacy,
    'Can a biblical covenant serve as a legitimate basis for territorial claims in secular international law? Does theological authority translate to political legitimacy in a multi-religious context?',
    'Analysis of international law''s treatment of religious territorial claims (Vatican, Mecca/Medina, Jerusalem holy sites); examination of whether secular states can ground legitimacy in religious texts; philosophical analysis of the relationship between theological and political authority.',
    'If biblical covenant is legitimate: other religious groups gain territorial claims based on sacred texts (Islamic claims to Andalusia, Hindu claims to Ayodhya, etc.). If biblical covenant is illegitimate in secular law: the Zionist claim must rest on non-theological grounds (historical presence, self-determination, international recognition). If covenant is legitimate only for believers: the claim becomes identity-locked rather than universal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biblical_covenant_secular_legitimacy, conceptual, 'Legitimacy of biblical covenant as basis for territorial claims in secular law').

omega_variable(
    persecution_refuge_vs_historical_right,
    'Is the Zionist project better understood as a refuge from persecution (which would ground legitimacy in humanitarian necessity and self-determination) or as a historical right (which grounds legitimacy in ancient presence and covenant)? Do these framings produce different obligations toward the indigenous population?',
    'Historical analysis of Zionist movement''s primary motivations (Herzl''s response to European antisemitism vs. religious Zionism''s covenant claims); examination of how different framings affect obligations to Palestinians (refuge framing might require compensation/accommodation; historical right framing might claim exclusive legitimacy); analysis of how the two framings interact in Israeli state ideology.',
    'If refuge framing is primary: obligations to indigenous population are stronger (the refuge claim doesn''t negate their presence). If historical right framing is primary: the claim is more absolute but also more contestable (ancient presence vs. continuous presence). If both framings coexist: the constraint exhibits internal tension between humanitarian and nationalist logics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(persecution_refuge_vs_historical_right, conceptual, 'Primary legitimacy basis: refuge from persecution vs. historical right').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis_flat_control, 0, 146).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_leg_theater_1880, zionist_legitimacy_basis_flat_control, theater_ratio, 0, 0.15).
narrative_ontology:measurement(zion_leg_theater_1900, zionist_legitimacy_basis_flat_control, theater_ratio, 20, 0.2).
narrative_ontology:measurement(zion_leg_theater_1920, zionist_legitimacy_basis_flat_control, theater_ratio, 40, 0.25).
narrative_ontology:measurement(zion_leg_theater_1947, zionist_legitimacy_basis_flat_control, theater_ratio, 67, 0.3).
narrative_ontology:measurement(zion_leg_theater_1968, zionist_legitimacy_basis_flat_control, theater_ratio, 88, 0.32).
narrative_ontology:measurement(zion_leg_theater_2000, zionist_legitimacy_basis_flat_control, theater_ratio, 120, 0.35).
narrative_ontology:measurement(zion_leg_theater_2026, zionist_legitimacy_basis_flat_control, theater_ratio, 146, 0.35).

% Extraction over time
narrative_ontology:measurement(zion_leg_extract_1880, zionist_legitimacy_basis_flat_control, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(zion_leg_extract_1900, zionist_legitimacy_basis_flat_control, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(zion_leg_extract_1920, zionist_legitimacy_basis_flat_control, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(zion_leg_extract_1947, zionist_legitimacy_basis_flat_control, base_extractiveness, 67, 0.75).
narrative_ontology:measurement(zion_leg_extract_1968, zionist_legitimacy_basis_flat_control, base_extractiveness, 88, 0.78).
narrative_ontology:measurement(zion_leg_extract_2000, zionist_legitimacy_basis_flat_control, base_extractiveness, 120, 0.78).
narrative_ontology:measurement(zion_leg_extract_2026, zionist_legitimacy_basis_flat_control, base_extractiveness, 146, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(zion_leg_suppress_1880, zionist_legitimacy_basis_flat_control, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(zion_leg_suppress_1900, zionist_legitimacy_basis_flat_control, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(zion_leg_suppress_1920, zionist_legitimacy_basis_flat_control, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(zion_leg_suppress_1947, zionist_legitimacy_basis_flat_control, suppression_requirement, 67, 0.7).
narrative_ontology:measurement(zion_leg_suppress_1968, zionist_legitimacy_basis_flat_control, suppression_requirement, 88, 0.82).
narrative_ontology:measurement(zion_leg_suppress_2000, zionist_legitimacy_basis_flat_control, suppression_requirement, 120, 0.82).
narrative_ontology:measurement(zion_leg_suppress_2026, zionist_legitimacy_basis_flat_control, suppression_requirement, 146, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis_flat_control, identity_coordination).

% DUAL FORMULATION NOTE:
% This is the flat construction of the Zionist historical right legitimacy basis. The constraint could be decomposed into multiple readings (religious Zionist covenant reading, secular historical presence reading, cultural Zionism diaspora longing reading, political Zionism self-determination reading), each with different beneficiary structures and extractiveness profiles. However, this flat construction treats the historical right claim as a single constraint with perspectival disagreement captured through the (P,T,E,S) tuple rather than through reading decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
