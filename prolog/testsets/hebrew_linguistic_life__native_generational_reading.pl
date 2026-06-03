% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__native_generational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__native_generational_reading, []).

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
 *   constraint_id: hebrew_linguistic_life__native_generational_reading
 *   human_readable: Hebrew Linguistic Life: Native Generational Acquisition Reading
 *   domain: sociolinguistics/religious_studies/nationalism
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'hebrew_linguistic_life' — specifically, the native_generational_reading.
 *   The kernel is a stabilized yet ambiguous commitment that grounds
 *   legitimacy claims about what it means for a language to be 'alive.' Three
 *   different readings coexist: (1) liturgical_preservation_reading: a
 *   language is alive through unbroken sacred transmission regardless of
 *   vernacular use; (2) marketplace_pidgin_reading: a language is alive when
 *   it functions as inter-communal medium for practical coordination; (3)
 *   native_generational_reading (THIS CONSTRAINT): a language is alive ONLY
 *   when children acquire it as mother tongue AND use it for all daily
 *   functions including secular mundane speech. Each reading operationalizes
 *   the kernel differently and produces different victim and beneficiary
 *   sets. This story generates ONLY the native_generational_reading as a
 *   structurally complete constraint, without averaging across readings or
 *   hedging the ε value. The sibling readings are other constraints (other
 *   JSON files); they are not folded into this one. The structural delta for
 *   this reading: Hebrew was linguistically dead during the 70-1880 CE
 *   dormancy period (no native child acquisition); revival from dormancy
 *   required generational transmission building; victim set includes Yiddish
 *   and Ladino speakers whose mother-tongue transmission to children was
 *   suppressed in favor of Hebrew monolingualism.
 *
 * KEY AGENTS:
 *   - Hebrew Revivalists and Israeli State Apparatus: Primary beneficiary (institutional/arbitrage) — benefits from unifying diaspora Jews through shared native language; can arbitrage alternatives while enforcing monolingual normativity on others; experiences constraint as pure coordination
 *   - Yiddish/Ladino Speakers: Primary victim (powerless/trapped) — mother-tongue transmission suppressed; exit requires abandoning native language identity; bears maximum extraction cost; linguistically 'dead' under this reading's framework
 *   - Jewish Diaspora Communities: Secondary victim (organized/constrained) — face resource constraints and external pressure toward linguistic shift; experience mixed coordination benefits (national cohesion) and extraction (diaspora linguistic diversity loss); some agency to resist but significant barriers
 *   - Language Rights Movements: Organized resistance (organized/constrained) — view native-generational criterion as temporary configuration; declare alternative legitimacy frameworks; see sunset pathway through international language vitality protocols; lower theater because they operate with explicit counterframeworks
 *   - Linguistic Purism Doctrine as Institutional Practice: Piton-like institution — normative claim persists through inertia and performative maintenance despite widespread code-switching and multilingual practice in actual Israeli society; detached from original unification function
 *   - Analytical Observer: Civilizational stance (analytical/analytical) — risks naturalizing the reading as linguistic law of nature; reveals the reading as a constructed kernel interpretation with identifiable beneficiaries and victims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.58).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.65).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Hebrew Linguistic Life: Native Generational Acquisition Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistics/religious_studies/nationalism").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, '01388971-402b-4a23-988e-8e2cb0bce235').
narrative_ontology:cs_kernel_codification('01388971-402b-4a23-988e-8e2cb0bce235', fixed_text).
narrative_ontology:cs_authority_grounding('01388971-402b-4a23-988e-8e2cb0bce235', lineage).
narrative_ontology:cs_interpretation_layer_present('01388971-402b-4a23-988e-8e2cb0bce235').
narrative_ontology:cs_reading_relation('01388971-402b-4a23-988e-8e2cb0bce235', hebrew_linguistic_life__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('01388971-402b-4a23-988e-8e2cb0bce235', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('01388971-402b-4a23-988e-8e2cb0bce235', foundational, native_generational_transmission_necessary).
narrative_ontology:cs_axiom_status(native_generational_transmission_necessary, holdable).
narrative_ontology:cs_axiom_grounding('01388971-402b-4a23-988e-8e2cb0bce235', native_generational_transmission_necessary, deontological).
narrative_ontology:cs_axiom('01388971-402b-4a23-988e-8e2cb0bce235', foundational, secular_daily_use_requirement).
narrative_ontology:cs_axiom_status(secular_daily_use_requirement, holdable).
narrative_ontology:cs_axiom_grounding('01388971-402b-4a23-988e-8e2cb0bce235', secular_daily_use_requirement, deontological).
narrative_ontology:cs_reference_frame('01388971-402b-4a23-988e-8e2cb0bce235', dormant_hebrew_1880_ce).
narrative_ontology:cs_drift_state('01388971-402b-4a23-988e-8e2cb0bce235', contemporary_israeli_multilingualism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('01388971-402b-4a23-988e-8e2cb0bce235', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_revivalists).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, jewish_nationalism).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, secular_israeli_state).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, diaspora_linguistic_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YIDDISH/LADINO SPEAKER (SNARE) — Structurally trapped. Mother-tongue transmission to children is suppressed by education policy, social pressure, and cultural delegitimization. Exit from the constraint requires abandoning native language identity. The speaker cannot maintain their native language in daily secular functions within the reading's normative framework — only Hebrew qualifies as 'alive,' rendering their mother tongue simultaneously 'dead.' Maximum experienced extraction: identity loss, cultural severing, intergenerational language death.
constraint_indexing:constraint_classification(hebrew_linguistic_life__native_generational_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: JEWISH DIASPORA COMMUNITIES (TANGLED ROPE) — Organized groups with some agency but facing resource constraints and external pressure. The constraint benefits them through national cohesion and shared linguistic identity (coordination function) while simultaneously extracting the loss of mother-tongue transmission networks and diaspora cultural diversity (asymmetric extraction). Enforcement requires institutional pressure: education mandates, cultural shaming, resource concentration on Hebrew instruction. Significant constraint but not maximal — some diaspora communities resist, negotiate, or maintain parallel linguistic practices.
constraint_indexing:constraint_classification(hebrew_linguistic_life__native_generational_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HEBREW REVIVALISTS / ISRAELI STATE (ROPE) — Primary beneficiary (institutional power, arbitrage exit). The constraint solves a genuine coordination problem: unifying Jews from diverse linguistic backgrounds into a single polity requires shared language. The reading's framework legitimizes this coordination as natural ('a language is alive only when...') and necessary. The beneficiary experiences this as pure coordination with no extraction cost — the framework renders alternative readings as linguistically 'dead' and thus illegitimate. Net flow: extraction runs toward this agent; they have exit capacity and can arbitrage alternatives (maintain multiple languages, pivot to English) while enforcing monolingual normativity on others.
constraint_indexing:constraint_classification(hebrew_linguistic_life__native_generational_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LINGUISTIC PURISM DOCTRINE (PITON) — The normative claim ('a language is alive only when children acquire it as mother tongue') is substantially performative. Actual Hebrew language practice in Israel includes code-switching, English penetration in tech sectors, Russian/Arabic multilingualism, and functional diglossia (formal Hebrew vs. colloquial Israeli Hebrew). The purism doctrine persists through institutional inertia and ideological maintenance despite widespread practice that contradicts it. Theater ratio high because the doctrine functions more as national identity narrative than as descriptive linguistic classification. The doctrine has become detached from its original function (unifying diaspora Jews) and is maintained primarily through education and media performance.
constraint_indexing:constraint_classification(hebrew_linguistic_life__native_generational_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal analytical stance, the claim appears as a natural law of linguistics: 'a language cannot remain alive without native speaker transmission.' This perspective naturalizes the reading as an inherent property of linguistic systems — mother-tongue acquisition is presented as the only true measure of linguistic vitality. However, the structural data (identifiable beneficiaries, active suppression of alternatives, extracted victims, institutional enforcement) contradicts the mountain classification. The engine's false summit detector will flag this as naturalization of a contestable normative claim. The 'natural law' framing conceals the constructed kernel and the reading's beneficiary structure.
constraint_indexing:constraint_classification(hebrew_linguistic_life__native_generational_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: LANGUAGE RIGHTS MOVEMENTS (SCAFFOLD) — Organized agents (Yiddish cultural organizations, Ladino preservation societies, indigenous language movements, UNESCO language vitality protocols) view the native-generational reading as a temporary institutional configuration with a sunset clause. Their framework declares that linguistic vitality is plural and multivalent — liturgical transmission, marketplace functionality, and native-speaker acquisition are ALL valid measures of language life. As language rights norms mature globally and indigenous/minority language movements gain institutional recognition, the exclusive native-generational criterion loses legitimacy. The scaffold classification reflects that this organized resistance has agency and visible exit pathways (alternative linguistic legitimacy frameworks), but faces resource constraints and institutional barriers (education policy, media concentration). Theater relatively low because language rights movements operate with explicit counterframeworks rather than performative ritual.
constraint_indexing:constraint_classification(hebrew_linguistic_life__native_generational_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__native_generational_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hebrew_linguistic_life__native_generational_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hebrew_linguistic_life__native_generational_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, TR),
    TR >= 0.70.

:- end_tests(hebrew_linguistic_life__native_generational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading's framework legitimizes extraction of diaspora linguistic diversity in service of unifying Jews under Hebrew. The beneficiary (Israeli state, Hebrew revivalists) captures gains from unified language policy; the victim (Yiddish/Ladino speakers) loses intergenerational transmission networks and cultural continuity. The value reflects genuine asymmetry: the suppression is real and institutional, but not absolute — some diaspora communities maintain parallel transmission practices, and the reading does solve a coordination problem (unifying disparate Jewish communities linguistically). Suppression (0.65): High and rising. Early suppression (t=0, value 0.35) reflects pre-state period with limited enforcement capacity. Suppression increases through t=20 (Israeli statehood, education policy mandates, media concentration, economic incentives toward Hebrew). Education policy makes Yiddish/Ladino education marginal; social prestige flows to Hebrew speakers; employment and housing advantages accrue to monolingual Hebrew competence. Theater ratio (0.48): Moderate and increasing. The native-generational criterion does capture something real about language transmission (mother-tongue acquisition is genuine functional mechanism), so theater is not maximal. But the criterion's application is increasingly performative: actual Israeli Hebrew includes code-switching, English in tech sectors, Russian/Arabic multilingualism, and functional diglossia between formal and colloquial registers. Theater rises as the reading's descriptive accuracy decreases while institutional enforcement increases.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence rooted in the reading's beneficiary/victim structure. The beneficiary (Israeli state, Hebrew revivalists) with arbitrage exit options sees the constraint as pure coordination (Rope) — solving the genuine problem of unifying Jews from diverse linguistic backgrounds. The victim (Yiddish/Ladino speakers) with trapped exit sees extraction (Snare) — identity loss, intergenerational cultural severing, mandatory assimilation. The diaspora communities with constrained exit see a hybrid (Tangled Rope) — they gain unification benefits while losing linguistic diversity. The language rights movements with organized resistance see a temporary configuration (Scaffold) — alternative legitimacy frameworks are emerging globally; the native-generational criterion will lose institutional monopoly. The institutional purism doctrine sees its own degradation (Piton) — the normative claim persists through inertia despite actual linguistic practice contradicting it. The analytical observer risks mistaking this constructed interpretation as a natural law (Mountain/false summit) — presenting the reading as an inherent property of how languages 'truly' stay alive. The perspectival gaps reveal that all disagreement about Hebrew's linguistic life are rooted in whether the observer is a beneficiary, a victim, or attempting to naturalize one side's reading as universal law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position: power level, exit capacity, and relationship to the reading's extraction flow. Beneficiaries with institutional power and arbitrage exit (Israeli state) experience low/negative effective extraction — the reading legitimizes their position and provides exit flexibility. Trapped victims with powerless status (Yiddish speakers) experience maximum extraction — they have no exit path and bear full cost of the reading's enforcement. Organized agents with constrained exit (diaspora communities) experience moderate extraction — they have some agency and some resistance capacity, but face significant barriers. The piton perspective derives from theater gates rather than from high experienced extraction. The mountain perspective at civilizational scale risks false summit status because of the identified beneficiary structure and active suppression of alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandatrophy is rooted in the kernel contest itself. The reading claims linguistic life requires native generational transmission, but this claim cannot be intrinsic to language as a system — it is a normative interpretation of the kernel that benefits specific agents (Israeli state, Hebrew revivalists) and harms others (diaspora linguistic diversity). The false summit detector flags the mountain perspective as naturalization. The tangled_rope classification reflects the genuine tension: the reading solves a coordination problem (unifying Jews across diaspora) while simultaneously extracting diaspora linguistic diversity and suppressing mother-tongue transmission of Yiddish/Ladino. The mandatrophy resolves by acknowledging that all six perspectives are legitimate readings of the same structural situation, rooted in different positions relative to the reading's beneficiary/victim structure. The reading is neither a natural law (mountain) nor pure extraction (snare) — it is a constructed kernel interpretation that entangles coordination (unification) with extraction (diversity suppression). The resolution requires recognizing the kernel's plurality: multiple readings of linguistic life coexist; no single reading is universal law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linguistic_dormancy_definition,
    'Was Hebrew genuinely ''dead'' (no native speakers, no daily vernacular use) during the 70-1880 CE period, or was it a specialized/liturgical language with native-speaker lineages among scholars and certain communities?',
    'Philological and sociolinguistic analysis of medieval Hebrew texts, manuscript evidence of native-speaker competence in reading, writing, and composition during dormancy period; identification of communities that may have maintained Hebrew as a secondary first language alongside Aramaic/Arabic',
    'If genuinely dead (no native acquisition): the reading''s core premise (life requires native generational transmission) is historically validated; dormancy period is real break. If specialized/scholarly native competence persisted: the reading retroactively reclassifies historical Hebrew speakers as ''dead'' despite active transmission, which reveals the reading''s normative rather than descriptive status. Classification may shift from false summit to contingent kernel interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(linguistic_dormancy_definition, empirical, 'Whether Hebrew had native speakers during the dormancy period').

omega_variable(
    coercive_suppression_mechanism,
    'To what degree was Yiddish/Ladino speaker abandonment driven by education policy enforcement versus by economic incentives and voluntary language shift toward Hebrew/majority languages?',
    'Historical analysis of education policy mandates, employment discrimination by language, social prestige dynamics, cost-benefit analysis for families choosing linguistic transmission strategy; comparison with voluntary language shift in contexts without institutional coercion (e.g., immigrant communities in diaspora without Hebrew education mandates)',
    'If suppression is primarily institutional (policy enforcement): the snare and tangled_rope perspectives are confirmed; extraction is high and coercive. If primarily economic/voluntary: the constraint classification shifts; it becomes a coordination mechanism (rope-like) with incentive structure rather than coercive suppression. Beneficiary/victim relationship becomes ambiguous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercive_suppression_mechanism, empirical, 'Degree of institutional coercion in language-shift enforcement').

omega_variable(
    sibling_reading_foreclosure,
    'Does the native-generational reading logically foreclose the liturgical-preservation reading within a single coherent framework, or can a language simultaneously be ''alive'' (generationally transmitted with secular daily use) in one context while also being ''alive'' (liturgically preserved in unbroken transmission chain) in another?',
    'Logical analysis of the readings'' core premises. Native-generational criterion: ''alive only when children acquire as mother tongue AND use for all daily functions including secular mundane speech.'' Liturgical preservation criterion: ''alive when sacred texts continuously recited/studied/transmitted regardless of vernacular use.'' If both criteria can be satisfied simultaneously (Hebrew is generationally transmitted for secular daily use in Israel AND liturgically preserved in diaspora yeshivas), the readings coexist. If the readings claim mutually exclusive definitions of ''alive,'' they foreclose each other.',
    'If foreclose relationship: only one reading''s normative framework can govern linguistic legitimacy assessment; the other reading is logically ruled out. Constraint structure simplifies to binary competition. If coexist relationship: multiple definitions of linguistic life are simultaneously operative in different communities; constraint enables plural legitimacy. This affects how the kernel itself is understood — contested definition vs. binary exclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between native-generational and liturgical-preservation definitions of linguistic life').

omega_variable(
    marketplace_pidgin_functionality,
    'Does the marketplace-pidgin reading describe a genuine third measuring criterion (inter-communal coordination without native-speaker requirement), or is it logically subsumed within the native-generational reading''s scope (pidgin eventually requires native acquisition to survive)?',
    'Analysis of historical cases: Arabic as inter-communal trade language without native generational transmission; English as global lingua franca with non-native dominant use; Tok Pisin, Swahili, and other contact languages that persist without native-speaker replacement. If such languages maintain functionality without native speaker generation, the marketplace reading is independent. If they eventually require native acquisition to avoid attrition, the native-generational reading subsumes and forecloses the marketplace reading.',
    'If marketplace reading is independent: the kernel has three genuinely distinct criteria; Hebrew''s status differs depending on measurement. If native-generational forecloses marketplace: the reading hierarchy simplifies; native acquisition is the ultimate requirement, making the marketplace criterion temporary or provisional. Affects constraint family structure and foreclosure relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marketplace_pidgin_functionality, empirical, 'Whether marketplace-pidgin criterion is functionally independent from native-generational acquisition').

omega_variable(
    axiom_status_empirical_shift,
    'Has the foundational axiom ''native generational transmission is the only valid measure of linguistic life'' been formally overridden within the reading''s own tradition (Israeli linguistic academia, language policy discourse), or does it remain holdable as a normative position?',
    'Survey of Israeli linguistic scholarship, language education policy documents, UNESCO/international linguistic standards, and Israeli public discourse from 1950s-present. Identify explicit rejections of the native-generational criterion by authorities within the reading''s own tradition (e.g., Israeli linguists, education ministry, cultural institutions) or formal adoption of alternative criteria.',
    'If axiom is overridden: the reading itself has abandoned its foundation; constraint classification may degrade to piton or scaffold as the axiom loses institutional authority. If axiom remains holdable: the reading maintains internal coherence and can sustain its mountain (false summit) or tangled_rope classification. This determines whether the constraint is still actively sustained by its beneficiary group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_status_empirical_shift, empirical, 'Authority status of native-generational transmission axiom within contemporary Hebrew language discourse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_ng_tr_t0, hebrew_linguistic_life__native_generational_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hebrew_ng_tr_t10, hebrew_linguistic_life__native_generational_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(hebrew_ng_tr_t20, hebrew_linguistic_life__native_generational_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(hebrew_ng_be_t0, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(hebrew_ng_be_t10, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(hebrew_ng_be_t20, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_ng_su_t0, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hebrew_ng_su_t10, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(hebrew_ng_su_t20, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% The constraint 'hebrew_linguistic_life' decomposes into three structurally distinct readings with different ε values, beneficiary/victim structures, and kernel interpretations. This file instantiates ONLY the native_generational_reading. The sibling readings (liturgical_preservation_reading, marketplace_pidgin_reading) are separate constraint stories with their own ε, their own perspectives, and their own classifications. All three stories are linked via network.affects_constraints because they compete for authority over a single contested kernel. The native-generational reading influences the other readings by claiming exclusivity: it argues that Hebrew's linguistic life ONLY through native acquisition, which forecloses or downgrades the other readings. The kernel contest is the structural relationship between all three stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
