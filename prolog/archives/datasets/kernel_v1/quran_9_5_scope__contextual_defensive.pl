% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__contextual_defensive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__contextual_defensive, []).

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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Qur'an 9:5 Contextual-Defensive Reading: Scope-Limited Authorization
 *   domain: islamic_jurisprudence/quranic_hermeneutics/political_theology
 *
 * SUMMARY:
 *   Qur'an 9:5 is one of the most contentious verses in Islamic scripture,
 *   invoked across a spectrum of hermeneutical readings with radically
 *   different implications for Muslim-non-Muslim relations and the legitimacy
 *   of violence. The contextual-defensive reading interprets 9:5 as a
 *   historically bounded authorization: it addresses specific 7th-century
 *   Medinan polytheist tribes who had violated treaties with the early Muslim
 *   polity, does not abrogate verses affirming religious freedom (2:256,
 *   49:13), and establishes defensive warfare and treaty enforcement as the
 *   only legitimate scope. This reading situates 9:5 within its textual and
 *   historical context (preceded by 9:1-3 specifying treaty violations,
 *   followed by 9:4 reaffirming treaty obligations to non-violating
 *   polytheists, and distinct from 9:29's framework for People of the Book).
 *   The constraint is ONE READING of the contested kernel 'quran_9_5_scope'.
 *   Three readings coexist: (1) contextual-defensive (this file), limiting
 *   scope to treaty violators and defensive necessity; (2)
 *   abrogating-universal, treating 9:5 as abrogating peaceful verses and
 *   authorizing conversion by force of all polytheists; (3)
 *   progressive-synthesis, treating both readings as valid stages of
 *   revelation reflecting community maturation. The contextual-defensive
 *   reading benefits integrationist Muslim-majority states and pluralist
 *   Muslim movements seeking to ground peaceful coexistence in scriptural
 *   authority. It constrains actors claiming scriptural justification for
 *   non-defensive violence while maintaining genuine coordination benefits
 *   (legitimate defense framework). The constraint's extractiveness is
 *   moderate because the reading simultaneously provides and restricts: it
 *   authorizes defense (benefits those under threat) while restricting
 *   universal application (costs those seeking expansionist rhetoric).
 *   Theater ratio is moderate because the reading requires hermeneutical
 *   sophistication to distinguish from universalizing readings; in contexts
 *   where hermeneutical authority is contested, the theater rises (different
 *   audiences accept different readings). The analytical observer perceives
 *   the historical-textual bedrock as immutable (mountain), but this
 *   naturalization of 'context matters' is itself part of the argument — the
 *   reading's success depends on establishing context as binding rather than
 *   optional.
 *
 * KEY AGENTS:
 *   - Integrationist Islamic jurisprudential tradition (organized/constrained) — benefits from scriptural framework legitimizing pluralism; constrained by pressure from universalizing readings within Muslim discourse
 *   - Muslim-majority states with pluralist commitments (institutional/constrained) — benefit from rhetorical framework supporting coexistence; constrained by domestic constituencies and international alignments
 *   - Interfaith peacebuilding organizations (institutional/arbitrage) — benefits from reduced communal tension; arbitrage exit available (refocus on other traditions)
 *   - Religious minorities in conflict zones (powerless/trapped) — bear existential risk regardless of which reading prevails in their region; no exit option except displacement
 *   - Western academic Orientalism (institutional/mobile) — maintains performative scholarly neutrality while gatekeeping recognition of contextual reading; mobile exit available but institutionally costly
 *   - Classical Islamic jurisprudence tradition (analytical/analytical) — provides textual-historical bedrock; immutable feature of the constraint's structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.28).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.35).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.28).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Qur'an 9:5 Contextual-Defensive Reading: Scope-Limited Authorization").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "islamic_jurisprudence/quranic_hermeneutics/political_theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, '8d88d326-3887-4c5f-a5a5-e95e23162ceb').
narrative_ontology:cs_kernel_codification('8d88d326-3887-4c5f-a5a5-e95e23162ceb', fixed_text).
narrative_ontology:cs_authority_grounding('8d88d326-3887-4c5f-a5a5-e95e23162ceb', lineage).
narrative_ontology:cs_interpretation_layer_present('8d88d326-3887-4c5f-a5a5-e95e23162ceb').
narrative_ontology:cs_reading_relation('8d88d326-3887-4c5f-a5a5-e95e23162ceb', quran_9_5_scope__abrogating_universal, coexists_with).
narrative_ontology:cs_reading_relation('8d88d326-3887-4c5f-a5a5-e95e23162ceb', quran_9_5_scope__progressive_synthesis, coexists_with).
narrative_ontology:cs_axiom('8d88d326-3887-4c5f-a5a5-e95e23162ceb', foundational, contextual_limitation_binding).
narrative_ontology:cs_axiom_status(contextual_limitation_binding, holdable).
narrative_ontology:cs_axiom_grounding('8d88d326-3887-4c5f-a5a5-e95e23162ceb', contextual_limitation_binding, empirically_contingent).
narrative_ontology:cs_axiom('8d88d326-3887-4c5f-a5a5-e95e23162ceb', foundational, abrogation_not_operative_on_contingent_authorization).
narrative_ontology:cs_axiom_status(abrogation_not_operative_on_contingent_authorization, holdable).
narrative_ontology:cs_axiom_grounding('8d88d326-3887-4c5f-a5a5-e95e23162ceb', abrogation_not_operative_on_contingent_authorization, deontological).
narrative_ontology:cs_reference_frame('8d88d326-3887-4c5f-a5a5-e95e23162ceb', coexistence_framework_scriptural_legitimacy).
narrative_ontology:cs_drift_state('8d88d326-3887-4c5f-a5a5-e95e23162ceb', contemporary_political_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8d88d326-3887-4c5f-a5a5-e95e23162ceb', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, coexistence_framework_proponents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTEGRATIONIST TRADITION (ROPE) — Organized scholarly consensus (Al-Azhar, AQSA, international Islamic law associations) sees 9:5 as a coordination mechanism grounding legitimate self-defense and treaty enforcement within a bounded ethical framework. The constraint coordinates defense obligations with scriptural restraint: treaty obligations take precedence, and warfare is authorized only against violators, not universally. Constrained by political pressure from universalizing readings but maintains scholarly coherence.
constraint_indexing:constraint_classification(quran_9_5_scope__contextual_defensive, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: HISTORICAL-TEXTUAL BEDROCK (MOUNTAIN) — From a civilizational analytical view, the Medinan context is an irreducible structural feature: Qur'an 9:5 is embedded in a specific textual sequence (9:4 reaffirms treaty obligations; 9:1-3 specifies prior treaty-breaking by polytheist tribes; 9:29 addresses a different category, People of the Book with distinct rules). The historical nexus is immutable — the verse cannot be read as ahistorical without denying its own internal textual architecture. This creates a binding analytical constraint on legitimate interpretation.
constraint_indexing:constraint_classification(quran_9_5_scope__contextual_defensive, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: MUSLIM-MAJORITY STATE PRACTITIONERS (TANGLED ROPE) — States with large Muslim populations navigate competing pressures: domestic constituencies invoking 9:5 for expansionist rhetoric, international legal obligations requiring restraint, geopolitical alignment with pluralist norms. The contextual-defensive reading provides coordination benefit (legitimizes defense within rule-of-law framework) alongside extraction cost (constrains political actors from invoking scriptural authority for non-defensive action). Moderate power with constrained exit — abandoning the coordination framework risks both domestic legitimacy and international standing.
constraint_indexing:constraint_classification(quran_9_5_scope__contextual_defensive, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: RELIGIOUS MINORITIES IN CONFLICT ZONES (SNARE) — From the position of non-Muslim minorities in regions where 9:5 is invoked, the constraint appears as pure extraction regardless of the contextual reading's internal logic. The distinction between contextual-defensive and universalizing readings is epistemically inaccessible to those lacking hermeneutical authority; the constraint's performative effect (whether action is justified or not) depends on which reading prevails in the specific regime. Trapped by geographic location and religious identity; no exit option except displacement.
constraint_indexing:constraint_classification(quran_9_5_scope__contextual_defensive, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: WESTERN ACADEMIC ORIENTALISM (PITON) — Academic institutions in Western contexts often maintain a performative commitment to 'objective study of Islamic texts' while the institutional practice is substantially theatrical — selective citation of verses without hermeneutical context, refusal to engage the contextual-defensive reading as epistemically valid, reduction of 1400 years of jurisprudential development to a single verse. The theater persists through institutional inertia and disciplinary gatekeeping despite low functional understanding. Mobile exit for individual scholars is available but institutionally costly.
constraint_indexing:constraint_classification(quran_9_5_scope__contextual_defensive, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERFAITH PEACEBUILDING (ROPE) — International interfaith organizations and pluralist Muslim-led NGOs experience the contextual-defensive reading as a pure coordination mechanism: it provides a scriptural framework for Muslim leaders to unambiguously reject expansionist violence while maintaining spiritual authority within their communities. The reading solves a collective action problem — how to coordinate pluralist norms across Muslim-majority and secular-majority societies — with minimal coercive overhead. Arbitrage exit available (refocus on other traditions); beneficiary position (benefits from reduced communal tension).
constraint_indexing:constraint_classification(quran_9_5_scope__contextual_defensive, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__contextual_defensive_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quran_9_5_scope__contextual_defensive, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quran_9_5_scope__contextual_defensive, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, TR),
    TR >= 0.70.

:- end_tests(quran_9_5_scope__contextual_defensive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The contextual-defensive reading provides genuine coordination benefits (legitimizes defense, constrains abuse) alongside modest extraction (restricts universalizing rhetoric, requires hermeneutical authority). The reading does not concentrate extraction on powerless agents — its primary effect is to coordinate plural communities. Suppression (0.35): Moderate. Barriers to the reading's adoption include institutional gatekeeping in Western academia, political incentives for universalizing interpretations in some regimes, and lower visibility compared to more inflammatory readings. However, suppression is not high because the reading has active scholarly advocates and institutional support in Islamic jurisprudential institutions. Theater ratio (0.42): Moderate. The contextual-defensive reading does substantive interpretive work (analyzing historical context, comparing textual sequences, engaging abrogation doctrine), but its adoption in political discourse often becomes theatrical — cited as cover for moderation without the hermeneutical substance being engaged. The theater ratio reflects the gap between technical jurisprudential argument and its public circulation.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the integrationist reading (Rope) and the universalizing reading (absent from this constraint but structurally implied) is NOT merely a difference of opinion but a difference in what the constraint DOES. From the integrationist perspective, the constraint coordinates defense with restraint. From the universalizing perspective, the constraint authorizes conversion by force. These are not two interpretations of the same thing — they instantiate two different constraints with different epsilon values, different victim sets, and different beneficiaries. The contextual-defensive reading limits victims to treaty-violators; the abrogating reading includes all non-Muslims. The tension is not hermeneutical but structural: these readings produce genuinely different constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies substantially across perspectives because agents have fundamentally different relationships to the constraint. Integrationist scholars benefit from the reading (beneficiaries with constrained but navigable options → d ≈ 0.30). Muslim-majority states experience asymmetric benefit/cost (moderate power + constrained exit + mixed beneficiary-victim status → d ≈ 0.52). Religious minorities are pure targets (powerless + trapped → d ≈ 0.92). Western academic institutions benefit from the appearance of expertise without substantive engagement (institutional power + mobile exit + beneficiary status → d ≈ 0.18). Interfaith organizations benefit from reduced tension (institutional power + arbitrage exit + beneficiary status → d ≈ 0.10). The derived directionality differences explain why the same constraint classifies as Rope, Tangled Rope, Snare, Piton, and Mountain across perspectives — the constraint's effect on extractiveness is not uniform across the observation space.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that a single verse can instantiate multiple constraints depending on which reading is adopted. The 'which reading is correct?' question is not answerable by examining the verse alone — it requires examining the reading's coherence within Islamic jurisprudential tradition, its empirical consequences, and its relationship to other scriptural commitments. The contextual-defensive reading resolves the mandatrophy at the level where it operates (integrationist Islamic institutions) by articulating a coherent coordination mechanism. At the level of religious minorities in conflict zones, the mandatrophy cannot be resolved by hermeneutical argument alone — it is resolved by institutional power (which reading's authority prevails in the regime). The constraint story demonstrates that mandatrophy resolution is perspectival: it resolves at one level and remains in force at another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_authenticity,
    'Is the contextual-defensive reading a post-hoc rationalization constructed to accommodate modern pluralist norms, or does it represent a continuous scholarly tradition with pre-modern roots?',
    'Genealogical analysis of medieval tafsir (Tabari, Ibn Kathir, Al-Qurtubi) on 9:5; systematic documentation of the continuity or discontinuity of the contextual-defensive framework from classical through modern jurisprudence; identification of the historical moment(s) when abrogating readings gained ascendancy',
    'If pre-modern continuity: contextual-defensive reading gains epistemic authority and is not merely accommodationist. If post-hoc: the reading is theoretically coherent but lacks historical grounding, shifting classification toward piton (performative maintenance) or scaffold (temporary coordination mechanism). This directly affects whether the reading ''forecloses'' or merely ''coexists_with'' the abrogating reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_authenticity, empirical, 'Whether contextual-defensive reading has continuous scholarly tradition or post-hoc construction').

omega_variable(
    treaty_violation_threshold,
    'What constitutes sufficient prior treaty violation to activate the authorization in 9:5 under this reading? Is the standard objective (documented breach) or interpretive (perceived breach)?',
    'Analysis of 7th-century polytheist tribe conduct relative to documented treaty terms; examination of how medieval jurisprudence distinguished types of violation; application of the threshold to contemporary state conflicts',
    'If objective threshold: the reading maintains tight constraint on scope (extraction low, classification remains Rope). If interpretive: the reading''s restraint is illusory (extraction rises, classification trends toward Tangled Rope or Snare). This is the highest-leverage parameter for determining whether the contextual-defensive reading actually constrains or merely rhetorically decorates expansionist action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_violation_threshold, empirical, 'Definition of sufficient prior treaty violation under contextual-defensive reading').

omega_variable(
    abrogation_doctrine_status,
    'Does the contextual-defensive reading commit to the classical Islamic legal principle of abrogation (naskh), and if so, does accepting abrogation doctrine logically require accepting that 9:5 abrogates peaceful verses?',
    'Systematic comparison of how contextual-defensive scholars engage abrogation doctrine; examination of whether they accept naskh as valid principle but contest its application to 9:5, or whether they reject abrogation doctrine entirely; analysis of the logical consistency of these positions',
    'If abrogation doctrine is binding and applicable: the contextual-defensive reading must explain why 9:5 does NOT abrogate 2:256 (''no compulsion in religion'') and 49:13 (affirming human diversity). Failure to do so suggests the reading is incoherent within Islamic jurisprudential tradition. If abrogation is rejected: the reading gains coherence but diverges from classical jurisprudence, potentially limiting its authority. This determines whether the reading''s relationship to the abrogating_universal reading is ''forecloses'' (one must be abandoned) or ''coexists_with'' (both live).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abrogation_doctrine_status, conceptual, 'Whether abrogation doctrine logically entails or precludes contextual-defensive reading').

omega_variable(
    audience_hermeneutical_capacity,
    'What is the distribution of hermeneutical literacy (capacity to distinguish context-limited from universal readings) among actors who invoke 9:5 in political contexts?',
    'Qualitative analysis of political speech, propaganda, and judicial opinions invoking 9:5; coding for presence/absence of hermeneutical argument; comparison to technical jurisprudential literature; assessment of whether hermeneutical sophistication correlates with political regime type or stated objectives',
    'If hermeneutical literacy is high: the distinction between readings matters politically (extraction depends on which reading prevails). If literacy is low: the distinction is epistemically invisible to audiences (the constraint''s performative effect is independent of the reading''s actual content), and the real extraction mechanism is the reading''s use as rhetorical cover regardless of substantive argument. This affects whether suppression reflects epistemic barriers (interpretation unavailable) or structural barriers (interpretation available but suppressed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audience_hermeneutical_capacity, empirical, 'Distribution of hermeneutical literacy among actors invoking 9:5').

omega_variable(
    sibling_reading_foreclosure,
    'Does the contextual-defensive reading logically foreclose the abrogating_universal reading, or do both readings remain coherent within different theological frameworks that could coexist in pluralist Islamic thought?',
    'Systematic logical analysis: Does accepting ''9:5 is context-limited'' entail ''abrogation doctrine is invalid'' or ''abrogation cannot apply to 9:5''? If yes, foreclosure is claimed. If the reading only asserts ''we reject the abrogating conclusion'' without denying the logical machinery, the readings coexist. Cross-reference with theological traditions that do hold abrogation as valid but contest its application (e.g., some classical Shafi''i schools).',
    'If foreclosure: the reading_relations entry should use ''forecloses''. This signals that a single Islamic jurisprudential framework cannot coherently hold both readings, making the constraint''s resolution a matter of which reading is adopted at civilizational scale. If coexistence: use ''coexists_with'', signaling that different Islamic communities and scholars can hold different readings without logical contradiction, and the constraint''s effect depends on which reading''s institutional authority prevails in specific contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between contextual-defensive and abrogating_universal readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(q95_ctx_theater_early, quran_9_5_scope__contextual_defensive, theater_ratio, 0, 0.35).
narrative_ontology:measurement(q95_ctx_theater_mid, quran_9_5_scope__contextual_defensive, theater_ratio, 5, 0.39).
narrative_ontology:measurement(q95_ctx_theater_late, quran_9_5_scope__contextual_defensive, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(q95_ctx_extr_early, quran_9_5_scope__contextual_defensive, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(q95_ctx_extr_mid, quran_9_5_scope__contextual_defensive, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(q95_ctx_extr_late, quran_9_5_scope__contextual_defensive, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__progressive_synthesis).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, muslim_non_muslim_coexistence_authorization).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, scriptural_authority_pluralism_legitimacy).

% DUAL FORMULATION NOTE:
% The contextual-defensive reading is ONE of three structurally distinct constraints arising from the same verse. Each reading specifies a different victim set, different beneficiaries, and different epsilon values. The three stories form a constraint family linked by the kernel 'quran_9_5_scope'. Decomposition is required because the three readings differ radically in epistemic status (pre-modern continuity vs post-hoc rationalization vs stage theory), victim set (treaty-violators only vs all polytheists vs historic communities), and extractiveness (moderate vs high vs moderate). The epsilon values are determined by the reading itself, not by external observables — ε-invariance is preserved within each reading, and the readings themselves are distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_9_5_scope__contextual_defensive, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
