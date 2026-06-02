% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__contextual_egalitarian
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__contextual_egalitarian, []).

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
 *   constraint_id: quranic_gender_verses__contextual_egalitarian
 *   human_readable: Qur'anic Gender Verses: Contextual-Egalitarian Reading
 *   domain: islamic_jurisprudence/legal_hermeneutics/gender_studies
 *
 * SUMMARY:
 *   The Qur'anic verses that establish gender-differentiated rules
 *   (testimony, inheritance, family authority) occupy a contested
 *   hermeneutical space in contemporary Islam. The contextual-egalitarian
 *   reading interprets these verses as responses to specific 7th-century
 *   Arabian conditions — where women lacked formal education, property
 *   rights, legal standing — and therefore requiring reinterpretation in
 *   light of the Qur'an's overarching principles of equity, justice, and
 *   human dignity (maqasid al-shariah). This reading emerged forcefully in
 *   late-20th-century Islamic scholarship (Fatima Mernissi, Muhammad Shahrur,
 *   Asma Barlas, Amina Wadud, contemporary projects in Malaysia, Tunisia, and
 *   Egypt) but faces sustained resistance from traditional jurisprudential
 *   schools and from patriarchal legal establishments in Muslim-majority
 *   states. The constraint exhibits a tangled coordination-extraction hybrid:
 *   the reading genuinely solves the theological problem of reconciling
 *   Qur'anic authority with contemporary gender equity (coordination
 *   function), but simultaneously shifts interpretive power from traditional
 *   male scholars to reformist networks and women scholars (asymmetric
 *   extraction). The extractiveness value (0.52) reflects moderate but
 *   substantial structural conflict: women exit the victim set under the
 *   traditional reading and gain claims to equal inheritance/testimony;
 *   patriarchal elites lose discretionary power; intra-community conflict
 *   over interpretive legitimacy intensifies. The suppression metric (0.68)
 *   reflects the institutional resistance required to maintain traditional
 *   interpretations — in many Muslim-majority legal systems, the state uses
 *   judicial authority, educational control, and fatwa monopolies to prevent
 *   the contextual-egalitarian reading from gaining institutional standing.
 *   Theater ratio (0.58) indicates that traditional jurisprudential authority
 *   increasingly operates through performative legitimacy rather than
 *   functional power — schools issue fatwas maintaining traditional
 *   positions, but their ability to enforce these positions in practice has
 *   degraded in urban, educated communities, while reformist networks and
 *   NGOs exercise real influence through university positions, publishing,
 *   international advocacy, and community education. The constraint's
 *   measurement trajectory shows rising extractiveness (0.32 → 0.52) and
 *   rising theater (0.42 → 0.58) over a 40-year interval (roughly 1985–2025),
 *   reflecting the reading's increasing competitive position against
 *   traditional authority while that authority's functional power erodes.
 *
 * KEY AGENTS:
 *   - Women in Muslim-Majority Communities: Primary victim under traditional reading, primary beneficiary under contextual-egalitarian reading (powerless/identity_locked under traditional interpretation → moderate/constrained under reformist framing). Structural relationship determines which reading they occupy.
 *   - Reformist Islamic Scholars: Primary beneficiary (institutional/arbitrage). Gain interpretive authority, publishing prominence, international institutional positions. The constraint codifies their methodological position as legitimate alternative.
 *   - Rights-Based NGOs (CEDAW advocates, gender-equality organizations): Primary beneficiary (institutional/arbitrage). Gain Islamic theological grounding for gender-equity advocacy, permitting work within Muslim communities without accusation of Western cultural imperialism. The constraint provides legitimacy bridge.
 *   - Traditional Jurisprudential Schools (al-Azhar, Qayrawan, pesantren networks): Primary victim (institutional/constrained). Lose interpretive monopoly, face erosion of authority to train judges and issue binding fatwas. Intra-community legitimacy contested.
 *   - Patriarchal Legal Establishments (state family courts, qadi systems, honor-based justice frameworks): Primary victim (institutional/constrained). The constraint removes legal discretion in inheritance and testimony. Face delegitimization from rights-based reform movements.
 *   - Progressive Muslim Communities: Secondary victim and partial beneficiary (moderate/constrained). Bear the hermeneutical labor of reinterpretation and face accusations of cultural betrayal; gain theological coherence and social alignment with global equity norms.
 *   - Feminist Islamic Scholarship Movement (Asma Barlas, Amina Wadud, institutional research networks): Beneficiary-victim hybrid (organized/mobile). Benefit from opening of exegetical space; bear labor of establishing women as co-interpreters; experience epistemic resistance from traditional authority.
 *   - Analytical Observer: (analytical/analytical). Risks naturalizing either reading as obvious truth rather than recognizing the institutional dynamics that determine which reading gains power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__contextual_egalitarian, 0.52).
domain_priors:suppression_score(quranic_gender_verses__contextual_egalitarian, 0.68).
domain_priors:theater_ratio(quranic_gender_verses__contextual_egalitarian, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, extractiveness, 0.52).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(quranic_gender_verses__contextual_egalitarian, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__contextual_egalitarian, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__contextual_egalitarian, "Qur'anic Gender Verses: Contextual-Egalitarian Reading").
narrative_ontology:topic_domain(quranic_gender_verses__contextual_egalitarian, "islamic_jurisprudence/legal_hermeneutics/gender_studies").

domain_priors:requires_active_enforcement(quranic_gender_verses__contextual_egalitarian).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__contextual_egalitarian, '95a1e4eb-00e8-4f28-9945-22a3ff965ca3').
narrative_ontology:cs_kernel_codification('95a1e4eb-00e8-4f28-9945-22a3ff965ca3', fixed_text).
narrative_ontology:cs_authority_grounding('95a1e4eb-00e8-4f28-9945-22a3ff965ca3', extraction).
narrative_ontology:cs_interpretation_layer_present('95a1e4eb-00e8-4f28-9945-22a3ff965ca3').
narrative_ontology:cs_reading_relation('95a1e4eb-00e8-4f28-9945-22a3ff965ca3', quranic_gender_verses__literal_hierarchical, coexists_with).
narrative_ontology:cs_reading_relation('95a1e4eb-00e8-4f28-9945-22a3ff965ca3', quranic_gender_verses__progressive_abrogation, coexists_with).
narrative_ontology:cs_axiom('95a1e4eb-00e8-4f28-9945-22a3ff965ca3', foundational, gender_rules_contextually_responsive).
narrative_ontology:cs_axiom_status(gender_rules_contextually_responsive, holdable).
narrative_ontology:cs_axiom_grounding('95a1e4eb-00e8-4f28-9945-22a3ff965ca3', gender_rules_contextually_responsive, empirically_contingent).
narrative_ontology:cs_axiom('95a1e4eb-00e8-4f28-9945-22a3ff965ca3', foundational, equity_as_overarching_principle).
narrative_ontology:cs_axiom_status(equity_as_overarching_principle, holdable).
narrative_ontology:cs_axiom_grounding('95a1e4eb-00e8-4f28-9945-22a3ff965ca3', equity_as_overarching_principle, deontological).
narrative_ontology:cs_reference_frame('95a1e4eb-00e8-4f28-9945-22a3ff965ca3', quranic_equity_principle_priority).
narrative_ontology:cs_drift_state('95a1e4eb-00e8-4f28-9945-22a3ff965ca3', contemporary_post_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('95a1e4eb-00e8-4f28-9945-22a3ff965ca3', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, reformist_scholarship_tradition).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, women_rights_advocates).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__contextual_egalitarian, rights_based_ngos).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, patriarchal_legal_establishment).
narrative_ontology:constraint_victim(quranic_gender_verses__contextual_egalitarian, traditional_judicial_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN UNDER TRADITIONAL INTERPRETATION (SNARE) — Identity-locked through religious/cultural framing: to exit the traditional reading is to exit Islam itself (from within that frame). Structurally mobile — have income, agency, education in many communities — but identity fused with the roles the traditional reading assigns. Maximum experienced extraction: reduced testimony weight, unequal inheritance, restricted mobility framed as protection. No exit appears thinkable from within the traditional interpretive frame.
constraint_indexing:constraint_classification(quranic_gender_verses__contextual_egalitarian, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: PROGRESSIVE MUSLIM COMMUNITIES (TANGLED ROPE) — Constrained by intra-community legitimacy risks and institutional resistance from traditional authorities. But the contextual-egalitarian reading offers genuine coordination benefit: it enables theological coherence with contemporary equity principles while maintaining Qur'anic authority. Asymmetric extraction: must bear the hermeneutical labor of reinterpretation and face accusations of cultural capitulation; gain improved social standing and alignment with rights frameworks. Mixed incentive structure — some groups benefit, some bear costs.
constraint_indexing:constraint_classification(quranic_gender_verses__contextual_egalitarian, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORMIST SCHOLARSHIP AND RIGHTS-BASED NGOs (ROPE) — Primary beneficiary. This reading codifies their interpretive authority and legitimizes their institutional position in global human-rights discourse. The constraint functions as pure coordination from their perspective: establishing the contextual-egalitarian frame enables follow-up scholarship, institutional advocacy, and funding alignment. They experience the constraint as opening pathways, not closing them.
constraint_indexing:constraint_classification(quranic_gender_verses__contextual_egalitarian, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PATRIARCHAL LEGAL ESTABLISHMENT (SNARE) — Sees the contextual-egalitarian reading as extraction of their interpretive monopoly. Faces constrained exit: cannot simply reject the Qur'an or Islam without institutional suicide. Experiences suppression of their traditional authority claims. The constraint reduces their discretionary power in family law, inheritance courts, and testimony weights. High experienced extraction — they bear the cost of reframing without apparent coordination benefit.
constraint_indexing:constraint_classification(quranic_gender_verses__contextual_egalitarian, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL ISLAMIC JURISPRUDENTIAL SCHOOLS (PITON) — Formal guardianship of textual interpretation authority persists through institutional inertia. Theater ratio high (0.58): schools continue to issue fatwas and frame themselves as authoritative interpreters, but their actual power to enforce traditional readings has degraded in contexts where reformist readings compete. The constraint is performative maintenance of authority rather than functional control. They maintain legitimacy through ritual (formal jurisprudential methodology) while functional interpretation authority shifts to reformist networks and NGOs.
constraint_indexing:constraint_classification(quranic_gender_verses__contextual_egalitarian, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: FEMINIST ISLAMIC SCHOLARSHIP MOVEMENT (SCAFFOLD) — Organized agents (Fatima Mernissi, Amina Wadud, Asma Barlas, institutional research centers) see the contextual-egalitarian reading as a temporary support structure enabling women's exegetical participation. The sunset clause is the institutionalization of women as co-interpreters: once gender-inclusive exegetical authority is normalized in Islamic scholarship, the scaffold dissolves into rope (pure coordination). Low theater because the movement's methodology is genuinely novel exegetical practice, not performative ritual. Mobile exit: if the exegetical space opens, the movement can transition to steady-state scholarly participation.
constraint_indexing:constraint_classification(quranic_gender_verses__contextual_egalitarian, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal perspective, gender equity is an immutable principle grounded in universal human dignity and reason. The contextual-egalitarian reading aligns textual interpretation with this natural-law foundation. From this view, the traditional reading is an aberration, and the contextual-egalitarian reading simply reveals what was always true. However, the structural data contradicts this: significant suppression, contested authority, organized resistance indicate contingent institutional dynamics rather than self-evident natural law. This perspective instantiates the oracle gap: the analytical position assumes its own frame is transparent to the constraint.
constraint_indexing:constraint_classification(quranic_gender_verses__contextual_egalitarian, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__contextual_egalitarian_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quranic_gender_verses__contextual_egalitarian, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quranic_gender_verses__contextual_egalitarian, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__contextual_egalitarian, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quranic_gender_verses__contextual_egalitarian, TR),
    TR >= 0.70.

:- end_tests(quranic_gender_verses__contextual_egalitarian_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-to-substantial. The contextual-egalitarian reading redistributes interpretive authority and changes women's structural position in family law and inheritance — substantial gains for one set of agents (reformist scholars, rights-based NGOs, women claiming equal standing) and substantial losses for another (traditional jurisprudential authority, patriarchal courts). The reading is not pure coordination because it requires suppressing alternative interpretive methodologies (traditional schools must defend why their methodology is valid despite the coherence of the contextual approach). Extractiveness is not as high as the literal-hierarchical reading (which preserves traditional authority monopoly) because the contextual-egalitarian reading does offer genuine theological coordination benefit — it enables Muslims to maintain Qur'anic authority while embracing contemporary equity, resolving a real theological tension. Suppression (0.68): Moderate-high. The traditional reading maintains institutional enforcement through state judicial systems, educational control (which interpretive traditions are taught in seminaries), fatwa monopolies (who has standing to issue authoritative rulings), and social conformity pressure (gender roles naturalized through religious framing). The contextual-egalitarian reading faces active suppression: fewer pesantren teach it, state-sponsored muftis typically reject it, traditional scholars deploy accusations of cultural apostasy, family social pressure enforces traditional interpretations in many communities. However, suppression is not total (0.78+) because reformist networks have alternative institutional bases (NGOs, universities, online platforms, diaspora communities) and some Muslim-majority states (Tunisia, Morocco) have institutionalized family law reforms aligned with contextual-egalitarian principles. Theater ratio (0.58): Moderate-high. Traditional jurisprudential authority increasingly operates through ritual performance: issuing fatwas in the traditional format, conducting judicial deliberations in the traditional method, citing foundational authorities in the traditional manner — while actual power to enforce these determinations has eroded. Reformist scholarship and rights-based NGOs exercise functional power (influence over women's legal claims, institutional policy, educational curricula) but less elaborate ritual performance. The theater rises over the interval as traditional schools maintain formalistic authority while losing functional power, indicating piton degradation in the institutional architecture.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a maximum perspectival gap, with all six types represented. Women under traditional interpretation experience Snare (identity-locked): structurally mobile but identity-fused with traditional gender roles; exit appears unthinkable from within the traditional frame. Reformist scholars experience Rope: the contextual-egalitarian reading opens pathways for institutional authority and scholarly influence. The traditional jurisprudential establishment experiences Snare (institutional/constrained): loses their interpretive monopoly without visible exit or benefit. The feminist Islamic scholarship movement experiences Scaffold: temporary support structure enabling women exegetes to establish themselves; sunset occurs when women are normalized as co-interpreters. The analytical observer risks experiencing Mountain: naturalizing gender equity as an immutable principle rather than recognizing the institutional dynamics that enable or suppress different readings. The gap reflects real structural differences in how agents experience the constraint — it is not perspectival relativism but structural differentiation based on power, exit options, and relationship to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position: (1) Women under traditional reading: powerless + identity_locked → high d (0.89–0.95), experiencing maximum extraction. Under contextual-egalitarian reading, the same women experience moderate d (0.55–0.65) — they gain structural claims to equal inheritance/testimony but remain constrained by community resistance and internalized patriarchal frames. The reading does not fully free them from the constraint; it redistributes who benefits from it. (2) Reformist scholars: institutional + arbitrage → low d (0.05–0.15), experiencing negative extraction (subsidy/benefit). (3) Traditional jurisprudential schools: institutional + constrained → moderate-high d (0.60–0.75), experiencing substantial extraction as their functional authority erodes. (4) Rights-based NGOs: institutional + arbitrage → low d (0.05–0.15), gaining legitimacy and institutional standing. (5) Feminist Islamic scholarship: organized + mobile → moderate d (0.50–0.65), experiencing mixed incentives. The directional heterogeneity reveals that this is not a constraint uniformly affecting all agents — it redistributes the extraction flow, which is why it classifies as tangled rope rather than pure rope (coordination) or pure snare (extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the contextual-egalitarian reading instantiates a SPECIFIC institutional configuration: reformist scholars and rights-based NGOs gain interpretive authority; women gain structural claims within the family law domain; traditional jurisprudential schools lose institutional power. The reading is NOT a neutral hermeneutical improvement (Mountain) nor a pure coordination mechanism (Rope) nor a temporary support structure (Scaffold). It is a contested reconfiguration of who has authority to interpret the Qur'an on gender issues, and this reconfiguration has winners and losers. The tangled-rope classification captures this hybrid nature: the reading provides genuine theological coordination (resolving the tension between Qur'anic authority and contemporary equity), but only by extracting interpretive power from traditional authorities and redistributing it to reformist networks. Different groups experience the constraint according to their structural position relative to this power redistribution. The mandatrophy is resolved by acknowledging that there is no single 'true' classification — the constraint IS the contestation over which reading's institutional configuration becomes dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contextual_vs_normative_intent,
    'Are verses establishing gender-differentiated rules (testimony, inheritance) culturally-embedded responses to 7th-century conditions, or do they express immutable normative intent that transcends context?',
    'Comparative analysis of Qur''anic hermeneutical tradition: (a) when the text itself explicitly contextualizes a ruling (as in slavery regulations), does the same reasoning apply to gender rules? (b) Do foundational tafsir traditions (Ibn Abbas, al-Tabari) document historical context of specific verses? (c) What criteria distinguish time-bound from time-transcendent rules within Islamic jurisprudential methodology?',
    'If contextual embedding is accepted as hermeneutical principle: contextual-egalitarian reading is structural. If normative intent is primary: literal-hierarchical reading is structural. This omega determines which sibling reading has foundational legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contextual_vs_normative_intent, conceptual, 'Whether gender rules express cultural context or immutable normative intent').

omega_variable(
    maqasid_authority_grounding,
    'What gives the maqasid al-shariah (objectives of Islamic law) hermeneutical authority to override apparent textual meaning? On what principle can equity be declared a higher objective than explicit gender-differentiated rules?',
    'Genealogical analysis of maqasid development (al-Ghazali, al-Shatibi, contemporary revival); examination of which maqasid (preserve life, protect intellect, protect lineage, protect property, preserve faith) are invoked to justify gender equity vs. traditional rules; determination of whether maqasid represent methodological consensus or reformist innovation. Comparison with how maqasid are applied to other contested domains (interest prohibition, slavery).',
    'If maqasid have consensus hermeneutical standing: contextual-egalitarian reading gains structural legitimacy. If maqasid are reformist innovation without traditional jurisprudential basis: reading remains contestable, strengthening literal-hierarchical position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maqasid_authority_grounding, conceptual, 'Authority grounding for maqasid-based reinterpretation').

omega_variable(
    intra_community_authority_location,
    'Who has legitimate interpretive authority within Muslim communities — traditional jurisprudential schools, contemporary scholars with hermeneutical training, women''s voices, democratic community consensus, or some combination?',
    'Institutional mapping of who produces binding or influential fatwas in different Muslim-majority contexts; analysis of how women exegetes gain or lose standing; documentation of which reading communities (pesantren, seminaries, university programs, online networks) train interpreters; assessment of whether authority is determined by credentialing, institutional position, textual mastery, or community acceptance.',
    'If traditional schools retain exclusive authority: contextual-egalitarian reading remains marginalized (suppression high). If authority is democratized or distributed: reading gains structural power. If women gain co-interpretive standing: reading''s beneficiary group expands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intra_community_authority_location, empirical, 'Locus of hermeneutical authority in contemporary Muslim communities').

omega_variable(
    religious_identity_fusion_mechanism,
    'For women under traditional interpretation, is the identity lock primarily religious (Qur''an-based meaning-making), cultural (community belonging), relational (family structure), or some inseparable fusion?',
    'Ethnographic documentation of how women describe their constraints; interviews with converts to progressive readings about the cognitive shift required; analysis of whether women reject the traditional reading while remaining Muslim, and what this reveals about the binding mechanism; assessment of whether alternative readings (contextual-egalitarian) reduce the identity-lock experience or merely redistribute it.',
    'If primarily religious: the reading''s hermeneutical legitimacy directly unlocks the identity constraint. If primarily cultural or relational: hermeneutical change alone is insufficient. If fusion: the constraint may shift from identity_locked to constrained rather than dissolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_identity_fusion_mechanism, empirical, 'Identity-fusion binding mechanism for women under traditional reading').

omega_variable(
    literal_vs_contextual_falsifiability,
    'What empirical or textual evidence would demonstrate that the traditional literal reading is incorrect? What would demonstrate that the contextual-egalitarian reading has misapplied the maqasid framework?',
    'Specification of falsifying conditions for each reading: (a) For literal: discovery of contemporaneous documents showing the 7th-century context was NOT as traditionally understood? Textual evidence that the Prophet explicitly stated rules were time-bound? (b) For contextual-egalitarian: demonstration that applying equity principles consistently leads to contradiction with other explicit Qur''anic rules? Evidence that contemporary Muslim communities reject the reinterpretation regardless of philosophical coherence?',
    'If both readings are non-falsifiable: the constraint is purely conceptual (preference omega, not empirical). If one can be falsified and the other not: one reading has greater epistemic closure, indicating structural imbalance in how claims are adjudicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literal_vs_contextual_falsifiability, empirical, 'Falsifiability conditions for competing readings').

omega_variable(
    institutional_beneficiary_authentic_intent,
    'Do reformist scholars and rights-based NGOs genuinely believe the contextual-egalitarian reading is true, or do they adopt it instrumentally to gain legitimacy in Western rights discourse?',
    'Comparative analysis of how reformist scholars justify the reading in internal Islamic discourse vs. external human-rights presentations; examination of whether they maintain it consistently across other domains or apply selectivity; interviews with institutional actors about motivations for adoption; assessment of whether the reading was independently developed in Muslim scholarship or primarily imported from Western feminist theory.',
    'If genuine: the reading''s beneficiary group (reformist scholars, NGOs) has authentic ideological commitment, increasing stability. If instrumental: the constraint''s extraction mechanism (legitimacy-seeking) becomes transparent, potentially converting the reading into a false summit (naturalized power dynamics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_beneficiary_authentic_intent, empirical, 'Authenticity of institutional adoption of contextual-egalitarian reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__contextual_egalitarian, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qgv_ctx_eq_tr_t0, quranic_gender_verses__contextual_egalitarian, theater_ratio, 0, 0.42).
narrative_ontology:measurement(qgv_ctx_eq_tr_t20, quranic_gender_verses__contextual_egalitarian, theater_ratio, 20, 0.52).
narrative_ontology:measurement(qgv_ctx_eq_tr_t40, quranic_gender_verses__contextual_egalitarian, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(qgv_ctx_eq_be_t0, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(qgv_ctx_eq_be_t20, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(qgv_ctx_eq_be_t40, quranic_gender_verses__contextual_egalitarian, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(qgv_ctx_eq_su_t0, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(qgv_ctx_eq_su_t20, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(qgv_ctx_eq_su_t40, quranic_gender_verses__contextual_egalitarian, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__contextual_egalitarian, identity_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, quranic_gender_verses__progressive_abrogation).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, islamic_family_law__inheritance_reform).
narrative_ontology:affects_constraint(quranic_gender_verses__contextual_egalitarian, islamic_family_law__testimony_equity).

% DUAL FORMULATION NOTE:
% The Qur'anic gender verses kernel decomposes into three structurally distinct constraint stories, each with different ε values, different beneficiary/victim structures, and different suppression mechanisms. The contextual-egalitarian reading (this constraint) has ε ≈ 0.52 and requires institutional reformism and hermeneutical training as enforcement mechanism. The literal-hierarchical reading (sibling constraint) has lower extractiveness because it preserves existing institutional arrangements, but higher enforcement cost to suppress alternative readings. The progressive-abrogation reading (sibling constraint) has different ε reflecting different theological methodology. All three are linked as readings of the same kernel; each is a separate constraint because they have incommensurable ε values and different institutional configurations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__contextual_egalitarian, institutional, 0.15).
constraint_indexing:directionality_override(quranic_gender_verses__contextual_egalitarian, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
