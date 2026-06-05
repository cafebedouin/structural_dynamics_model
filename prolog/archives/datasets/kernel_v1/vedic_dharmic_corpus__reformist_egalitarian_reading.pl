% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__reformist_egalitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__reformist_egalitarian_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__reformist_egalitarian_reading
 *   human_readable: Vedic Dharmic Corpus — Reformist Egalitarian Reading
 *   domain: religious_authority/social_stratification/interpretive_legitimacy
 *
 * SUMMARY:
 *   The reformist egalitarian reading of the Vedic dharmic corpus asserts
 *   that textual meaning must conform to constitutional equality principles;
 *   that caste hierarchy is historical accretion rather than scriptural
 *   essence; and that rational critique supersedes traditional authority as
 *   the criterion for legitimate interpretation. This constraint instantiates
 *   one reading of a contested kernel — the Vedic corpus as a stabilized yet
 *   reinterpretable text — in direct contestation with the hereditary
 *   monopoly reading (which asserts brahminical interpretive authority and
 *   varna-system legitimacy) and the bhakti devotional reading (which
 *   privileges direct experience over textual exegesis). The reformist
 *   reading emerged as a coherent tradition in the 19th-20th century through
 *   the work of figures like Ram Mohan Roy, Ishwar Chandra Vidyasagar, and
 *   Swami Vivekananda, who synthesized upanishadic egalitarianism with
 *   Enlightenment rationalism and anti-colonial nationalism.
 *   Post-independence, the reading has been embedded in constitutional law
 *   (India's Constitution explicitly rejects caste hierarchy and enforces
 *   untouchability abolition) and administrative enforcement through the
 *   SC/ST Acts and landmark judicial interpretations. However, the reformist
 *   reading remains in active contestation with orthodox institutions that
 *   maintain hereditary ritual monopoly, brahminical authority structures,
 *   and de facto caste discrimination in social practice. The constraint
 *   exhibits substantial extractiveness (0.45) because the reading produces
 *   material redistribution of interpretive authority and ritual access from
 *   brahminical institutions to reformed/inclusive institutions and Dalit
 *   movements, backed by state enforcement. The suppression requirement
 *   (0.58) reflects ongoing resistance from orthodox institutions and
 *   persistence of informal caste discrimination despite legal prohibition.
 *   The theater ratio (0.64) indicates that reformist rhetorical claims to
 *   egalitarianism often exceed behavioral implementation — the gap between
 *   stated principles and actual ritual practice, institutional privilege,
 *   and social discrimination remains substantial.
 *
 * KEY AGENTS:
 *   - Dalit Movements & Dalit Rights Organizations: Primary beneficiaries (organized/mobile to powerless/identity_locked depending on subgroup) — gain interpretive legitimacy, legal protection, and religious inclusion through the reformist reading; bear structural barriers despite nominal rights
 *   - Orthodox Brahminical Institutions: Primary victims of delegitimization (institutional/constrained) — lose interpretive monopoly, ritual exclusivity, and hereditary authority; face legal enforcement and social contestation
 *   - State Constitutional Apparatus: Secondary beneficiary (institutional/arbitrage) — gains legitimacy for equality enforcement by anchoring law in reinterpreted religious authority; maintains monopoly on enforcement interpretation
 *   - Reform Hindu Organizations & Inclusive Religious Leaders: Secondary beneficiaries (institutional/arbitrage to moderate/constrained) — gain progressive legitimacy and bridge between traditional authority and modern legal regime; risk institutional capture and instrumental adoption
 *   - Brahminical Intellectuals & Textual Scholars Defending Traditional Reading: Secondary victims (powerful/constrained) — face professional delegitimization; lose standing in public discourse but retain institutional positions in orthodox organizations
 *   - Analytical Observer: External position (analytical/analytical) — perceives the constraint as a hermeneutic contest within a single tradition, or as state colonization of religious interpretation, depending on framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.58).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Vedic Dharmic Corpus — Reformist Egalitarian Reading").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious_authority/social_stratification/interpretive_legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, 'fbb671c9-2922-4956-b16e-7d7cba7a8a5e').
narrative_ontology:cs_kernel_codification('fbb671c9-2922-4956-b16e-7d7cba7a8a5e', fixed_text).
narrative_ontology:cs_authority_grounding('fbb671c9-2922-4956-b16e-7d7cba7a8a5e', lineage).
narrative_ontology:cs_interpretation_layer_present('fbb671c9-2922-4956-b16e-7d7cba7a8a5e').
narrative_ontology:cs_reading_relation('fbb671c9-2922-4956-b16e-7d7cba7a8a5e', vedic_dharmic_corpus__hereditary_monopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('fbb671c9-2922-4956-b16e-7d7cba7a8a5e', vedic_dharmic_corpus__bhakti_devotional_reading, influences).
narrative_ontology:cs_axiom('fbb671c9-2922-4956-b16e-7d7cba7a8a5e', foundational, dharma_inherently_egalitarian).
narrative_ontology:cs_axiom_status(dharma_inherently_egalitarian, holdable).
narrative_ontology:cs_axiom_grounding('fbb671c9-2922-4956-b16e-7d7cba7a8a5e', dharma_inherently_egalitarian, empirically_contingent).
narrative_ontology:cs_axiom('fbb671c9-2922-4956-b16e-7d7cba7a8a5e', foundational, rational_critique_supersedes_traditional_authority).
narrative_ontology:cs_axiom_status(rational_critique_supersedes_traditional_authority, holdable).
narrative_ontology:cs_axiom_grounding('fbb671c9-2922-4956-b16e-7d7cba7a8a5e', rational_critique_supersedes_traditional_authority, instrumental).
narrative_ontology:cs_reference_frame('fbb671c9-2922-4956-b16e-7d7cba7a8a5e', upanishadic_egalitarian_principle).
narrative_ontology:cs_drift_state('fbb671c9-2922-4956-b16e-7d7cba7a8a5e', contemporary_post_constitutional_enforcement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fbb671c9-2922-4956-b16e-7d7cba7a8a5e', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, progressive_legal_interpretation).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, state_egalitarian_apparatus).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahminical_institutions).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_authority_monopoly).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, traditional_varna_system_beneficiaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRUCTURALLY EXCLUDED DALIT (SNARE) — Identity fused with exclusion narrative; the reform reading offers cognitive escape ('you are equal under true dharma') but material barriers remain (caste discrimination in housing, marriage, employment). Cannot exercise exit even if identity frame shifted, because structural discrimination persists independent of textual reinterpretation. Experiences maximum extraction: zero structural benefit from the competing reading, maximum cost in bearing the contradiction between the reform narrative and lived exclusion.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__reformist_egalitarian_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM-ALIGNED RELIGIOUS INTELLECTUAL (TANGLED ROPE) — Constrained by professional risk (career dependence on religious institutions that may resist reinterpretation) but also benefits from the intellectual coherence and progressive legitimacy the reform reading provides. Genuine coordination function: reinterpretation can create space for inclusive religious community. Asymmetric extraction: institutional enforcement mechanisms (exclusion from orthodox spaces, loss of hereditary authority) fall on those defending the old reading, not this agent. Net extraction toward this perspective, but not maximal because the intellectual work produces genuine coordination value.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORTHODOX BRAHMINICAL INSTITUTION (SNARE) — Faces extraction through delegitimization and loss of hereditary monopoly on textual authority. The reformist reading directly targets the institutional beneficiaries of the old interpretation (brahminical privileges, ritual monopoly, varna-system authority). Constrained exit: institutional actors can resist reinterpretation but face mounting pressure from legal enforcement, public opinion, and intra-religious reformist movements. High suppression from the state apparatus enforcing egalitarian constitutional reading.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__reformist_egalitarian_reading, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DALIT RIGHTS MOVEMENT (TANGLED ROPE) — Organized agents with genuine coordination function: the reform reading provides intellectual legitimacy for legal advocacy and social mobilization. Mobile exit: the movement can operate through constitutional, legal, and civil-society channels. Benefits from reinterpretation (credibility, religious legitimacy for egalitarian claims) and creates coordination (inclusive reframing of dharma). Moderate extraction: enforcement mechanisms (confrontation with orthodox institutions, state repression of militant organizing) are real but movement has agency and alternative pathways.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE CONSTITUTIONAL APPARATUS (TANGLED ROPE) — Benefits from the reformist reading as legitimation for constitutional equality enforcement. Genuine coordination function: reinterpretation aligns religious authority with state legal authority (reducing friction between religious and secular institutions). Arbitrage exit: state can shift enforcement priorities without losing authority. Net beneficiary of the reading, but extraction is moderate because enforcement requires continuous institutional effort and produces ongoing contestation.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INTERPRETIVE PLURALISM VIEW (ROPE) — From civilizational scope, multiple readings of the Vedic corpus have coexisted and will continue to coexist. The reformist reading is a legitimate hermeneutic tradition with deep philosophical roots (upanishadic egalitarianism, advaita non-dualism). Pure coordination function at this level: clarifying legitimate readings enables more coherent religious pluralism. No inherent extraction from this view — structural benefit to all parties from having explicit readings rather than implicit contestation.
constraint_indexing:constraint_classification(vedic_dharmic_corpus__reformist_egalitarian_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vedic_dharmic_corpus__reformist_egalitarian_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vedic_dharmic_corpus__reformist_egalitarian_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45): Moderate-high, reflecting the reading's direct challenge to brahminical institutional authority. The measurement trajectory (0.28 → 0.38 → 0.45 over 40 years) shows extraction increasing as the reading moved from intellectual position (colonial period) through constitutional embedding (post-independence) to enforcement and social contestation (contemporary). The extractiveness is not at snare level (≥0.66) because: (1) the reading has genuine hermeneutic coherence grounded in upanishadic and advaita philosophical traditions, creating real coordination value, not merely coercive redistribution; (2) reform institutions have agency and can maintain alternative pathways (unlike trapped snare victims); (3) the state enforces the reading through legal authority independent of its religious authenticity, creating a hybrid extraction-coordination dynamic rather than pure extraction. Suppression (0.58): High, reflecting sustained resistance from orthodox institutions and continuation of informal discrimination despite legal prohibition. The measurement trajectory (0.42 → 0.52 → 0.58) shows suppression increasing as enforcement mechanisms tightened (SC/ST Act enforcement, constitutional amendments, temple-entry litigations) and resistance intensified. Suppression is not the dominant dynamic (which would push toward snare) because: (1) suppression is informal and distributed rather than centralized (no single institution holds monopoly on enforcement), (2) Dalit movements have built counter-organizational capacity and legal recourse, creating partial exit options, (3) state enforcement creates conduits for challenging suppression through constitutional processes. Theater ratio (0.64): Moderate-high, indicating substantial gap between the reading's egalitarian rhetoric and actual institutional practice. The trajectory (0.52 → 0.60 → 0.64) shows theater increasing as the reading became more embedded in formal law while informal discrimination persisted. Theater arises from: (1) reformist institutions' claims to have transcended brahminical privilege while retaining ritual authority and institutional position, (2) state enforcement of equality through constitutional amendment while social practice remains stratified, (3) extensive legal/religious/political discourse around equality that exceeds behavioral implementation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a perspectival gap between the beneficiaries of the reformist reading (Dalit movements, inclusive reformers, state apparatus) and its victims (orthodox institutions, brahminical authority). From the reformist institutional perspective (moderate/constrained), the reading is a genuine coordination achievement: reinterpreting dharma to align with egalitarian principles enables more inclusive religious community. From the orthodox institutional perspective (institutional/constrained), the reading is extraction through delegitimization: loss of interpretive authority, ritual monopoly, and social status. From the Dalit perspective (powerless/identity_locked), the reading's cognitive promise ('you are equal under true dharma') contradicts material reality (discrimination persists in housing, marriage, ritual access, employment). This perspectival gap reveals that the reformist reading does not resolve the underlying inequality so much as reframe its legitimacy — what changes is the narrative frame, not the material distribution of power. The analytical observer's rope classification (pure coordination at civilizational scope) risks naturalizing as harmless pluralism what is actually an ongoing extractive conflict where the reading is one weapon among many.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position relative to the constraint. Dalit movements occupy victim+organized status with constrained to mobile exit options, producing d ≈ 0.55–0.65 (high extraction experienced despite some agency). Orthodox institutions occupy beneficiary-of-old-order+constrained status, now targets of delegitimization, producing d ≈ 0.75–0.85 (high extraction from loss of monopoly). The state apparatus occupies beneficiary+arbitrage status (enforcement power without competing obligation), producing d ≈ 0.10–0.20 (low extraction, net benefit). Reform intellectuals occupy secondary beneficiaries+constrained status, producing d ≈ 0.35–0.45 (moderate extraction from professional risk, moderate benefit from legitimacy). The Dalit powerless agent with identity_locked exit experiences d ≈ 0.90 (maximum extraction: no material benefit from reinterpretation, maximum cognitive dissonance between the reform narrative and lived exclusion). The analytical observer with analytical exit experiences d ≈ 0.72 (observer extraction from inability to resolve the hermeneutic contest from external position alone).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_essentialism_vs_constructed_reading,
    'Is the reformist egalitarian reading a discovery of dharma''s true essence (''caste hierarchy is historical accretion'') or a constructed reinterpretation that projects contemporary values onto ancient texts?',
    'Hermeneutic analysis of textual layers and dating; examination of reformist reading''s historical emergence (19th-20th century) relative to orthographic evidence; assessment of whether the reading reconstructs or creates doctrinal coherence',
    'If essentialism holds: reformist reading is recovery, not invention; orthodox institutions are violating true dharma (reading becomes mountain-adjacent). If construction holds: reading is one legitimate option among several; no reading has exclusive claim to dharmic authenticity (coexistence becomes stable outcome).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_essentialism_vs_constructed_reading, conceptual, 'Whether reformist reading discovers or constructs dharmic essence').

omega_variable(
    brahminical_institutional_capture,
    'Do reformist institutional actors (reform Hindu organizations, inclusive religious leaders) genuinely reinterpret dharma or instrumentally adopt egalitarian framing to retain institutional authority in the post-colonial state?',
    'Longitudinal analysis of reform institutions'' positions on: (1) ritual monopoly (do they relinquish hereditary privileges?), (2) economic redistribution (do they support land reform or wealth redistribution targeting caste inequality?), (3) inter-caste marriage (do they perform ceremonies across caste lines without ritual degradation?). Assessment of whether institutions'' practices match their explicit egalitarian claims.',
    'If genuine: reformist institutions produce real coordination (Rope/Tangled Rope perspectives stable). If instrumental: reformist reading is a theater mechanism masking continued extraction (snare classification becomes more robust across perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahminical_institutional_capture, empirical, 'Whether reform institutions genuinely relinquish brahminical privileges').

omega_variable(
    state_enforcement_legitimacy_source,
    'Does the state''s enforcement of constitutional equality through reinterpretation of religious authority derive legitimacy from: (a) the reformist reading''s internal logic, (b) the state''s legal authority independent of religious authenticity, or (c) a hybrid where state power colonizes the reinterpretation process?',
    'Examination of state policy justifications: do they cite theological argument or only constitutional mandate? Analysis of cases where constitutional authority overrides religious authority (e.g., bans on untouchability rituals, enforcement of inter-caste access to temples). Assessment of whether state''s position is supporting a legitimate internal reading or imposing external authority.',
    'If (a): reformist reading is autonomous within dharmic tradition; constraint is endogenous conflict (Tangled Rope). If (b): state is the actual authority; religious reinterpretation is derivative theater (snare from state perspective, but state-imposed extraction from orthodox perspective). If (c): hybrid colonization; reading becomes instrumentalized (snare from orthodox perspective, rope from state perspective, but both are means to state ends).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_legitimacy_source, empirical, 'Source of legitimacy for state enforcement of constitutional egalitarianism').

omega_variable(
    practical_implementation_of_equality_norm,
    'Does the reformist reading''s egalitarian principle translate into material social change (reduced discrimination, increased inter-caste mobility, relinquishment of ritual monopoly), or does it function primarily as a rhetorical device in legal/political discourse while social practice remains stratified?',
    'Comparative analysis of: (1) legal protections (SC/ST Act enforcement, temple-entry laws), (2) social indicators (inter-caste marriage rates, occupational mobility, ritual participation across caste), (3) institutional practice (do reform temples actually practice inclusive ritual? do reform leaders marry across caste?). Measurement of gap between stated egalitarian principles and behavioral outcomes.',
    'If implementation is high: constraint represents genuine coordinate reframing (Rope/Tangled Rope stable). If implementation is low: constraint is primarily theater (Piton classification becomes salient; suppression may be understated because coercive mechanisms are nominally dismantled but informally persist).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(practical_implementation_of_equality_norm, empirical, 'Whether egalitarian principles translate to material social change').

omega_variable(
    sibling_reading_logical_relation,
    'Does the reformist egalitarian reading logically foreclose the hereditary monopoly reading (making them mutually exclusive in principle), or do they coexist as different readings that different factions hold simultaneously?',
    'Formal analysis of logical structure: if reformist reading asserts ''dharma is inherently egalitarian,'' does this entail ''hereditary monopoly reading is false/inauthentic'' in the same logical framework? Or do both readings coexist because they operate in different institutional jurisdictions (state-enforced law vs. orthodox temple practice) or epistemic communities (reformist vs. orthodox scholars)?',
    'If foreclosure: reformist reading is engaged in direct contest for authoritative interpretation; constraint is zero-sum (snare from orthodox perspective). If coexistence: constraint is institutional pluralism; multiple readings persist (rope/tangled rope from coexistence perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_logical_relation, conceptual, 'Logical relationship between reformist and hereditary monopoly readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vdcr_theater_t0_colonial_period, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(vdcr_theater_t20_post_independence, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 20, 0.6).
narrative_ontology:measurement(vdcr_theater_t40_contemporary, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 40, 0.64).

% Extraction over time
narrative_ontology:measurement(vdcr_extractiveness_t0_colonial_period, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(vdcr_extractiveness_t20_post_independence, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(vdcr_extractiveness_t40_contemporary, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 40, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vdcr_suppression_t0_colonial_period, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(vdcr_suppression_t20_post_independence, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(vdcr_suppression_t40_contemporary, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__reformist_egalitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_equality_enforcement).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, caste_discrimination_in_social_practice).

% DUAL FORMULATION NOTE:
% The reformist egalitarian reading is one of three structurally distinct constraints within the vedic_dharmic_corpus kernel. Each reading has its own ε, beneficiary/victim structure, and classification. The three readings coexist in active contestation. The reformist reading (this story, ε=0.45) upstream-affects the hereditary monopoly reading (ε=0.72, snare) by delegitimizing its authority claims. The reformist reading is influenced by the bhakti reading's philosophical resources (upanishadic egalitarianism) but shapes how bhakti is interpreted in contemporary context. All three readings are downstream of constitutional_equality_enforcement, which provides the state-institutional backing for the reformist reading's enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_dharmic_corpus__reformist_egalitarian_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
