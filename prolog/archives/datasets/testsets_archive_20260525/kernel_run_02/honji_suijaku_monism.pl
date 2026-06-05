% ============================================================================
% CONSTRAINT STORY: honji_suijaku_monism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honji_suijaku_monism, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honji_suijaku_monism
 *   human_readable: Honji-Suijaku Monism: Buddhist Ontological Grounding of Kami
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   The honji-suijaku (original ground / phenomenal trace) doctrine
 *   represents one systematic response to the coexistence of Buddhist and
 *   kami religious practices in Japan. Under this framework, kami are
 *   understood as suijaku — localized manifestations or traces — of honji,
 *   the ultimate Buddhist ground (Buddha/bodhisattva forms or Buddha-nature
 *   itself). This is a reading of the contested kernel 'kami-buddha ontology'
 *   that subordinates indigenous kami theology to Buddhist metaphysical
 *   authority. The constraint exhibits the mixed character of institutional
 *   religious subordination: it provides genuine coordination benefits
 *   (unified ritual calendar, shared textual authority, institutional
 *   support) while extracting kami theological autonomy and centering
 *   interpretive authority in the Buddhist establishment. The framework
 *   reached institutional dominance from roughly the 9th–12th centuries and
 *   became formalized in the syncretic shrine-temple associations (jingū-ji)
 *   that characterized the early modern period. The constraint's theater
 *   ratio reflects that by the Edo period, honji-suijaku had become largely
 *   performative — the theological force of the doctrine had attenuated while
 *   institutional structures perpetuated it through inertia. This constraint
 *   family decomposes into three siblings (domain_partition,
 *   incoherent_bundle), each representing a different resolution of the
 *   kami-buddha relation, each with different beneficiary structures and
 *   extractiveness values.
 *
 * KEY AGENTS:
 *   - Buddhist Institutional Establishment: Primary beneficiary (institutional/arbitrage) — captures interpretive authority over both Buddhist and kami domains; extends doctrinal coherence to encompass local practice without doctrinal compromise
 *   - Kami Priests and Shrine Networks: Primary victim (powerless/trapped at biographical scale; moderate/constrained at institutional scale) — lose theological autonomy and interpretive authority; incorporated into Buddhist-centered framework
 *   - Shrine Worshippers and Kami Devotees: Secondary victim (moderate/constrained) — devotional practice reframed within Buddhist metaphysics; access to institutional resources but at cost of theological reinterpretation
 *   - Imperial Administrative Authority: Secondary beneficiary (powerful/mobile) — uses honji-suijaku doctrine as state coordination mechanism for unified religious governance
 *   - Indigenous Kami Theology: Systemic victim (analytical/trapped) — abstract system of theological concepts loses institutional embodiment; theological vocabulary and categories devalued relative to Buddhist frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honji_suijaku_monism, 0.38).
domain_priors:suppression_score(honji_suijaku_monism, 0.52).
domain_priors:theater_ratio(honji_suijaku_monism, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honji_suijaku_monism, extractiveness, 0.38).
narrative_ontology:constraint_metric(honji_suijaku_monism, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(honji_suijaku_monism, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honji_suijaku_monism, tangled_rope).
narrative_ontology:human_readable(honji_suijaku_monism, "Honji-Suijaku Monism: Buddhist Ontological Grounding of Kami").
narrative_ontology:topic_domain(honji_suijaku_monism, "religious_studies/philosophy_of_religion/japanese_cultural_history").

domain_priors:requires_active_enforcement(honji_suijaku_monism).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(honji_suijaku_monism, formalized).
narrative_ontology:cs_authority_grounding(honji_suijaku_monism, lineage).
narrative_ontology:cs_interpretation_layer_present(honji_suijaku_monism).
narrative_ontology:cs_kernel_id(honji_suijaku_monism, kami_buddha_ontology).
narrative_ontology:cs_reading_relation(honji_suijaku_monism, kami_buddha_domain_partition, coexists_with).
narrative_ontology:cs_reading_relation(honji_suijaku_monism, kami_buddha_incoherent_bundle, influences).
narrative_ontology:cs_axiom(honji_suijaku_monism, foundational, single_ultimate_reality_principle).
narrative_ontology:cs_axiom_status(single_ultimate_reality_principle, holdable).
narrative_ontology:cs_axiom_grounding(honji_suijaku_monism, single_ultimate_reality_principle, deontological).
narrative_ontology:cs_axiom(honji_suijaku_monism, foundational, manifestation_hierarchy_asymmetry).
narrative_ontology:cs_axiom_status(manifestation_hierarchy_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding(honji_suijaku_monism, manifestation_hierarchy_asymmetry, conventional).
narrative_ontology:cs_reference_frame(honji_suijaku_monism, unified_buddhist_ontological_authority).
narrative_ontology:cs_drift_state(honji_suijaku_monism, contemporary_sectarian_revival, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honji_suijaku_monism, buddhist_institutional_authority).
narrative_ontology:constraint_beneficiary(honji_suijaku_monism, syncretic_religious_elites).
narrative_ontology:constraint_victim(honji_suijaku_monism, indigenous_kami_theology).
narrative_ontology:constraint_victim(honji_suijaku_monism, kami_worshippers).
narrative_ontology:constraint_victim(honji_suijaku_monism, theological_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KAMI PRIEST / LOCAL SHRINE (SNARE) — Structurally trapped within the honji-suijaku framework once it becomes institutionally dominant. Cannot exit without abandoning professional identity and authority structure. Kami theology is subordinated to Buddhist metaphysics; kami cease to be autonomous beings and become mere manifestations. The priest bears the cost of theoretical subordination while the Buddhist establishment captures the authority to interpret both systems. Maximum experienced extraction with no organizational recourse.
constraint_indexing:constraint_classification(honji_suijaku_monism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SHRINE COMMUNITY / WORSHIPPERS (TANGLED ROPE) — Face real constraints: devotional practices toward kami are reframed as devotion to Buddha/bodhisattva; theological autonomy is curtailed; but integration into the Buddhist institutional network also provides new resources, legitimacy, and access to Buddhist textual authority. Extraction exists (loss of kami autonomy), but genuine coordination function emerges (combined ritual calendar, shared textual resources, institutional support for shrines). Mixed costs and benefits — not pure snare, but significant asymmetry.
constraint_indexing:constraint_classification(honji_suijaku_monism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BUDDHIST ESTABLISHMENT (ROPE) — Institutional beneficiary. The honji-suijaku framework expands Buddhist authority to encompass kami worship without abandoning Buddhist ontology. Priests, monasteries, and Buddhist elite gain interpretive authority over indigenous religious practice. The constraint appears as coordination to Buddhist actors: we are solving the problem of how to maintain doctrinal coherence while incorporating local practice. Net beneficiary position with arbitrage options — can engage or disengage from kami contexts while maintaining institutional primacy.
constraint_indexing:constraint_classification(honji_suijaku_monism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: IMPERIAL SYNCRETIC AUTHORITY (TANGLED ROPE) — The state uses honji-suijaku doctrine as a coordination mechanism for unified religious administration: a single theoretical framework allows both Buddhist and kami institutions to coexist under centralized authority. The constraint functions as genuine coordination (solves the state's need to govern diverse religious populations) but also extracts by centralizing interpretive authority and subordinating autonomous shrine networks. Powerful actor with mobile exit options — can adjust the framework or abandon it if political conditions change.
constraint_indexing:constraint_classification(honji_suijaku_monism, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL LEGACY SYSTEM (PITON) — By the Edo and early modern periods, honji-suijaku had become largely performative institutional practice: temples maintained dual identity structures, priests recited corresponding honji-suijaku formulae, but the actual theological commitment had weakened in many contexts. The framework persists through institutional inertia — shrine-temple associations continue, formulae are transmitted, the system is maintained by regulation and tradition despite reduced functional theological force. Theater ratio high relative to actual theological enforcement.
constraint_indexing:constraint_classification(honji_suijaku_monism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational vantage, the subsumption of kami under Buddhist ontology appears as a natural theoretical consequence: given Buddhist metaphysics of ultimate emptiness and manifest forms, kami must be phenomenal expressions of Buddha-nature. This appears as an inexorable logical conclusion rather than a contestable historical imposition. However, the presence of alternative readings (domain partition, incoherent bundle) and the identifiable beneficiaries of the honji-suijaku framework suggest this is a false summit: a contingent theological choice naturalized as philosophical necessity.
constraint_indexing:constraint_classification(honji_suijaku_monism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honji_suijaku_monism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(honji_suijaku_monism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(honji_suijaku_monism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(honji_suijaku_monism, TR),
    TR >= 0.70.

:- end_tests(honji_suijaku_monism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The honji-suijaku framework extracts kami theological autonomy and subordinates local authority to Buddhist interpretive power, but the extraction is not as severe as pure institutional suppression would suggest (which might score ε ≥ 0.46). The framework provides genuine coordination benefits and institutional resources — shrines receive Buddhist support, dual ritual calendars function, theological coherence is achieved. The extraction is real but embedded in a mixed coordination function. Theater ratio (0.58): Moderate-high. By the later institutional period (Edo onward), honji-suijaku had become increasingly performative: temples and shrines maintained the dual-identity structures and corresponding nomenclature, but the theological force of the doctrine had weakened. Priests recited the framework without active enforcement of its metaphysical claims. The institutional apparatus persisted through inertia rather than active theological commitment. Suppression (0.52): Moderate-high. Suppression operates through institutional structure (shrine-temple associations, legal incorporation, textual monopoly) and through internalized intellectual capture (kami priests educated within Buddhist philosophical frameworks, conceptual vocabulary shifted toward Buddhist categories). Not total suppression — kami theology was never completely eradicated — but substantial constraint on autonomous expression and development.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the Buddhist establishment (rope) and kami priests (snare) is maximal — the same constraint appears as pure coordination to the beneficiary and pure extraction to the victim. The beneficiary genuinely perceives the constraint as solving a real problem (how to maintain Buddhist doctrinal coherence while incorporating local worship), while the victim perceives loss of theological ground and interpretive authority. The imperial authority (tangled rope) perceives genuine coordination (unified religious administration) with some asymmetric benefits. The institutional legacy system (piton) reveals a temporal gap: the constraint was once enforced with high theological commitment; by the modern period it persists through inertia with reduced functional force. The analytical observer (mountain) risks naturalizing what is contingent: the subsumption of kami under Buddhist honji appears as metaphysical necessity until the presence of alternative readings (domain_partition, incoherent_bundle) reveals it as a constructed framework.
 *
 * DIRECTIONALITY LOGIC:
 *   The honji-suijaku reading generates distinct directionality values for different agents because their structural relationship to the constraint varies sharply. The Buddhist establishment (beneficiary + arbitrage) derives d ≈ 0.10–0.20, experiencing low or negative effective extraction — the framework benefits them. Kami priests (victim + trapped at biographical scale) derive d ≈ 0.90–0.95, experiencing maximum extraction — they lose professional autonomy and theological authority with no exit option. Shrine communities (victim + constrained) derive d ≈ 0.65–0.75, experiencing significant but not maximal extraction — they face high costs to exit (loss of institutional support) but receive coordination benefits. The imperial authority (beneficiary + mobile) derives d ≈ 0.35–0.45, experiencing moderate extraction — they benefit from religious coordination but can shift the framework if political conditions demand. The chi formula scales these base d values by f(d) (the sigmoid directionality function), producing the perspectival gap between rope (Buddhist view), tangled rope (moderate/constrained view), and snare (trapped view).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits moderate mandatrophy risk (extractiveness 0.38, below the 0.46 threshold for requiring mandatrophy resolution). However, the constraint's classification as tangled_rope introduces the mandatrophy question: Is this framework a genuine coordination mechanism that happens to have asymmetric benefits, or is it extraction disguised as coordination? The resolution: the framework exhibits BOTH genuine coordination (unified administration, shared resources, ritual integration) AND asymmetric extraction (Buddhist authority capture, kami theological subordination). The tangled_rope classification is appropriate and resolves the mandatrophy by recognizing the dual function. The piton perspective (institutional legacy system) adds temporal dimension: the coordination function was stronger in the foundation period (9th–12th centuries) and weakened by the early modern period, while institutional apparatus persisted. This suggests that the extraction mechanism has outlived the coordination function — a classic degradation into piton territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    buddhist_metaphysics_universality,
    'Is the subsumption of kami under Buddhist honji-suijaku logic a necessary consequence of Buddhist metaphysical principles, or a contingent historical choice available only to particular schools and periods?',
    'Comparative analysis of Buddhist responses to local deities across Asia (China, Tibet, Southeast Asia). Examination of Buddhist philosophical texts to determine whether the honji-suijaku logic is a required implication or one optional deployment among others.',
    'If necessary consequence: the mountain reading is partially justified — the logic is inherent to Buddhist metaphysics. If contingent: the framework is a constructed doctrine, and the constraint is primarily tangled-rope/snare (institutional extraction under theological cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(buddhist_metaphysics_universality, empirical, 'Whether honji-suijaku subsumption is metaphysically necessary or historically contingent').

omega_variable(
    kami_ontological_independence,
    'Can kami be coherently understood as independent beings with their own ontological ground, or does their phenomenal character (manifestation, appearance, trace) logically require a prior honji (original ground)?',
    'Philological and textual analysis of pre-Buddhist kami theology and cosmology. Philosophical reconstruction of kami ontology independent of honji-suijaku framework. Assessment of whether kami worship can sustain a coherent theology without Buddhist grounding.',
    'If kami can be ontologically independent: the domain_partition reading gains strength, and the honji-suijaku reading appears as theoretical imperialism. If kami necessarily require grounding: the honji-suijaku reading has stronger internal logical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kami_ontological_independence, conceptual, 'Whether kami require Buddhist grounding or can be ontologically autonomous').

omega_variable(
    reading_committer_framework_scope,
    'This constraint is one reading (honji-suijaku monism) of a contested kernel (kami-buddha ontology). Do the competing readings (domain_partition, incoherent_bundle) instantiate genuinely different commitment frameworks, or are they intra-Buddhist theological differences?',
    'Historical analysis of which institutional actors and theological traditions held each reading. Determination of whether alternative readings were championed by kami-centered authorities or by Buddhist schools with different metaphysical premises.',
    'If readings reflect different institutional frameworks: the reading_relations should be coexists_with (different parties, different commitments). If readings are intra-Buddhist schools: the relations might be foreclosed or influences (one Buddhist school''s logic rules out or pressures others).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_framework_scope, empirical, 'Whether sibling readings represent distinct committer frameworks or intra-Buddhist theological variation').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of kami theological autonomy primarily structural (legal prohibition of kami-centered doctrine, enforcement of dual nomenclature, institutional subordination) or internalized (kami priests themselves adopt the framework, accept kami as manifestations, lose the conceptual vocabulary for autonomous kami theology)?',
    'Historical examination of shrine-temple integration: voluntary adoption vs. forced incorporation. Analysis of kami priestly training and textual transmission — whether kami theology was actively suppressed or passively replaced. Post-Meiji separation of shrines and temples — did restoration of institutional independence restore kami theological autonomy?',
    'If primarily structural suppression: the constraint remains snare from the kami perspective as long as enforcement is maintained; suppression could decrease if enforcement lapses. If internalized: suppression persists even after institutional separation; recovery of autonomous kami theology requires cultural/cognitive reconstruction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of kami autonomy is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honji_suijaku_monism, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honj_tr_t0, honji_suijaku_monism, theater_ratio, 0, 0.42).
narrative_ontology:measurement(honj_tr_t4, honji_suijaku_monism, theater_ratio, 4, 0.51).
narrative_ontology:measurement(honj_tr_t8, honji_suijaku_monism, theater_ratio, 8, 0.58).

% Extraction over time
narrative_ontology:measurement(honj_be_t0, honji_suijaku_monism, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(honj_be_t4, honji_suijaku_monism, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(honj_be_t8, honji_suijaku_monism, base_extractiveness, 8, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honji_suijaku_monism, identity_coordination).
narrative_ontology:affects_constraint(honji_suijaku_monism, kami_buddha_domain_partition).
narrative_ontology:affects_constraint(honji_suijaku_monism, kami_buddha_incoherent_bundle).
narrative_ontology:affects_constraint(honji_suijaku_monism, shrine_temple_institutional_coupling).
narrative_ontology:affects_constraint(honji_suijaku_monism, meiji_kami_theological_restoration).

% DUAL FORMULATION NOTE:
% The honji-suijaku reading is upstream to two sibling readings (domain_partition, incoherent_bundle) within the kami_buddha_ontology kernel. Each reading produces a distinct constraint with different ε values, beneficiary structures, and suppression mechanisms. The honji-suijaku reading is the institutionally dominant reading, making it the network center. The domain_partition reading would constitute kami and Buddhas as distinct ontological systems (likely lower extractiveness, different beneficiary structures). The incoherent_bundle reading would treat the relationship as fundamentally contested and resistant to integration (likely higher theater ratio, reduced institutional enforceability). Each reading is a separate constraint story linked by network.affects_constraints to represent the family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honji_suijaku_monism, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
