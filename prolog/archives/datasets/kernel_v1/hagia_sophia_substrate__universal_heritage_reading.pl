% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__universal_heritage_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__universal_heritage_reading
 *   human_readable: Hagia Sophia Universal Heritage Legitimacy (Museum Administration Reading)
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   The Hagia Sophia universal heritage legitimacy reading represents one
 *   institutionalized framing of a contested kernel: the question of what
 *   authority structure and reading framework should govern a 6th-century
 *   religious building that has functioned as Orthodox cathedral, Islamic
 *   mosque, and secular museum across its 1,500-year history. This reading
 *   instantiates the museum/heritage governance model, where legitimacy
 *   derives from UNESCO frameworks, international scholarly consensus, and
 *   secular constitutional authority. The reading treats the building as
 *   transcending any single religious or national claim and as belonging to
 *   'humanity as a whole' through preservation and scholarly access. This
 *   constraint exhibits tangled coordination-extraction dynamics: genuine
 *   coordination benefits (preservation funding, maintenance expertise,
 *   international conservation standards) coexist with asymmetric extraction
 *   (suppression of Islamic worship claims, marginalization of Orthodox
 *   restitution narratives, ideological signaling of secular modernity). The
 *   constraint's theater ratio shows rising performative content over the
 *   25-year interval (0.48→0.72), indicating that the secular museum framing
 *   requires increasing theatrical maintenance as religious pluralism norms
 *   strengthen globally and the purely secular legitimation framework faces
 *   mounting pressure. The suppression requirement has also increased
 *   (0.35→0.48), reflecting growing enforcement costs as Islamic and Orthodox
 *   advocacy intensifies. This reading is one of three structural positions
 *   in the hagia_sophia_substrate kernel; the other readings
 *   (islamic_sovereignty_reading, orthodox_restitution_reading) represent
 *   competing legitimacy frameworks and will be instantiated in separate
 *   constraint stories linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Global Tourism Sector: Primary beneficiary (institutional/arbitrage) — derives revenue and access stability from universal heritage framing
 *   - International Heritage Scholarship Community: Secondary beneficiary (moderate/constrained) — gains research access and prestige; constrained by requirement to affirm universal heritage narrative
 *   - Secular Turkish State Administration: Primary institutional beneficiary and enforcer (institutional/constrained) — benefits from soft power and international legitimacy; constrained by enforcement costs and diplomatic management
 *   - Islamic Worship Community: Primary victim (powerless/trapped) — excluded from preferred use; no exit option
 *   - Orthodox Restitution Claimants: Secondary victim (powerless/trapped) — excluded by same mechanism; structural barriers to political remedy
 *   - UNESCO and International Heritage Institutions: Scaffolding actors (organized/constrained) — temporary institutional stability; facing renegotiation pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.52).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.48).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia Universal Heritage Legitimacy (Museum Administration Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, 'e3037982-cc7d-4970-ad58-1137c75858c6').
narrative_ontology:cs_kernel_codification('e3037982-cc7d-4970-ad58-1137c75858c6', implicit).
narrative_ontology:cs_authority_grounding('e3037982-cc7d-4970-ad58-1137c75858c6', extraction).
narrative_ontology:cs_interpretation_layer_present('e3037982-cc7d-4970-ad58-1137c75858c6').
narrative_ontology:cs_reading_relation('e3037982-cc7d-4970-ad58-1137c75858c6', hagia_sophia_substrate__islamic_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3037982-cc7d-4970-ad58-1137c75858c6', hagia_sophia_substrate__orthodox_restitution_reading, coexists_with).
narrative_ontology:cs_axiom('e3037982-cc7d-4970-ad58-1137c75858c6', foundational, religious_sites_transcend_sectarian_claims).
narrative_ontology:cs_axiom_status(religious_sites_transcend_sectarian_claims, holdable).
narrative_ontology:cs_axiom_grounding('e3037982-cc7d-4970-ad58-1137c75858c6', religious_sites_transcend_sectarian_claims, deontological).
narrative_ontology:cs_axiom('e3037982-cc7d-4970-ad58-1137c75858c6', foundational, secular_governance_preserves_multiplicity).
narrative_ontology:cs_axiom_status(secular_governance_preserves_multiplicity, holdable).
narrative_ontology:cs_axiom_grounding('e3037982-cc7d-4970-ad58-1137c75858c6', secular_governance_preserves_multiplicity, instrumental).
narrative_ontology:cs_reference_frame('e3037982-cc7d-4970-ad58-1137c75858c6', universal_secular_heritage_authority).
narrative_ontology:cs_drift_state('e3037982-cc7d-4970-ad58-1137c75858c6', contemporary_pluralism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e3037982-cc7d-4970-ad58-1137c75858c6', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secular_turkish_elites).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, international_heritage_scholars).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_claims).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, orthodox_restitution_claims).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, local_religious_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISLAMIC WORSHIP COMMUNITY (SNARE) — Structurally excluded from their preferred use of the building. No exit option: the structure is fixed, the classification is enforced by law and armed security, and alternative worship sites cannot substitute for the historical and architectural significance. The suppression mechanism is formalized in the museum statute and daily enforcement through restricted access.
constraint_indexing:constraint_classification(hagia_sophia_substrate__universal_heritage_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORTHODOX RESTITUTION CLAIMANTS (SNARE) — Face structural exclusion via the same mechanism as Islamic claimants. The museum reading forecloses their restitution claim by naturalizing 'universal heritage' as incompatible with sectarian religious control. Legal barriers and state enforcement prevent exit.
constraint_indexing:constraint_classification(hagia_sophia_substrate__universal_heritage_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: GLOBAL TOURISM SECTOR (ROPE) — Benefits from the universal heritage framing through stable access, predictable revenue, and absence of sectarian disruption. The constraint functions as pure coordination: the museum structure enables global audiences to experience the building and funds preservation through tourism revenue. Net beneficiary with substantial arbitrage options — can redirect investment elsewhere but benefits from the Hagia Sophia specifically.
constraint_indexing:constraint_classification(hagia_sophia_substrate__universal_heritage_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SECULAR TURKISH STATE ADMINISTRATION (TANGLED ROPE) — Primary beneficiary but also faces constraints. Benefits from the universal heritage framing through international legitimacy, soft power, and preservation funding. The coordination function is real: maintaining the building in good condition requires substantial resources and expertise that the universal heritage network provides. But the state also faces suppression costs: maintaining the secular framing requires active enforcement against Islamic and Orthodox claims, diplomatic management of religious communities, and management of domestic political dissent. Exit is constrained—abandoning the universal heritage claim would trigger complex renegotiations with international heritage bodies and regional religious powers.
constraint_indexing:constraint_classification(hagia_sophia_substrate__universal_heritage_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL HERITAGE SCHOLARSHIP COMMUNITY (TANGLED ROPE) — Derives significant benefits from the universal heritage framing: unimpeded research access, international funding, prestige of working on a UNESCO World Heritage site. Also faces extraction: the scholarly narrative must affirm the universal heritage claim to maintain legitimacy within the international community, creating a feedback loop that suppresses alternative scholarly framings. Exit is costly but possible—scholars can work on other sites or adopt alternative framings, but this carries professional penalties.
constraint_indexing:constraint_classification(hagia_sophia_substrate__universal_heritage_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: UNESCO AND INTERNATIONAL HERITAGE INSTITUTIONS (SCAFFOLD) — The universal heritage reading has a built-in sunset: as religious pluralism norms strengthen globally and secular governance models face legitimacy challenges, the pure-museum solution becomes increasingly difficult to sustain. The institutional machinery (UNESCO protocols, conservation standards, international governance frameworks) creates temporary stability but requires active reaffirmation. Rising pressure from Islamic advocacy and Orthodox restitution claims creates institutional pressure toward renegotiation. The sunset is generational, not immediate, but the institutional structure is increasingly performative.
constraint_indexing:constraint_classification(hagia_sophia_substrate__universal_heritage_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: MUSEUM ADMINISTRATION RITUAL (PITON) — At civilizational timescale, the museum framing is revealed as substantially performative. The daily practice of treating a 6th-century religious building as a secular heritage asset requires constant theatrical maintenance: guards preventing worship, signage declaring universality, scholarship narratives suppressing religious use-history, diplomatic language managing international religious pressures. The core function—preserving the physical structure—could operate under Islamic administration, Orthodox administration, or shared governance with equivalent or superior maintenance outcomes. The universal museum framing persists through institutional inertia and ideological commitment to secular modernity, not because it uniquely solves the preservation problem.
constraint_indexing:constraint_classification(hagia_sophia_substrate__universal_heritage_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a deep civilizational perspective, one could argue that transcendent religious sites inevitably face incommensurable claims and that secular governance (treating them as heritage rather than as active worship spaces) represents the only structurally stable allocation. This naturalization reads the museum solution as an inevitable response to the logical impossibility of simultaneous Christian and Islamic sovereignty. However, this perspective constitutes a false summit: the structure (resource allocation, legitimacy claims, enforcement) is contingent, not necessary.
constraint_indexing:constraint_classification(hagia_sophia_substrate__universal_heritage_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hagia_sophia_substrate__universal_heritage_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hagia_sophia_substrate__universal_heritage_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, TR),
    TR >= 0.70.

:- end_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The universal heritage reading generates substantial benefits for the tourism sector, international scholars, and secular Turkish elites through cultural prestige, funding access, and ideological alignment with secular modernity narratives. The extraction is real—Islamic worship claims are suppressed, Orthodox restitution is marginalized, and local religious communities bear the cost of exclusion. However, the extraction is not maximal (not 0.70+) because genuine coordination benefits exist: the building is preserved through the heritage framework, international conservation expertise is mobilized, and the structure's cultural significance is protected. The constraint would collapse into pure snare (0.70+) if alternative governance models (Islamic administration, Orthodox administration) could not maintain equivalent or superior preservation outcomes; the existence of viable alternatives means the universal heritage reading's claim to provide the 'only' solution is contestable, moderating the extractiveness to tangled rope range. Suppression (0.48): Moderate. Active enforcement mechanisms exist (guards, legal statutes, security barriers, credential requirements for worship access) but are not maximally coercive—they operate through administrative exclusion and normalization rather than through direct violence or imminent threat. The suppression is structurally stable through law and institution rather than through constant visible force. Theater ratio (0.64): Moderate-high. The secular museum framing requires substantial performative work: the daily ritual of treating an active religious building as a passive heritage asset, the scholarly narratives that bracket religious history, the curatorial decisions that emphasize universality and minimize sectarian distinctiveness, the diplomatic language managing religious pressures. The rising trajectory (0.48→0.72) reflects that the secular framing's performative content increases as its legitimacy base weakens—more theatrical work is required to maintain the reading as alternative legitimacy claims strengthen.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural property (a 1,500-year-old building with multiple religious and cultural associations) produces radically different classifications depending on observer position. The Islamic worship community sees pure extraction (snare)—they are excluded with no exit option. The Orthodox restitution claimants see the same snare from their position. The global tourism sector sees pure coordination (rope)—the constraint enables their activity and protects their interests. The secular Turkish state sees mixed coordination-extraction (tangled rope)—they benefit but also face rising enforcement costs. International scholars see tangled rope with constrained exit—benefits from research access coexist with extraction (requirement to affirm the universal heritage narrative). UNESCO institutions see a temporarily stable but increasingly pressured framework (scaffold)—the institutional structure has a built-in sunset. The museum administration ritual appears as performative theater at civilizational scale (piton)—the core function (preservation) could operate under different governance but is maintained through institutional inertia and ideological commitment. The analytical observer risks naturalizing the museum solution as an inevitable law of religious site governance (mountain)—but this constitutes a false summit revealing contingent institutional choices as natural necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to this specific constraint. The Islamic worship community has high d (near 1.0)—they are primary victims with no exit options. The Orthodox restitution claimants have similarly high d. The tourism sector has low d (near 0.0)—they are primary beneficiaries with arbitrage options. The secular Turkish state has moderate d (0.40–0.55)—they are net beneficiaries but face suppression costs. International scholars have moderate d (0.50–0.60)—mixed benefits and constraints. UNESCO institutions have low-to-moderate d (0.35–0.50)—they benefit from the institutional framework but constrained by renegotiation pressures. The engine derives d from the beneficiary/victim declarations and exit options: victims + trapped → high d → high f(d) → high experienced extraction. Beneficiaries + arbitrage → low d → negative f(d) → low or negative experienced extraction. The perspectival gap arises because d values span the full range [0.0, 1.0], producing classification variation from rope (low d beneficiaries) to snare (high d victims).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_heritage_authenticity,
    'Is the ''universal heritage'' classification of Hagia Sophia an authentic description of the building''s actual historical and cultural status, or a constructed framing that naturalizes political choices?',
    'Historical analysis of how the building functioned across its 6th-century Orthodox, 15th-21st century Islamic, and 20th-century museum phases. Assessment of whether ''universal'' characterization captures genuine multiplicity or imposes homogenizing frame that suppresses sectarian distinctiveness.',
    'If authentic: universal heritage reading is genuine rope/coordinate solution. If constructed: reading is legitimation of extraction (snare/tangled rope). Determines whether the false summit signature fires and reclassifies the analytical mountain to snare/tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_heritage_authenticity, conceptual, 'Whether universal heritage framing is authentic or constructed legitimation').

omega_variable(
    axis_of_contested_readings,
    'What is the structural relationship between the universal heritage reading and the sibling readings (islamic_sovereignty_reading, orthodox_restitution_reading)? Do they foreclose each other, coexist in different communities, or create structural pressures without logical foreclosure?',
    'Examination of contemporary discourse: (1) Can the same institution (Turkish state) simultaneously hold the universal heritage reading and negotiate in good faith with Orthodox restitution advocates? (2) Can international heritage bodies acknowledge the legitimacy of Islamic worship claims while maintaining museum governance? (3) Have any institutional frameworks successfully held multiple readings without foreclosure?',
    'If foreclose relation: only one reading can be institutionally maintained; others must be actively suppressed. If coexist relation: multiple readings could theoretically be held; the choice of universal heritage is contingent. If influence relation: the universal reading creates structural pressure that constrains but does not eliminate alternatives. The relation determines the engine''s reclassification potential and the omega variables needed in sibling stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axis_of_contested_readings, conceptual, 'Structural relationship (foreclosure, coexistence, or influence) between universal heritage reading and sibling readings').

omega_variable(
    secular_governance_sustainability,
    'Is the secular technocratic framing of cultural heritage a stable long-term institutional solution for religiously contested sites, or is it structurally unstable and approaching renegotiation pressure?',
    'Longitudinal tracking of: (1) frequency and intensity of religious restitution claims; (2) international institutional support for secular governance (UNESCO mandate erosion or strengthening); (3) domestic political pressure in Turkey for religious uses; (4) emergence of successful alternative governance models (shared administration, time-sharing worship, hybrid religious-secular governance) in other religiously contested sites.',
    'If stable: scaffold perspective is misclassified; sunset is indefinite or non-existent. If unstable: scaffold classification confirmed; renegotiation pressure is rising. Determines whether the temporal measurements show rising enforcement costs (suppression_requirement increasing over time).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_governance_sustainability, empirical, 'Long-term sustainability of secular technocratic governance for contested religious sites').

omega_variable(
    kernel_boundary_ambiguity,
    'Is the kernel being contested in the hagia_sophia_substrate family one of political legitimacy (who has the right to decide the building''s use), or one of metaphysical status (what the building IS — a mosque, a church, a museum, a hybrid), or both?',
    'Analysis of rhetoric in each reading: does the universal heritage reading claim (a) that no single religious community should have exclusive use (legitimacy argument), or (b) that the building has transcended its religious identity and become something universally shared (metaphysical argument), or (c) both?',
    'If metaphysical framing dominates: the universal reading genuinely forecloses religious readings (they cannot coexist because the building''s fundamental nature is incompatible). If legitimacy framing dominates: coexistence or power-based determination is possible. Affects cs_structure.reading_relations classification (forecloses vs coexists_with).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_boundary_ambiguity, conceptual, 'Whether kernel dispute is political legitimacy, metaphysical status, or both').

omega_variable(
    suppression_visibility_asymmetry,
    'Is the suppression of Islamic worship and Orthodox restitution claims enforced visibly (guards, legal exclusions, security barriers) or invisibly (normalization of secular access, scholarly discourse that marginalizes religious interpretation)?',
    'Ethnographic observation of enforcement mechanisms; analysis of published heritage literature for what religious claims are mentioned vs suppressed; interviews with denied claimants about enforcement experience.',
    'If suppression is primarily visible: it can be publicly contested and potentially renegotiated. If suppression is primarily invisible/normalized: it is more stable but more difficult to contest (appears as natural heritage status rather than policy choice).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_visibility_asymmetry, empirical, 'Visible vs invisible mechanisms of suppressing religious claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagia_univ_tr_t0, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(hagia_univ_tr_t10, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 10, 0.64).
narrative_ontology:measurement(hagia_univ_tr_t25, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 25, 0.72).

% Extraction over time
narrative_ontology:measurement(hagia_univ_be_t0, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hagia_univ_be_t10, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(hagia_univ_be_t25, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 25, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(hagia_univ_su_t0, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hagia_univ_su_t10, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(hagia_univ_su_t25, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 25, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__universal_heritage_reading, 0.12).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% The hagia_sophia_substrate kernel decomposes into three structurally distinct constraint stories, one per reading. Each reading instantiates a different epsilon (universal_heritage~0.52, islamic_sovereignty~0.65, orthodox_restitution~0.58), different beneficiary/victim sets, and different dominant classification types. The readings coexist as competing institutional positions held by different actors; no single reading logically forecloses the others, but each reading creates structural pressures that marginalize alternatives. All three stories link via network.affects_constraints to enable contamination analysis and comparative classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
