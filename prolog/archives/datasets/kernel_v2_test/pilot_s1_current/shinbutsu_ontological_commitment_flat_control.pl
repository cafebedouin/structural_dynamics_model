% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment_flat_control, []).

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
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: shinbutsu_ontological_commitment_flat_control
 *   human_readable: Kami-Buddha Ontological Commitment in Pre-Meiji Japan
 *   domain: religious_studies/japanese_history/ontology
 *
 * SUMMARY:
 *   The shinbutsu syncretism of pre-Meiji Japan presents a constraint that
 *   spans ontology, institutional practice, and lay religious life. The
 *   constraint is whether kami (native Shinto deities, locally embodied, tied
 *   to specific shrines and landscapes) and buddhas (universal Buddhist
 *   figures, transcendent, integrated into a cosmological hierarchy) could be
 *   understood as inhabiting a single coherent metaphysical order, or whether
 *   their apparent integration was an incoherent institutional arrangement
 *   sustained through enforced participation and theological theater. From
 *   the Heian period onward, Japanese religious institutions developed the
 *   Suijaku theory (honji suijaku) to frame kami as manifestations of buddhas
 *   in Japan — a theological move that transformed what could have been a
 *   competitive or conflictual relationship into a hierarchically integrated
 *   system. However, the mechanism that made this work was not principally
 *   logical coherence but institutional flexibility: shrines and temples
 *   maintained parallel authority structures, dual practice was normalized as
 *   unremarkable, and both lay practitioners and elites learned not to ask
 *   foundational questions about whether the metaphysics actually coherent.
 *   By the late Edo period, the system had become substantially performative
 *   — maintained through ritual formality and institutional authority
 *   assertion rather than active coordination work. The Meiji government's
 *   deliberate shinbutsu bunri (separation of kami and buddhas into distinct
 *   institutional domains, 1868+) revealed the atrophied function almost
 *   immediately: the separation was accomplished with relatively little
 *   resistance, suggesting the system had outlived its coordination mandate
 *   and persisted primarily through institutional inertia. This constraint
 *   exhibits all six DR types depending on which actor's perspective is
 *   privileged.
 *
 * KEY AGENTS:
 *   - Village Practitioners: Primary victim (powerless/trapped) — bound by dual obligations to shrine and temple without coherent metaphysical framework, bearing suppression costs
 *   - Buddhist Institutional Authority: Primary beneficiary (institutional/arbitrage) — gains integration mechanism, institutional stability, and legitimacy from kami incorporation via Suijaku theory
 *   - Shrine Networks: Secondary beneficiary with constraints (institutional/constrained) — gain cosmological legitimacy but subordinated within Buddhist hierarchy and constrained by institutional interdependence
 *   - Scholarly-Priestly Mediator Class: Tertiary beneficiary (moderate/constrained) — control interpretation of boundaries and accumulate authority as synthesizers, but depend on system incoherence for their mediating function
 *   - Ontological Coherence (abstract): Victim (powerless/trapped) — the constraint requires that foundational questions about kami-buddha relationship remain suppressed
 *   - Meiji Separatist Movement: Organized agent (organized/mobile) — sees the constraint as temporary and chooses institutional sunset via explicit separation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment_flat_control, 0.38).
domain_priors:suppression_score(shinbutsu_ontological_commitment_flat_control, 0.42).
domain_priors:theater_ratio(shinbutsu_ontological_commitment_flat_control, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment_flat_control, extractiveness, 0.38).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment_flat_control, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment_flat_control, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment_flat_control, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment_flat_control, "Kami-Buddha Ontological Commitment in Pre-Meiji Japan").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment_flat_control, "religious_studies/japanese_history/ontology").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(shinbutsu_ontological_commitment_flat_control, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment_flat_control, institutional_buddhism).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment_flat_control, established_shrine_networks).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment_flat_control, ontological_coherence).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment_flat_control, lay_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VILLAGE PRACTITIONER (SNARE) — Trapped in dual obligation to both shrine and temple without coherent metaphysical framework. Bears the cost of maintenance obligations, ritual duplications, and doctrinal contradictions that institutions refuse to resolve. No exit: religious practice is embedded in social identity and economic participation. Maximum suppression — the constraint naturalizes itself as 'just how things are done' with no alternatives visible from inside.
constraint_indexing:constraint_classification(shinbutsu_ontological_commitment_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SHRINE NETWORK (TANGLED ROPE) — Benefits from Buddhist theological framework that elevates kami to bodhisattva status (Suijaku theory), gaining cosmological legitimacy and institutional stability. Simultaneously bears costs: kami remain subordinate within Buddhist hierarchy, shrine autonomy is constrained by Buddhist institutional requirements, and the metaphysical incoherence creates ongoing negotiations. Active enforcement required to maintain the boundary: periodic recalibrations of kami-buddha relationships, institutional protocols for ritual jurisdiction, theological reframing of conflicts. Exit constrained by deeply embedded institutional interdependence.
constraint_indexing:constraint_classification(shinbutsu_ontological_commitment_flat_control, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BUDDHIST INSTITUTIONAL AUTHORITY (ROPE) — Experiences the constraint as coordination mechanism. Buddhist institutions benefit from institutional arrangement: kami incorporation (via Suijaku theory, honji suijaku) provides integration pathway for native religious structures, reduces resistance to Buddhist expansion, maintains institutional flexibility. Exit options are genuinely available — could define kami as demonic obstacles (as some Buddhist schools did) but choose the coordinative path instead. The arrangement solves a real coordination problem: integrating two major religious traditions without schism. Net beneficiary with agency.
constraint_indexing:constraint_classification(shinbutsu_ontological_commitment_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SCHOLARLY-PRIESTLY MEDIATOR CLASS (TANGLED ROPE) — Buddhist scholars, syncretist theologians, shrine priests with Buddhist training. Benefit from their position as mediators: control interpretation of doctrinal boundaries, manage institutional negotiations, accumulate cultural authority as synthesizers of two systems. Simultaneously constrained: their legitimacy depends on maintaining the incoherent system (if coherence emerged, their mediating function disappears); they cannot exit into pure Buddhism or pure Shinto without losing status. Active enforcement: must continuously reframe contradictions as compatible, produce theological justifications for institutional decisions, manage lay expectations.
constraint_indexing:constraint_classification(shinbutsu_ontological_commitment_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MEIJI-ERA INSTITUTIONAL MEMORY (PITON) — By late Edo/early Meiji, the shinbutsu syncretism had become substantially performative. The original coordination function (integrating two major traditions) had atrophied into institutional inertia: temples housed kami without coherent justification, rituals continued through habit rather than theological coherence, the system persisted through theater (ritual formality, institutional authority assertion) rather than functional integration. Theater ratio high because the constraint had outlived its mandate: Meiji government's deliberate shinbutsu bunri (separation) revealed the atrophied function almost immediately, suggesting the system was maintained more through institutional momentum than active coordination.
constraint_indexing:constraint_classification(shinbutsu_ontological_commitment_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL INCOHERENCE (MOUNTAIN) — From a formal logical perspective, the constraint appears as an immutable property of the conceptual substrate: kami (native, local, embodied in shrines) and buddhas (universal, transcendent, integrated into Buddhist cosmology) occupy incommensurable ontological categories. No institutional arrangement can make both simultaneously true without contradiction. The appearance of coherence is theatrical — a negotiated incoherence sustained by refusing to ask the foundational questions. However, this perspective risks naturalizing what is actually a contingent institutional choice: both Buddhist institutions and village practitioners COULD have rejected the syncretism at various points; instead they maintained it through active institutional work.
constraint_indexing:constraint_classification(shinbutsu_ontological_commitment_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: MEIJI SEPARATIST MOVEMENT (SCAFFOLD) — Organized actors (state ideologues, Shinto revivalists, Buddhist reformers) saw the shinbutsu syncretism as a temporary coordination mechanism that had served its purpose and now needed replacement. The Meiji shinbutsu bunri (1868+) was a deliberate institutional sunset: replace the incoherent fusion with explicit separation, returning kami to pure Shinto institutional control and buddhas to Buddhist institutional control. This perspective sees the constraint as temporary precisely because new institutional pathways existed (Meiji state capacity, ideological clarity, institutional infrastructure for separate domains). Sunset clause evident in historical record: deliberate policy choice to terminate the arrangement.
constraint_indexing:constraint_classification(shinbutsu_ontological_commitment_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shinbutsu_ontological_commitment_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shinbutsu_ontological_commitment_flat_control, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(shinbutsu_ontological_commitment_flat_control, TR),
    TR >= 0.70.

:- end_tests(shinbutsu_ontological_commitment_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint does extract from lay practitioners (obligation to maintain practices they may not fully believe in, resource costs of dual participation) and from any scholar or priest attempting genuine metaphysical coherence (must suppress logical inconsistencies). However, the extraction is not severe because many participants — particularly institutional beneficiaries — experience the arrangement as genuine coordination rather than imposition. The moderate value reflects that the constraint benefits multiple parties (Buddhist institutions, shrine networks, mediator class) while imposing costs on others (lay practitioners, coherence itself), making it tangled rather than purely extractive. Theater ratio (0.65): Moderately high. The Suijaku framework provides sophisticated theological cover for what is fundamentally an institutional arrangement based on pragmatic tolerance rather than logical necessity. By the late Edo period, the ritual formality and authority assertion required to maintain the system exceed the actual functional integration achieved — the system persists because institutions know how to perform it, not because it solves active coordination problems. The upward trajectory (0.40 → 0.65 over 500 years) suggests that as the original coordination function stabilized, the maintenance effort became increasingly performative. Suppression (0.42): Moderate-high. The constraint suppresses foundational questions about kami-buddha metaphysics at multiple levels: institutional policy discourages theological disputation, lay practitioners are expected to participate without explanation, the category system itself (native/universal, embodied/transcendent) naturalizes the contradiction as inevitable rather than chosen. Suppression is not total because scholars exist who explicitly engage with the incoherence (producing increasingly elaborate theological justifications), and the Meiji separatists demonstrate that the constraint can be dismantled, suggesting suppression is institutional rather than inevitable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The village practitioner sees snare — trapped in obligation, bearing suppression costs, experiencing the constraint as inescapable because it is embedded in social and religious identity. Buddhist institutional authority sees rope — a genuine coordination mechanism solving the real problem of integrating two major traditions, with clear net benefits and meaningful alternatives available if the benefit/cost calculus changed. The shrine network sees tangled_rope — they benefit from the arrangement (kami gain cosmological legitimacy) but are also constrained (subordination within Buddhist hierarchy, dependence on Buddhist institutions for their own cosmological standing). The mediator class sees tangled_rope from a different angle — they benefit from their mediating role but are locked into maintaining the system's incoherence (coherence would eliminate their function). The late-Edo institutional perspective sees piton — the original coordination function has atrophied, the system is maintained through ritual formality and institutional inertia, and the appearance of harmony masks that active enforcement is now required to prevent the system from fragmenting. The analytical observer risks seeing mountain — ontological incoherence between kami and buddhas as a logical impossibility that institutions must manage theatrically. However, the historical record suggests the analytical observer's mountain is a false summit: the apparent incoherence is actually an institutional choice that could have been made differently (explicit conflict, hierarchical subordination, or genuine synthesis) and was deliberately unmade at the Meiji transition.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) represents each agent's structural position within the extraction flow. Buddhist institutional authority has d near 0.0 (full beneficiary): they gain institutional stability and integration mechanism without bearing suppression costs; they have arbitrage-level exit options (could define kami as obstacles, could ignore native traditions, could explicitly subordinate shrines — choices are available). Village practitioners have d near 1.0 (full target): they bear suppression costs and obligation without equivalent benefit; they have trapped-level exit options (religious participation is embedded in social identity and economic life). The shrine network has d moderate-to-high (0.55-0.65): they benefit from cosmological legitimacy but are constrained by subordination and institutional interdependence; their exit options are constrained (leaving Buddhism entirely would isolate shrines from the broader religious system and institutional resources). The mediator class has d moderate (0.45-0.55): they benefit from their position but depend on system incoherence for their function; exit options are constrained (moving to pure Buddhism or pure Shinto would eliminate their unique role). The engine computes effective extraction (χ) by modulating d through power level and scope: a high-d agent with low power experiences high χ (village practitioner); a low-d agent with high power experiences low or negative χ (Buddhist institutions); a moderate-d agent with constrained exit experiences moderate χ (shrine networks, mediators).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to integrate two major religious traditions and provide a cosmological framework within which both kami and buddhas could be honored without violent institutional conflict. This mandate was clearly live and actively pursued from the Heian period through the early Edo period. However, by the late Edo period (visible in theater_ratio trend), the mandate had outlived its function: the integration was complete enough that Meiji separatists could deliberately unmake it (shinbutsu bunri) with minimal lay resistance, suggesting the coordination function had atrophied into institutional inertia. The constraint exhibits mandatrophy characteristics: it persists in degraded form (high theater_ratio, performative ritual maintenance) after its original coordination function is no longer necessary. However, mandatrophy_resolved is false because the Meiji bunri was a historical event, not an internal resolution: the constraint was externally terminated rather than resolving its own obsolescence. The modern analytical problem is that the Meiji government's separatist ideology has obscured the historical reality of the pre-Meiji constraint: by declaring the syncretism a 'false' or 'corrupted' state that needed to be 'corrected' to pure Shinto, Meiji ideologues performed a historiography that naturalized separation as the default state, making it harder to see that the integrated system was a real coordination achievement that persisted for centuries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_institutional_status,
    'Is the constraint fundamentally about the ontological properties of kami and buddhas themselves, or about institutional arrangements and lay expectations that happen to rest on ontological claims?',
    'Examine historical sources distinguishing between (a) explicit philosophical/theological claims about kami-buddha identity, (b) institutional policy decisions managing shrine-temple boundaries, (c) lay practitioner behavior and expressed understanding. Measure the gap between elite theological positions and actual practice.',
    'If fundamentally ontological: the constraint persists as logical incoherence that institutions maintain theatrically (mountain classification supported). If fundamentally institutional: the constraint is a real coordination mechanism that generates beneficiaries and victims through institutional choices (tangled_rope classification supported). The impact is perspectival: from inside the system, the incoherence feels inevitable; from outside, it appears chosen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_institutional_status, conceptual, 'Ontological vs institutional nature of the shinbutsu syncretism').

omega_variable(
    suijaku_theory_coherence,
    'Did the Suijaku theory (honji suijaku — original essence/manifest traces) genuinely resolve the ontological tension, or was it a sophisticated theological gesture masking unresolved contradiction?',
    'Close textual analysis of Suijaku justifications; examine where the theory breaks down or requires additional ad-hoc modifications; test whether the framework consistently handles edge cases (where kami and buddha claims directly conflict, where institutional boundaries are contested). Document instances where Suijaku logic was abandoned for pragmatic institutional decisions.',
    'If Suijaku genuinely coherent: the constraint should classify as rope (coordination mechanism) from more perspectives. If Suijaku is sophisticated theater: the constraint is better classified as tangled_rope with high theater_ratio, because the theoretical apparatus exists primarily to maintain institutional flexibility rather than resolve logical tensions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suijaku_theory_coherence, empirical, 'Whether Suijaku theory provided genuine coherence or masked incoherence').

omega_variable(
    lay_practitioner_agency,
    'To what extent were lay practitioners genuine believers in the unified cosmology versus pragmatists performing dual practice without internal contradiction because they weren''t asked to articulate the metaphysics?',
    'Evidence from lay sources (diaries, confessional documents, folk practice records) distinguishing between (a) explicit statements that kami and buddhas are metaphysically unified, (b) statements that they are separate but both worth honoring, (c) pragmatic participation without ontological claims. Measure the prevalence of theological inconsistency in lay sources.',
    'If lay practitioners genuinely believed in unified cosmology: the constraint operates as rope from their perspective — they see real coordination. If lay practitioners were pragmatists: they bore suppression costs (obligation to maintain contradictory practices) without intellectual buy-in, making the snare classification more accurate. This shifts the victim/beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_practitioner_agency, empirical, 'Lay practitioner ontological commitment versus pragmatic participation').

omega_variable(
    institutional_flexibility_necessity,
    'Was the incoherent tolerance of kami-buddha fusion structurally necessary for pre-Meiji Japan (i.e., the only way to integrate two major traditions without violent conflict), or was it chosen because it provided benefits to elite institutions that coherent alternatives would not?',
    'Comparative analysis: Did other societies integrating multiple religious traditions develop coherent syntheses (e.g., Islamic-Hindu syncretism in South Asia, Christian-indigenous synthesis in Latin America)? If yes, document what institutional mechanisms enabled coherence. Examine internal evidence for periods when coherence was debated and rejected by elites.',
    'If structurally necessary: the constraint operates closer to mountain status — institutional arrangements reflect logical necessity rather than choice. If chosen for elite benefit: extraction component is higher, and the tangled_rope classification emphasizes how institutional flexibility benefited organized actors at the cost of lay coherence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_flexibility_necessity, conceptual, 'Whether the incoherent syncretism was institutionally necessary or chosen for elite benefit').

omega_variable(
    meiji_bunri_mandatrophy,
    'Did the Meiji shinbutsu bunri represent genuine institutional sunset (the constraint''s coordination function was exhausted and new pathways became viable) or institutional capture (the constraint was terminated because it benefited elites who lost power in the Meiji transition)?',
    'Compare institutional dynamics pre-bunri (how much active enforcement was required to maintain the syncretism, how stable was the arrangement, what new institutional alternatives were emerging) with post-bunri (what actual institutional changes resulted, did the constraint persist in degraded form in rural areas, how complete was the separation). Document whether the bunri solved coordination problems or created new ones.',
    'If genuine sunset: the scaffold perspective is correct — the constraint was a temporary coordination mechanism that accomplished its purpose and was rationally replaced. If capture: the bunri is best understood as institutional violence — an arrangement serving lay practitioners and lower-level priests was terminated because Meiji elites benefited from Shinto nationalism. This affects whether mandatrophy_resolved should be true (sunset) or the constraint should be reclassified post-bunri as snare (violent termination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_bunri_mandatrophy, empirical, 'Meiji bunri as sunset versus institutional capture').

omega_variable(
    performance_vs_belief_gap,
    'What proportion of the observed stability in the shinbutsu syncretism was due to genuine shared ontological commitment versus institutional performance (ritual formality, authority assertion, enforcement of participation)?',
    'Examine periods of explicit conflict between shrine and temple; measure frequency of doctrinal reframing required to resolve institutional disputes; compare stability in elite-controlled urban centers versus village-level practices; track theater_ratio over time in institutional records (rhetoric about harmony versus actual institutional disputes).',
    'If commitment-driven: the constraint shows lower suppression and more genuine coordination (rope perspectives are more accurate). If performance-driven: suppression and theater_ratio are underestimated, the piton classification becomes more prominent, and the constraint is better understood as inertial rather than functionally integrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_belief_gap, empirical, 'Balance between ontological commitment and institutional performance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment_flat_control, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_tr_t0, shinbutsu_ontological_commitment_flat_control, theater_ratio, 0, 0.4).
narrative_ontology:measurement(shinbutsu_tr_t250, shinbutsu_ontological_commitment_flat_control, theater_ratio, 250, 0.52).
narrative_ontology:measurement(shinbutsu_tr_t500, shinbutsu_ontological_commitment_flat_control, theater_ratio, 500, 0.65).

% Extraction over time
narrative_ontology:measurement(shinbutsu_be_t0, shinbutsu_ontological_commitment_flat_control, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(shinbutsu_be_t250, shinbutsu_ontological_commitment_flat_control, base_extractiveness, 250, 0.35).
narrative_ontology:measurement(shinbutsu_be_t500, shinbutsu_ontological_commitment_flat_control, base_extractiveness, 500, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_su_t0, shinbutsu_ontological_commitment_flat_control, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(shinbutsu_su_t250, shinbutsu_ontological_commitment_flat_control, suppression_requirement, 250, 0.4).
narrative_ontology:measurement(shinbutsu_su_t500, shinbutsu_ontological_commitment_flat_control, suppression_requirement, 500, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment_flat_control, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment_flat_control, meiji_religious_nationalism_ideology).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment_flat_control, shrine_institutional_autonomy_meiji_transition).

% DUAL FORMULATION NOTE:
% The shinbutsu syncretism is a single constraint operating at multiple levels: cosmological (whether kami and buddhas are ontologically unified), institutional (whether shrines and temples can share authority and resources), and practical (whether lay practitioners can maintain dual participation without cognitive dissonance). These are not separate constraints with different ε values — they are different dimensions of the same coordination mechanism. The constraint family links downstream to Meiji-era decomposition constraints (how the separatist ideology was constructed, how shrine networks adapted to disestablishment, how lay practice changed post-bunri), but these downstream constraints have their own distinct ε values reflecting the different institutional dynamics of the Meiji period.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
