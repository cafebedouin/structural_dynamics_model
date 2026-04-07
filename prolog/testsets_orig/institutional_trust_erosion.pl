% ============================================================================
% CONSTRAINT STORY: institutional_trust_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_trust_erosion, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: institutional_trust_erosion
 *   human_readable: Institutional Trust Erosion
 *   domain: political_economy/institutional_degradation
 *
 * SUMMARY:
 *   Institutional trust erosion describes the structural constraint operating
 *   when the legitimacy gap widens between institutions' claimed capacity and
 *   actual performance. This constraint exhibits a hybrid character: it is
 *   simultaneously a genuine coordination problem (institutions must maintain
 *   some baseline legitimacy to function) and an extraction mechanism (agents
 *   who capture institutions benefit from eroded public monitoring and can
 *   extract rents under cover of institutional authority). The constraint
 *   manifests as degradation of public confidence, diminished institutional
 *   compliance, and substitution of performative legitimacy claims for
 *   substantive institutional capacity. The trajectory shows accelerating
 *   erosion: extractiveness rises from 0.32 to 0.58 over the interval, while
 *   theater ratio climbs from 0.35 to 0.68, indicating that institutions
 *   increasingly rely on symbolic legitimacy maintenance (ceremonies, public
 *   relations, aspirational narratives) rather than functional capacity to
 *   justify compliance. This is the signature of institutional pitonization:
 *   the institution persists through inertia and residual borrowed
 *   legitimacy, not through genuine coordination function.
 *
 * KEY AGENTS:
 *   - General Public / Betrayed Citizens: Primary victim (powerless/trapped) — bears extraction costs through diminished public goods, reduced institutional responsiveness, erosion of institutional protection. No exit options within national institutional scope.
 *   - Institutional Professionals (teachers, civil servants, judges, journalists): Secondary victim (moderate/constrained) — experience internal institutional degradation, reduced autonomy, professional authority erosion. Constrained by career sunk costs and professional identity fused with institutions.
 *   - Capturing Networks (rent-seeking coalitions, regulatory capture blocs, entrenched factions): Primary beneficiary (institutional/arbitrage) — extract rents under cover of institutional authority during periods of eroded public monitoring. Can shift between institutions.
 *   - Institutional Actors (government agencies, regulatory bodies, courts): Organizational victim/perpetrator — persist through inertia; maintain legitimacy through performative theater while substantive capacity degrades. Experience themselves as degraded.
 *   - Civil Society Reformers (transparency advocates, anti-corruption movements, competing political forces): Secondary organized actor (organized/constrained) — perceive erosion as resolvable through structural reform with sunset logic. Have strategic agency but face suppression from incumbent networks.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional degradation as inevitable lifecycle outcome rather than contingent institutional failure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_trust_erosion, 0.58).
domain_priors:suppression_score(institutional_trust_erosion, 0.65).
domain_priors:theater_ratio(institutional_trust_erosion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_trust_erosion, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_trust_erosion, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_trust_erosion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_trust_erosion, tangled_rope).
narrative_ontology:human_readable(institutional_trust_erosion, "Institutional Trust Erosion").
narrative_ontology:topic_domain(institutional_trust_erosion, "political_economy/institutional_degradation").

domain_priors:requires_active_enforcement(institutional_trust_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_trust_erosion, extractive_institutional_actors).
narrative_ontology:constraint_beneficiary(institutional_trust_erosion, rent_seeking_networks).
narrative_ontology:constraint_victim(institutional_trust_erosion, general_public).
narrative_ontology:constraint_victim(institutional_trust_erosion, institutional_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BETRAYED CITIZEN (SNARE) — Citizen voter trapped within institutional system they cannot exit. Experiences erosion as coercive extraction: institutions extract legitimacy and compliance while providing diminished public goods. High suppression — no alternative institutions available within national scope. Maximum experienced extractiveness.
constraint_indexing:constraint_classification(institutional_trust_erosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DISILLUSIONED PROFESSIONAL (TANGLED ROPE) — Teacher, civil servant, or journalist embedded in institutions experiencing internal degradation. Constrained by career sunk costs and professional identity. Experiences genuine coordination function (institutions still provide jobs, structure, social role) alongside extraction (declining resources, reduced autonomy, erosion of professional authority). Significant enforcement burden on institutional loyalty.
constraint_indexing:constraint_classification(institutional_trust_erosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPTURING NETWORK (ROPE) — Coalition of rent-seeking institutional actors (regulatory capture networks, corporate influence blocs, entrenched bureaucratic factions) benefits from erosion itself. Frames erosion as mere 'coordination challenges' or 'resource constraints.' Experiences constraint as efficient extraction mechanism. Can arbitrage between institutions and capture domains.
constraint_indexing:constraint_classification(institutional_trust_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DELEGITIMIZED INSTITUTION (PITON) — The institution itself (government agency, central bank, judiciary, military) persists through inertia despite eroded function. Theater ratio 0.68 reflects performative maintenance of legitimacy: ceremonies, public relations, symbolic acts replace substantive institutional capacity. Institutional survival mechanism depends on residual legitimacy borrowed from past performance.
constraint_indexing:constraint_classification(institutional_trust_erosion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM MOVEMENT (SCAFFOLD) — Organized agents (civil society organizations, reform coalitions, transparency initiatives, competing political movements) perceive trust erosion as a temporary degradation with identifiable causes and potential sunset. High suppression during active phase (they face institutional resistance), but the perspective includes sunset logic: restoration of institutional accountability through transparency, competition, or structural reform. Low effective extraction because organized agents have exit strategies and voice capacity.
constraint_indexing:constraint_classification(institutional_trust_erosion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL NATURALIZATION (MOUNTAIN) — From civilizational scope, institutional trust erosion can appear as an immutable law of institutional lifecycles: all institutions eventually become captured, corrupt, or delegitimized; decay is inherent to institutional entropy. This naturalizes what is actually a contingent outcome dependent on institutional design, checks, transparency mechanisms, and power distribution. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(institutional_trust_erosion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_trust_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_trust_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_trust_erosion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_trust_erosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_trust_erosion, TR),
    TR >= 0.70.

:- end_tests(institutional_trust_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, increasing over interval. The constraint exhibits cumulative extraction: rent-seeking networks capture institutional authority and extract rents through regulatory capture, bureaucratic favoritism, and public procurement mechanisms while eroded monitoring capacity reduces enforcement costs. However, extraction is not total because institutions retain residual legitimacy from past performance and continued provision of baseline services. The measurement trajectory from 0.32 to 0.62 reflects that initial erosion enables capture, which then accelerates erosion in a feedback loop. Suppression (0.65): High. Citizens and professionals face significant barriers to institutional exit or reform: geographic exit from national institutions is costly; creating alternative institutions requires coordination capacity that eroded trust makes difficult; voice mechanisms within institutions are captured or performative. Suppression is partly structural (material barriers to exit) and partly internalized (citizens lose confidence that reform is possible). Theater ratio (0.68): High and rising. Institutions increasingly substitute symbolic legitimacy claims for substantive capacity: public consultation ceremonies (no genuine input), transparency reports (selective disclosure), reform initiatives (no implementation), anti-corruption rhetoric (selective enforcement). The trajectory from 0.35 to 0.74 shows classic pitonization: the institution persists through borrowed legitimacy and performative ritual, not through functional coordination.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximum perspectival divergence. The beneficiary (capturing network) sees a functional extraction mechanism — institutions still coordinate public compliance while extraction is invisible. The trapped citizen sees pure predation (Snare) — institutions extract legitimacy and public goods while providing diminished services. The institutional professional sees mixed coordination and extraction (Tangled Rope) — their institution still provides career structure and professional role, but with degrading autonomy and declining resources. The reform movement sees a temporary problem with sunset (Scaffold) — restoration is achievable through transparency, competition, or structural reform. The institution itself sees degradation (Piton) — it experiences its own function as eroded but persists through inertia. The analytical observer risks naturalizing this as inevitable institutional entropy (Mountain) — the false summit that all institutions decay. The perspectival gap reflects the constraint's hybrid character: genuine coordination failure legitimately reduces institutional capacity, but capture networks transform that failure into extraction mechanism. Which classification is 'correct' depends on which mechanism dominates — a key question for the omega variables.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation proceeds from structural beneficiary/victim declarations. Capturing networks (institutional power, arbitrage exit) benefit from erosion and monitoring reduction: they derive low d values (~0.15) yielding negative effective extraction (they extract from the system). General public (powerless, trapped) bear extraction costs from eroded institutional protection and diminished public goods: they derive high d values (~0.95) yielding high f(d) (~1.42), maximum experienced extraction. Institutional professionals (moderate power, constrained exit) occupy intermediate position: career sunk costs and identity fusion reduce exit capacity, but professional qualifications provide some arbitrage capability. Their d values (~0.65) yield moderate f(d) (~1.0), moderate extraction. Reform movements (organized power, constrained exit by incumbent suppression) see medium d (~0.50) yielding enhanced effective extraction through scope scaling — what appears as Rope at local scope becomes higher extraction at national scope where incumbent networks have more concentrated power. The perspectival gap emerges because beneficiaries and victims experience fundamentally different directionalities within the same constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   Institutional trust erosion resolves the mandatrophy by disambiguating the coordination failure from the extraction mechanism. The constraint is NOT a pure coordination problem (Rope would be incorrect) because capturing networks deliberately benefit from eroded monitoring capacity — institutional degradation enables rent extraction that would be visible if institutional function remained strong. The constraint is NOT pure extraction (Snare would be incomplete) because institutions retain genuine coordination functions: they still provide legal frameworks, public safety, education, healthcare, and other baseline services that citizens cannot easily replicate through alternative coordination mechanisms. Tangled Rope is correct because: (1) institutions coordinate public compliance and provide baseline services (coordination function); (2) capturing networks extract rents under cover of institutional authority (asymmetric extraction); (3) enforcement is active (incumbent networks defend institutional capture against reform). The mandatrophy warning flags why misclassification occurs: institutions can be described as either 'coordination mechanisms that have failed' or 'extraction mechanisms disguised as coordination.' Both descriptions are partially true. Tangled Rope captures both dimensions. The measurement trajectory (rising extractiveness and theater, both pointing toward Snare) indicates that if the constraint persists, the coordination function may continue to degrade, eventually tipping classification toward Snare. The scaffold perspective's sunset logic applies: institutional reform (transparency, competitive governance, power distribution) can restore the coordination function before pure extraction emerges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    erosion_causality_mechanism,
    'Does trust erosion primarily drive institutional capture, or does capture drive trust erosion?',
    'Temporal sequencing analysis: identify whether capture networks form before or after observable trust decline. Case comparison of institutions with similar initial trust levels that diverge in capture outcomes.',
    'If capture-first: the extraction mechanism is deliberate (Snare classification strengthened). If erosion-first: the extraction mechanism may be emergent from degradation (Tangled Rope classification strengthened). Direction determines intervention logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(erosion_causality_mechanism, empirical, 'Whether capture drives erosion or erosion drives capture').

omega_variable(
    irreversibility_threshold,
    'What threshold of trust loss makes institutional restoration structurally impossible versus merely difficult?',
    'Historical case analysis of institutional recovery: identify institutions that recovered from high trust erosion (40%+ public confidence loss) and compare structural interventions required. Establish threshold where recovery requires wholesale institutional replacement rather than reform.',
    'If threshold is high (80%+ confidence loss): scaffold sunset assumptions are realistic (reform is achievable). If threshold is low (30%+ loss): scaffold becomes ineffective (mountain or snare emerges). Determines whether organized reform movements can succeed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irreversibility_threshold, empirical, 'Threshold of irreversibility for institutional trust degradation').

omega_variable(
    suppression_internalization_pathway,
    'Do citizens accept institutional decline through rational cost-benefit analysis, or through internalized resignation that persists even when exit becomes materially possible?',
    'Comparative analysis of trust patterns in emigration-available vs emigration-restricted populations; longitudinal tracking of attitude changes in individuals who gain exit options (international mobility, dual citizenship); defection rates when exit barriers drop.',
    'If rational cost-benefit: suppression is structural (external barriers, material costs). If internalized: suppression includes cognitive lock (identity_locked exit becomes relevant). Internalization suggests piton classification is understated — institutional persistence depends on resignation, not function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_pathway, empirical, 'Whether suppression is structural or internalized resignation').

omega_variable(
    network_heterogeneity_bifurcation,
    'Does institutional trust erosion create bifurcated trust landscapes (high trust in captured institutions, low trust in non-captured ones) or uniform erosion across all institutions?',
    'Comparative trust metrics across different institutional sectors (judicial, legislative, executive, military, private sector); network analysis of information flows and collective belief formation. Identify whether trust patterns cluster by capture exposure.',
    'If bifurcated: multiple constraints are operating (separate stories for captured vs non-captured institutional domains). If uniform: single constraint story is appropriate. Bifurcation suggests constraint decomposition per ε-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_heterogeneity_bifurcation, empirical, 'Whether erosion is uniform across institutions or bifurcated by capture status').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_trust_erosion, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trust_ero_tr_t0, institutional_trust_erosion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(trust_ero_tr_t5, institutional_trust_erosion, theater_ratio, 5, 0.52).
narrative_ontology:measurement(trust_ero_tr_t10, institutional_trust_erosion, theater_ratio, 10, 0.68).
narrative_ontology:measurement(trust_ero_tr_t15, institutional_trust_erosion, theater_ratio, 15, 0.74).

% Extraction over time
narrative_ontology:measurement(trust_ero_be_t0, institutional_trust_erosion, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(trust_ero_be_t5, institutional_trust_erosion, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(trust_ero_be_t10, institutional_trust_erosion, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(trust_ero_be_t15, institutional_trust_erosion, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_trust_erosion, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(institutional_trust_erosion, 0.18).
narrative_ontology:affects_constraint(institutional_trust_erosion, regulatory_capture).
narrative_ontology:affects_constraint(institutional_trust_erosion, public_goods_provision_failure).
narrative_ontology:affects_constraint(institutional_trust_erosion, legitimacy_cycling).

% DUAL FORMULATION NOTE:
% Institutional trust erosion is upstream of specific capture mechanisms (regulatory capture, corruption dynamics) and downstream of institutional legitimacy mechanics. The coordination type 'enforcement_mechanism' reflects that institutions are fundamentally coercive coordination structures; erosion of monitoring capacity directly undermines enforcement function, creating the extraction opportunity. Floor override (0.18) reflects that enforcement mechanisms have inherent costs (verification, sanction, legitimacy maintenance) that should not be miscounted as extractive overhead.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_trust_erosion, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
