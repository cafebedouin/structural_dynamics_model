% ============================================================================
% CONSTRAINT STORY: historiographical_legitimation_struggle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historiographical_legitimation_struggle, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: historiographical_legitimation_struggle
 *   human_readable: Historiographical Legitimation Struggle: Zionism as National Liberation vs Settler-Colonialism
 *   domain: political_history/nationalism_studies/settler_colonial_studies
 *
 * SUMMARY:
 *   The historiographical legitimation struggle over Zionism's classification
 *   as national liberation movement versus settler-colonial project operates
 *   as a tangled rope: it coordinates scholarly attention and comparative
 *   analysis while extracting from epistemic integrity through archival
 *   suppression, career penalties for dissenting scholars, and political
 *   instrumentalization of historical inquiry. The constraint emerged with
 *   the founding of Israel (1948) and intensified through three phases:
 *   initial consensus (1948-1967), fracture after the Six-Day War
 *   (1967-1980s), and the New Historians' archival challenge (1980s-present).
 *   The theater ratio has increased as the framing contest has become more
 *   performative — ritualized denunciations, loyalty oaths, and funding
 *   restrictions replace substantive engagement with archival evidence. The
 *   constraint is downstream of two structural realities: the demographic
 *   elimination imperative (mountain — the zero-sum logic of demographic
 *   competition in the territory) and transfer as policy consensus (tangled
 *   rope — the documented preference for population transfer among Zionist
 *   leadership). These upstream constraints provide empirical anchors that
 *   the historiographical struggle must either incorporate or suppress,
 *   raising the extraction cost of maintaining competing narratives.
 *
 * KEY AGENTS:
 *   - Historical Clarity: Primary victim (powerless/trapped) — epistemic commons consumed by political instrumentalization; cannot exit or self-correct
 *   - Displaced Populations: Primary victim (powerless/identity_locked) — lived experience subordinated to competing legitimation projects; identity constituted through displacement itself
 *   - Dissenting Scholars: Secondary victim (moderate/constrained) — New Historians, post-Zionist academics face career penalties but benefit from scholarly ecosystem; mixed extraction-coordination
 *   - State Legitimation Apparatus: Primary beneficiary (institutional/arbitrage) — controls educational curricula, museums, commemorations; can shift between frameworks strategically
 *   - Academic Gatekeepers: Mixed position (institutional/constrained) — journal editors, tenure committees navigate scholarly rigor vs political pressure; both benefit from and are constrained by the contest
 *   - Competing Political Coalitions: Primary beneficiary (organized/mobile) — pro-Israel advocacy, Palestinian solidarity, anti-colonial movements mobilize historiography for contemporary goals; low extraction
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both coordination (comparative analysis enabled) and extraction (evidence suppressed, dissent punished)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historiographical_legitimation_struggle, 0.48).
domain_priors:suppression_score(historiographical_legitimation_struggle, 0.62).
domain_priors:theater_ratio(historiographical_legitimation_struggle, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historiographical_legitimation_struggle, extractiveness, 0.48).
narrative_ontology:constraint_metric(historiographical_legitimation_struggle, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(historiographical_legitimation_struggle, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historiographical_legitimation_struggle, tangled_rope).
narrative_ontology:human_readable(historiographical_legitimation_struggle, "Historiographical Legitimation Struggle: Zionism as National Liberation vs Settler-Colonialism").
narrative_ontology:topic_domain(historiographical_legitimation_struggle, "political_history/nationalism_studies/settler_colonial_studies").

domain_priors:requires_active_enforcement(historiographical_legitimation_struggle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historiographical_legitimation_struggle, competing_political_coalitions).
narrative_ontology:constraint_beneficiary(historiographical_legitimation_struggle, academic_gatekeepers).
narrative_ontology:constraint_beneficiary(historiographical_legitimation_struggle, state_legitimation_apparatus).
narrative_ontology:constraint_victim(historiographical_legitimation_struggle, historical_clarity).
narrative_ontology:constraint_victim(historiographical_legitimation_struggle, displaced_populations).
narrative_ontology:constraint_victim(historiographical_legitimation_struggle, dissenting_scholars).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HISTORICAL CLARITY (SNARE) — The epistemic commons cannot exit the framing contest. Each competing narrative suppresses archival evidence and alternative interpretations. Maximum extraction: the capacity for shared historical understanding is consumed by political instrumentalization with no self-correction mechanism.
constraint_indexing:constraint_classification(historiographical_legitimation_struggle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISPLACED POPULATIONS (SNARE) — Structurally mobile in principle (could adopt alternative narratives) but identity-locked: their lived experience and collective memory are constituted through the displacement itself. The historiographical contest determines whether their experience is recognized as ethnic cleansing or reframed as voluntary migration. High extraction: their historical reality is subordinated to competing legitimation projects.
constraint_indexing:constraint_classification(historiographical_legitimation_struggle, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: DISSENTING SCHOLARS (TANGLED ROPE) — New Historians, post-Zionist academics, and critical scholars face career penalties and institutional marginalization but also benefit from the scholarly ecosystem (archival access, academic platforms, international networks). Constrained exit: can publish but face funding cuts, institutional pressure, public vilification. Mixed coordination-extraction: the historiographical contest both enables critical scholarship and punishes it.
constraint_indexing:constraint_classification(historiographical_legitimation_struggle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE LEGITIMATION APPARATUS (ROPE) — Benefits from the framing contest by controlling educational curricula, museum narratives, and official commemorations. Experiences the constraint as coordination: managing competing narratives to maintain domestic consensus and international legitimacy. Arbitrage exit: can shift between frameworks (national liberation, indigenous return, defensive necessity) as strategic context demands.
constraint_indexing:constraint_classification(historiographical_legitimation_struggle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ACADEMIC GATEKEEPERS (TANGLED ROPE) — Journal editors, tenure committees, funding bodies navigate competing pressures: scholarly rigor vs political acceptability, archival evidence vs donor preferences, academic freedom vs institutional reputation. Constrained exit: can enforce standards but face external pressure. Both benefit from (control scholarly discourse) and are constrained by (political instrumentalization of their decisions) the framing contest.
constraint_indexing:constraint_classification(historiographical_legitimation_struggle, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COMPETING POLITICAL COALITIONS (ROPE) — Pro-Israel advocacy groups, Palestinian solidarity movements, anti-colonial coalitions, and nationalist movements all benefit from the historiographical contest by mobilizing it for contemporary political goals. Mobile exit: can shift historical frameworks, emphasize different time periods, or reframe narratives as strategic needs evolve. Low extraction: the contest is a coordination mechanism for political mobilization.
constraint_indexing:constraint_classification(historiographical_legitimation_struggle, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the framing contest exhibits both genuine coordination (enabling comparative analysis of nationalism, settler-colonialism, and decolonization across cases) and asymmetric extraction (suppressing archival evidence, marginalizing dissenting scholarship, instrumentalizing historical inquiry for political legitimation). The constraint coordinates scholarly attention while extracting from epistemic integrity.
constraint_indexing:constraint_classification(historiographical_legitimation_struggle, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historiographical_legitimation_struggle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(historiographical_legitimation_struggle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(historiographical_legitimation_struggle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(historiographical_legitimation_struggle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historiographical_legitimation_struggle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The framing contest extracts from historical clarity (epistemic commons), displaced populations (whose experience is reframed or denied), and dissenting scholars (who face career penalties). But extraction is not maximal — archival work continues, alternative narratives circulate internationally, and the New Historians demonstrated that evidence-based challenges are possible even under institutional pressure. The value reflects genuine coordination function (enabling comparative nationalism studies, decolonization analysis) alongside asymmetric extraction. Suppression (0.62): High. Archival access is restricted, dissenting scholars face funding cuts and public vilification, educational curricula are politically controlled, and international legal frameworks are selectively applied (UN 3379 vs 46/86 reversal). But suppression is not total — the New Historians accessed Israeli State Archives in the 1980s, Palestinian oral histories circulate, and comparative settler-colonial studies proceed in international academia. Theater ratio (0.58): Moderate-high. The framing contest has become increasingly performative: ritualized denunciations of 'delegitimization,' loyalty oaths for academics, funding restrictions tied to narrative compliance, and symbolic UN resolutions that reverse based on geopolitical pressure rather than empirical reassessment. But theater is not total — substantive archival work continues, and the New Historians' findings have not been empirically refuted, only politically marginalized.
 *
 * PERSPECTIVAL GAP:
 *   The state legitimation apparatus sees coordination (Rope) — managing narratives to maintain domestic consensus and international legitimacy is a legitimate governance function. Competing political coalitions also see coordination (Rope) — mobilizing historiography for contemporary political goals is standard advocacy. Dissenting scholars and academic gatekeepers see mixed coordination-extraction (Tangled Rope) — the scholarly ecosystem both enables and punishes critical work. Historical clarity and displaced populations see pure extraction (Snare) — their epistemic and experiential reality is consumed with no self-correction mechanism. The analytical observer sees the tangled rope structure: the constraint coordinates comparative analysis while extracting from epistemic integrity. The perspectival gap reveals that beneficiaries experience the constraint as coordination (managing legitimate political contestation) while victims experience it as extraction (suppression of evidence and lived experience). The gap is not resolvable by better communication — it reflects genuine structural asymmetry in who benefits and who bears costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Historical clarity and displaced populations are victims with no exit — they bear maximum extraction. Displaced populations are identity_locked rather than simply trapped: their collective memory and lived experience are constituted through the displacement itself, making alternative framings not just costly but identity-dissolving. Dissenting scholars are victims with constrained exit — they can publish and access archives but face career penalties, producing moderate extraction. State legitimation apparatus and competing political coalitions are beneficiaries with arbitrage/mobile exit — they benefit from the framing contest and can shift narratives strategically, experiencing low or negative extraction. Academic gatekeepers occupy a mixed position: institutional power but constrained exit, both benefiting from (controlling discourse) and victimized by (political pressure) the contest. The analytical observer sees the full structure: genuine coordination (comparative analysis) and asymmetric extraction (suppression, marginalization).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the historiographical contest is neither pure coordination (Rope) nor pure extraction (Snare) but a tangled rope: it genuinely coordinates scholarly attention to nationalism, settler-colonialism, and decolonization while asymmetrically extracting from historical clarity, displaced populations, and dissenting scholars. The coordination function is real — comparative analysis of Zionism alongside other cases (Algeria, South Africa, Ireland, Rhodesia) has advanced theoretical understanding of nationalism and settler-colonialism. The extraction is also real — archival suppression, career penalties, and political instrumentalization subordinate epistemic integrity to legitimation projects. The tangled rope classification prevents two errors: (1) dismissing the entire contest as pure political theater (which ignores genuine scholarly advances in nationalism studies), and (2) treating it as neutral scholarly debate (which ignores the asymmetric costs borne by dissenting scholars and displaced populations). The upstream constraints (demographic_elimination_imperative as mountain, transfer_as_policy_consensus as tangled rope) provide empirical anchors that raise the cost of maintaining competing narratives — the more archival evidence accumulates, the higher the extraction required to suppress it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framework_commensurability,
    'Are ''national liberation'' and ''settler-colonialism'' mutually exclusive analytical frameworks, or can a single historical process exhibit characteristics of both?',
    'Comparative analysis of decolonization cases (Algeria, South Africa, Rhodesia, Ireland) where national liberation and settler-colonial dynamics coexisted; theoretical work on framework compatibility in postcolonial studies',
    'If mutually exclusive: one narrative is correct and the other is false, raising extractiveness. If compatible: both frameworks capture partial truths, lowering extractiveness and shifting toward coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framework_commensurability, conceptual, 'Whether national liberation and settler-colonialism are mutually exclusive frameworks').

omega_variable(
    archival_suppression_magnitude,
    'What proportion of relevant archival evidence remains classified, destroyed, or inaccessible due to political sensitivity?',
    'Declassification tracking; comparison of Israeli State Archives, British Mandate records, and Palestinian oral history projects; analysis of archival access restrictions and their political determinants',
    'If suppression is high (>60% inaccessible): extractiveness increases, as the framing contest operates in an evidence vacuum. If suppression is low (<30%): coordination function increases, as competing narratives can be adjudicated empirically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(archival_suppression_magnitude, empirical, 'Magnitude of politically-motivated archival suppression').

omega_variable(
    new_historians_impact_trajectory,
    'Did the New Historians'' archival work (1980s-2000s) shift the historiographical consensus, or did it create a parallel discourse with minimal cross-contamination?',
    'Citation analysis of Israeli history textbooks, museum exhibits, and public discourse before and after New Historians'' publications; tracking of which findings were incorporated into mainstream narratives vs marginalized',
    'If consensus shifted: the constraint has a sunset (scaffold-like trajectory). If parallel discourses persist: extraction is stable and the tangled rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(new_historians_impact_trajectory, empirical, 'Whether New Historians shifted consensus or created parallel discourse').

omega_variable(
    un_resolution_reversal_mechanism,
    'Was the reversal of UN Resolution 3379 (Zionism as racism, 1975-1991) driven by empirical reassessment, geopolitical pressure, or framework incommensurability?',
    'Analysis of UN General Assembly debates, voting patterns, and stated rationales for Resolution 46/86 (1991 reversal); comparison with other rescinded UN resolutions',
    'If empirical reassessment: suggests frameworks are adjudicable, lowering extraction. If geopolitical pressure: confirms extraction mechanism (power determines historical framing). If framework incommensurability: suggests conceptual omega rather than empirical one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(un_resolution_reversal_mechanism, empirical, 'Mechanism behind UN Resolution 3379 reversal').

omega_variable(
    transfer_policy_historiographical_impact,
    'Does the documented consensus on population transfer among Zionist leadership (per upstream constraint transfer_as_policy_consensus) settle the settler-colonial framework question, or is transfer policy orthogonal to the framework choice?',
    'Comparative analysis: do other national liberation movements (Irish independence, Algerian FLN, Indian partition) with documented transfer policies get classified as settler-colonial? Theoretical work on whether transfer is definitional for settler-colonialism or a contingent feature.',
    'If transfer is definitional: the upstream constraint resolves this omega, confirming settler-colonial framework and raising extractiveness of the legitimation struggle (one side is empirically wrong). If orthogonal: both frameworks remain viable, maintaining tangled rope status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_policy_historiographical_impact, conceptual, 'Whether transfer policy settles the framework question').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historiographical_legitimation_struggle, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(histleg_theater_1948, historiographical_legitimation_struggle, theater_ratio, 0, 0.35).
narrative_ontology:measurement(histleg_theater_1973, historiographical_legitimation_struggle, theater_ratio, 25, 0.48).
narrative_ontology:measurement(histleg_theater_1988, historiographical_legitimation_struggle, theater_ratio, 40, 0.52).
narrative_ontology:measurement(histleg_theater_2008, historiographical_legitimation_struggle, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(histleg_extract_1948, historiographical_legitimation_struggle, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(histleg_extract_1973, historiographical_legitimation_struggle, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(histleg_extract_1988, historiographical_legitimation_struggle, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(histleg_extract_2008, historiographical_legitimation_struggle, base_extractiveness, 60, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historiographical_legitimation_struggle, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of demographic_elimination_imperative (mountain — zero-sum demographic logic) and transfer_as_policy_consensus (tangled rope — documented transfer preference). The upstream constraints provide empirical anchors: the demographic imperative establishes the structural context, and the transfer consensus provides archival evidence that the historiographical struggle must either incorporate or suppress. The historiographical struggle is a distinct constraint with its own extractiveness (0.48) reflecting the career penalties, archival suppression, and political instrumentalization, separate from the upstream constraints' extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historiographical_legitimation_struggle, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
