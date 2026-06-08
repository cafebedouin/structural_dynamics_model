% ============================================================================
% CONSTRAINT STORY: british_mandate_scaffolding
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_british_mandate_scaffolding, []).

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
 *   constraint_id: british_mandate_scaffolding
 *   human_readable: British Mandate Scaffolding for Zionist State-Building
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   The British Mandate for Palestine (1920-1948) was a League of Nations
 *   trusteeship explicitly designed to facilitate the establishment of a
 *   'Jewish national home' per the 1917 Balfour Declaration. The Mandate
 *   provided legal framework, military protection, and immigration
 *   facilitation that the Zionist movement could not create autonomously in a
 *   region where Jews were 10% of the population in 1920. This constraint is
 *   structurally a scaffold: temporary external support with a built-in
 *   sunset (transition to self-governance), coordinating state-building
 *   infrastructure while extracting from the Arab majority through political
 *   disenfranchisement and demographic engineering. The scaffold's sunset
 *   occurred (British withdrawal 1948), but the structure it built persisted
 *   as the Israeli state, inheriting the Mandate's legal framework,
 *   territorial claims, and institutional apparatus. The constraint exhibits
 *   all six DR types from different structural positions: Palestinian Arabs
 *   experienced a snare (trapped, maximum extraction, coordination story as
 *   cover); Zionist institutions experienced rope (net beneficiaries,
 *   arbitrage exit, genuine coordination); British administration experienced
 *   scaffold (temporary, sunset clause, mobile exit); post-1945 the Mandate
 *   system degraded to piton (atrophied function, performative
 *   administration). The analytical observer sees tangled rope: irreducible
 *   hybridity of genuine coordination function (legal infrastructure,
 *   economic development) and asymmetric extraction (demographic
 *   displacement, political disenfranchisement). Measurements show rising
 *   extractiveness and suppression through 1936 (Arab Revolt), stabilization
 *   1936-45 (revolt suppressed, WWII), then rising theater_ratio 1945-48 as
 *   British maintain administrative ritual while planning withdrawal.
 *
 * KEY AGENTS:
 *   - Palestinian Arabs Under Mandate: Primary victim (powerless/trapped) — bore full cost of demographic displacement, land expropriation, political disenfranchisement; no exit options, no voice in Mandate governance
 *   - Zionist Quasi-State Institutions: Primary beneficiary (institutional/arbitrage) — Jewish Agency, Histadrut, Haganah captured state-building infrastructure, legal privileges, immigration facilitation; could negotiate with British and appeal to international bodies
 *   - British Imperial Administration: Institutional actor (institutional/mobile) — provided temporary scaffolding with sunset clause; withdrew when strategic costs exceeded benefits (1947-48)
 *   - Palestinian Urban Merchant Class: Secondary victim (moderate/constrained) — some economic integration and political voice, but constrained by land market distortion and marginalization; could emigrate at high cost
 *   - League of Nations Mandate System: Institutional framework (institutional/analytical) — degraded to piton post-1945 as legal authority atrophied but performative administration continued
 *   - Regional Self-Determination: Abstract victim (powerless/trapped) — Arab majority's political aspirations subordinated to Mandate's explicit purpose of facilitating Jewish national home
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(british_mandate_scaffolding, 0.68).
domain_priors:suppression_score(british_mandate_scaffolding, 0.82).
domain_priors:theater_ratio(british_mandate_scaffolding, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(british_mandate_scaffolding, extractiveness, 0.68).
narrative_ontology:constraint_metric(british_mandate_scaffolding, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(british_mandate_scaffolding, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(british_mandate_scaffolding, scaffold).
narrative_ontology:human_readable(british_mandate_scaffolding, "British Mandate Scaffolding for Zionist State-Building").
narrative_ontology:topic_domain(british_mandate_scaffolding, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(british_mandate_scaffolding).
narrative_ontology:has_sunset_clause(british_mandate_scaffolding).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(british_mandate_scaffolding, 'b19b6f6e-6107-4368-bee5-6bf7ba915c8d').
narrative_ontology:cs_kernel_codification('b19b6f6e-6107-4368-bee5-6bf7ba915c8d', distributed).
narrative_ontology:cs_authority_grounding('b19b6f6e-6107-4368-bee5-6bf7ba915c8d', lineage).
narrative_ontology:cs_interpretation_layer_present('b19b6f6e-6107-4368-bee5-6bf7ba915c8d').
narrative_ontology:cs_reading_relation('b19b6f6e-6107-4368-bee5-6bf7ba915c8d', british_mandate_scaffolding__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('b19b6f6e-6107-4368-bee5-6bf7ba915c8d', british_mandate_scaffolding__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('b19b6f6e-6107-4368-bee5-6bf7ba915c8d', foundational, indigenous_return_to_ancestral_homeland).
narrative_ontology:cs_axiom_status(indigenous_return_to_ancestral_homeland, holdable).
narrative_ontology:cs_axiom_grounding('b19b6f6e-6107-4368-bee5-6bf7ba915c8d', indigenous_return_to_ancestral_homeland, conventional).
narrative_ontology:cs_axiom('b19b6f6e-6107-4368-bee5-6bf7ba915c8d', foundational, persecution_driven_necessity).
narrative_ontology:cs_axiom_status(persecution_driven_necessity, holdable).
narrative_ontology:cs_axiom_grounding('b19b6f6e-6107-4368-bee5-6bf7ba915c8d', persecution_driven_necessity, empirically_contingent).
narrative_ontology:cs_axiom('b19b6f6e-6107-4368-bee5-6bf7ba915c8d', secondary, self_determination_principle).
narrative_ontology:cs_axiom_status(self_determination_principle, holdable).
narrative_ontology:cs_axiom_grounding('b19b6f6e-6107-4368-bee5-6bf7ba915c8d', self_determination_principle, deontological).
narrative_ontology:cs_reference_frame('b19b6f6e-6107-4368-bee5-6bf7ba915c8d', historical_continuity_with_ancient_kingdoms).
narrative_ontology:cs_drift_state('b19b6f6e-6107-4368-bee5-6bf7ba915c8d', post_1948_state_establishment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b19b6f6e-6107-4368-bee5-6bf7ba915c8d', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(british_mandate_scaffolding, zionist_quasi_state).
narrative_ontology:constraint_beneficiary(british_mandate_scaffolding, british_imperial_interests).
narrative_ontology:constraint_victim(british_mandate_scaffolding, palestinian_arabs_under_mandate).
narrative_ontology:constraint_victim(british_mandate_scaffolding, regional_self_determination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN ARABS (SNARE) — Trapped within a legal framework explicitly designed to facilitate their demographic displacement. No exit options: cannot leave homeland, cannot vote out Mandate authority, cannot access international legal recourse. British military suppression of resistance (1936-39 revolt) demonstrates active enforcement against exit attempts. Maximum extraction: land expropriation, political disenfranchisement, cultural erasure. The 'coordination' story (developing Palestine) is pure cover.
constraint_indexing:constraint_classification(british_mandate_scaffolding, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ZIONIST INSTITUTIONS (ROPE) — Jewish Agency, Histadrut, Haganah experience the Mandate as coordination infrastructure. British legal framework enables land purchase, immigration facilitation, parallel governance structures. Arbitrage exit: can negotiate with British, appeal to League of Nations, mobilize international Jewish support. Net beneficiary: extraction flows toward this agent. The scaffold IS the coordination mechanism from this position.
constraint_indexing:constraint_classification(british_mandate_scaffolding, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: BRITISH IMPERIAL ADMINISTRATION (SCAFFOLD) — Mandate explicitly temporary: League of Nations framework requires transition to self-governance. British see themselves as providing transitional infrastructure (legal system, security, economic development) until local capacity matures. Sunset built into Mandate charter. Mobile exit: can withdraw when strategic costs exceed benefits (as occurred 1947-48). Theater_ratio moderate: some genuine administrative function alongside imperial extraction.
constraint_indexing:constraint_classification(british_mandate_scaffolding, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: PALESTINIAN URBAN ELITE (TANGLED ROPE) — Constrained but not trapped: some economic integration with Mandate economy, some political voice through limited municipal structures. Benefits from infrastructure development (ports, roads, telegraph) while bearing costs of land market distortion and political marginalization. Mixed experience: genuine coordination function exists alongside asymmetric extraction. Can emigrate but at high cost to status and property.
constraint_indexing:constraint_classification(british_mandate_scaffolding, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: MANDATE SYSTEM POST-WWII (PITON) — By 1945, League of Nations defunct, Mandate legal framework atrophied, but British continue performative administration. Primary function (transition to self-governance) abandoned; what remains is institutional inertia and face-saving. Theater_ratio rises sharply 1945-48 as British maintain administrative ritual while planning withdrawal. Piton classification reflects degraded function, not high extraction from this perspective.
constraint_indexing:constraint_classification(british_mandate_scaffolding, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the Mandate exhibits both genuine coordination function (legal infrastructure, economic development, conflict mediation attempts) AND asymmetric extraction (demographic engineering, political disenfranchisement of Arab majority, resource transfer). The scaffold's sunset was real (British withdrew) but the structure it built persisted as the Israeli state, inheriting the Mandate's legal framework and territorial claims. Tangled Rope classification captures the irreducible hybridity: neither pure coordination nor pure extraction.
constraint_indexing:constraint_classification(british_mandate_scaffolding, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(british_mandate_scaffolding_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(british_mandate_scaffolding, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(british_mandate_scaffolding, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(british_mandate_scaffolding, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(british_mandate_scaffolding, TR),
    TR >= 0.70.

:- end_tests(british_mandate_scaffolding_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The Mandate's legal framework and British military power enabled systematic transfer of land, resources, and political power from Arab majority to Jewish minority. Land laws favored Jewish purchase (1940 Land Transfer Regulations restricted Arab sales but not Jewish acquisition in most zones). Immigration quotas prioritized Jewish immigration despite Arab majority opposition (1920: 10% Jewish, 1948: 33% Jewish). Political structure denied Arab majority democratic representation (no elected legislature with real power; appointed bodies dominated by British and Jewish representatives). However, extractiveness is not maximal (not 0.85+) because some genuine administrative infrastructure was built (roads, ports, legal system, public health) that served both communities, and British did attempt (ineffectively) to mediate conflicts. The extraction was substantial but not totalizing. Suppression (0.82): Very high. British military suppression of Arab resistance was systematic and severe: 1936-39 Arab Revolt crushed with mass detentions (5000+), executions (100+), collective punishment (village demolitions, fines), and emergency regulations that suspended civil liberties. Jewish paramilitary organizations (Haganah, Irgun) operated with British tolerance or active cooperation. Arab political organizations banned or restricted. No democratic recourse: Arab majority could not vote out Mandate authority or access international legal mechanisms (League of Nations Mandate Commission had no enforcement power). Exit options for Arabs: emigration (high cost, loss of property and homeland) or armed resistance (met with overwhelming force). Suppression not maximal (not 0.90+) because some Arab political activity was tolerated (municipal councils, limited press freedom, petition rights), and British did occasionally restrain Jewish paramilitary violence. Theater ratio (0.35 at endpoint, rising from 0.25): Moderate, rising sharply post-1945. Early Mandate (1920-36) had substantial functional content: building legal system, infrastructure, economic institutions, conflict mediation attempts. Theater_ratio rises during 1936-39 revolt (performative 'law and order' rhetoric masking demographic engineering) and spikes 1945-48 as British maintain administrative ritual while planning withdrawal. By 1948, Mandate governance is largely theatrical: no genuine attempt to fulfill stated purpose (transition to self-governance for both communities), only face-saving performance until exit. Final value (0.35) reflects that even at endpoint, some functional administration persisted (courts, police, public services) — not pure theater like a fully degraded piton.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how structural position determines classification. Palestinian Arabs see snare: trapped, maximum extraction, coordination story (developing Palestine) is pure cover. Zionist institutions see rope: net beneficiaries, genuine coordination function (state-building infrastructure), arbitrage exit options. British administration sees scaffold: temporary support with sunset clause, mobile exit when costs exceed benefits. Palestinian urban elite sees tangled rope: genuine coordination (infrastructure, economic development) alongside asymmetric extraction (political marginalization, land market distortion). Post-1945 Mandate system sees piton: atrophied function (no genuine transition to self-governance), performative administration maintained through inertia. Analytical observer sees tangled rope at civilizational scope: irreducible hybridity of coordination and extraction that cannot be resolved to either pure type. The gap is not a measurement error — it is the structural reality of a constraint that coordinates for some agents while extracting from others, with a sunset that was real (British withdrew) but left a persistent structure (Israeli state inheriting Mandate framework). The mandatrophy is resolved by recognizing that all six types are legitimate perspectival readings of the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position and beneficiary/victim declarations. Palestinian Arabs: victims + trapped exit → d approaches 1.0 (full target) → maximum experienced extraction. British suppression of 1936-39 revolt demonstrates active enforcement preventing exit. Zionist institutions: beneficiaries + arbitrage exit → d approaches 0.0 (full beneficiary) → negative experienced extraction (subsidy). Jewish Agency received British funding, legal privileges, immigration facilitation — extraction flowed toward this agent. Palestinian urban elite: victims (political marginalization, land market distortion) but also minor beneficiaries (infrastructure, economic integration) + constrained exit → d ≈ 0.6-0.7 (mixed, leaning target) → moderate experienced extraction. British administration: complex — beneficiaries of imperial prestige and strategic position (Suez access, oil routes) but also bore costs of administration and resistance suppression. As institutional actor with mobile exit, d ≈ 0.3-0.4 (mixed, leaning beneficiary) → low experienced extraction. The perspectival gap is maximal: same constraint appears as pure coordination (rope) to beneficiaries, pure extraction (snare) to trapped victims, and mixed (tangled rope) to analytical observer and constrained actors.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint's claimed type (scaffold) is structurally accurate from the British imperial administration's perspective: temporary support with built-in sunset, mobile exit, genuine coordination function (legal infrastructure, economic development). But the same constraint is a snare from the Palestinian Arab perspective (trapped, maximum extraction, coordination story as cover), rope from the Zionist institutional perspective (net beneficiary, genuine coordination), tangled rope from the analytical perspective (irreducible hybridity), and piton from the post-1945 Mandate system perspective (degraded function, performative administration). The mandatrophy is not 'which type is correct?' but 'which perspective are you measuring from?' The scaffold's sunset was real (British withdrew 1948), validating the scaffold classification from the British perspective. But the structure the scaffold built persisted as the Israeli state, inheriting the Mandate's legal framework, territorial claims, and institutional apparatus — which is exactly what scaffolds do when they succeed. The extraction experienced by Palestinian Arabs was not incidental to the coordination function; it was constitutive. A scaffold that coordinates state-building for one community by displacing another is still a scaffold (temporary, sunset clause) AND a snare (for the displaced) AND a rope (for the beneficiaries). The presheaf over the observation site IS the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sunset_fulfillment_vs_abandonment,
    'Did the British Mandate''s sunset represent fulfillment of its stated purpose (transition to self-governance) or abandonment of an unworkable extraction mechanism?',
    'Historical analysis of British withdrawal decision-making (1945-48): Was exit driven by (a) assessment that local capacity for self-governance had matured, or (b) strategic calculation that costs (Arab resistance, international pressure, resource drain) exceeded benefits? Archival evidence from Cabinet papers, Colonial Office correspondence.',
    'If fulfillment: Scaffold classification validated — temporary support achieved its purpose. If abandonment: Scaffold classification is British self-narrative; structural reality is failed Tangled Rope or degraded Piton. Changes interpretation of 1948 partition: planned transition vs. imperial retreat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_fulfillment_vs_abandonment, empirical, 'Whether Mandate sunset was purposeful transition or strategic abandonment').

omega_variable(
    coordination_function_vs_extraction_cover,
    'Did the Mandate''s administrative infrastructure (legal system, economic development, public works) constitute genuine coordination, or was it primarily cover for facilitating demographic displacement?',
    'Comparative analysis: (1) Infrastructure investment patterns — did roads, ports, schools serve both communities proportionally or primarily Jewish settlement areas? (2) Legal framework application — were land laws, immigration quotas, security measures applied symmetrically or asymmetrically? (3) Resource allocation — British budget expenditures by community. Quantitative data from Mandate government reports 1920-1948.',
    'If genuine coordination: Tangled Rope classification appropriate — mixed function. If extraction cover: Snare classification from more perspectives — coordination story is theatrical. Affects assessment of whether British were neutral administrators or active participants in displacement project.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_vs_extraction_cover, empirical, 'Whether Mandate administrative functions were genuine or extractive cover').

omega_variable(
    counterfactual_autonomy,
    'Could the Zionist quasi-state have achieved statehood without British Mandate scaffolding, given the same time horizon and regional opposition?',
    'Counterfactual analysis comparing: (1) Zionist institutional capacity 1920 vs. 1948 (military, economic, administrative), (2) Regional power balance (Ottoman collapse, Arab state formation, British/French imperial presence), (3) Alternative scenarios (direct Ottoman negotiation, League of Nations trusteeship without privileged status, immediate partition). Historical modeling of resource flows, military capacity, diplomatic leverage.',
    'If autonomy possible: Scaffold classification weakens — support was accelerant, not prerequisite. If autonomy impossible: Scaffold classification strengthens — British power was constitutive, not incidental. Determines whether Mandate was temporary support for viable project or permanent dependency relationship that collapsed when withdrawn.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_autonomy, conceptual, 'Whether Zionist state-building required British imperial scaffolding').

omega_variable(
    cs_framing_ambiguity,
    'Is the kernel ''historical right to Palestine'' or the broader ''Zionist legitimacy basis'' that includes multiple groundings (historical, religious, persecution-driven necessity)?',
    'Textual analysis of Balfour Declaration, Mandate charter, Zionist Congress resolutions 1897-1948: Which legitimacy claim is invoked most frequently and with most authority? Does the kernel narrow to a single claim (ancient presence) or remain a composite (presence + persecution + self-determination)? If composite, do the components support each other or create internal tensions?',
    'If kernel is narrow (historical right only): Religious and national-liberation readings are interpretive layers above the kernel. If kernel is composite: The readings are selecting different components of an under-determined kernel, not interpreting a fixed one. Changes the CS pattern from fixed_text + lineage to distributed + multiple authorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_ambiguity, conceptual, 'Whether the legitimacy kernel is singular or composite').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(british_mandate_scaffolding, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mandate_theater_1920, british_mandate_scaffolding, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mandate_theater_1928, british_mandate_scaffolding, theater_ratio, 8, 0.3).
narrative_ontology:measurement(mandate_theater_1936, british_mandate_scaffolding, theater_ratio, 16, 0.35).
narrative_ontology:measurement(mandate_theater_1945, british_mandate_scaffolding, theater_ratio, 25, 0.55).
narrative_ontology:measurement(mandate_theater_1948, british_mandate_scaffolding, theater_ratio, 28, 0.72).

% Extraction over time
narrative_ontology:measurement(mandate_extract_1920, british_mandate_scaffolding, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mandate_extract_1928, british_mandate_scaffolding, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(mandate_extract_1936, british_mandate_scaffolding, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(mandate_extract_1945, british_mandate_scaffolding, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(mandate_extract_1948, british_mandate_scaffolding, base_extractiveness, 28, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mandate_suppress_1920, british_mandate_scaffolding, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(mandate_suppress_1928, british_mandate_scaffolding, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(mandate_suppress_1936, british_mandate_scaffolding, suppression_requirement, 16, 0.88).
narrative_ontology:measurement(mandate_suppress_1945, british_mandate_scaffolding, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(mandate_suppress_1948, british_mandate_scaffolding, suppression_requirement, 28, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(british_mandate_scaffolding, enforcement_mechanism).
narrative_ontology:affects_constraint(british_mandate_scaffolding, demographic_engineering_imperative).

% DUAL FORMULATION NOTE:
% The British Mandate scaffolding is upstream of the demographic engineering imperative: the Mandate's legal framework and military protection enabled systematic demographic transformation (Jewish immigration, land acquisition, institutional development) that the Zionist movement could not achieve autonomously. The demographic engineering constraint (tangled_rope per SCOPE manifest) describes the ongoing structural imperative to maintain Jewish demographic majority; the Mandate scaffolding describes the temporary external support that made the initial demographic shift possible. Both constraints share the same beneficiary (Zionist quasi-state / Israeli state) and victim (Palestinian Arabs) but operate at different time scales and with different mechanisms. The Mandate scaffolding had a sunset (British withdrawal 1948); the demographic engineering imperative persists as long as the ethno-state framework persists.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(british_mandate_scaffolding, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
