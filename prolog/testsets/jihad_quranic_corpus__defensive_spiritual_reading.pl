% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__defensive_spiritual_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__defensive_spiritual_reading
 *   human_readable: Jihad as Defensive Spiritual Struggle and Armed Response (Quranic Defensive Reading)
 *   domain: islamic_jurisprudence/comparative_religious_law/political_theology
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested Quranic jihad
 *   corpus: the defensive-spiritual reading that privileges internal moral
 *   struggle (jihad al-nafs) as the primary form of jihad and frames armed
 *   response (qital) as legitimate only when: (1) response to actual
 *   aggression, (2) authorized by legitimate state authority, (3) conducted
 *   within proportionality and non-combatant immunity constraints. This
 *   reading emerged in classical Islamic jurisprudence (Hanafi tafsir, Maliki
 *   traditions) and has been revitalized by contemporary scholars responding
 *   to post-9/11 geopolitical pressures and the need to distinguish
 *   legitimate Islamic self-defense from expansionist conquest narratives.
 *   The constraint exhibits structural features of a tangled rope: genuine
 *   coordination function (legitimate communities coordinating collective
 *   defense; scholars aligning Islamic jurisprudence with international
 *   humanitarian law norms) coupled with asymmetric extraction (armed
 *   combatants bear disproportionate mortality risk; non-combatants face
 *   un-enforced immunity promises; conscripted soldiers face coercive
 *   mobilization). The theater ratio has increased over the 10-period
 *   interval as the classical jurisprudential authority structure has become
 *   increasingly performative: modern state decisions about war/peace are
 *   made through secular political processes, while the theological
 *   authorization apparatus continues to speak as if it governs, but without
 *   actual decision-making power.
 *
 * KEY AGENTS:
 *   - Legitimate State Authority (institutional/arbitrage): Primary beneficiary — monopolizes legitimate authorization for armed jihad; experiences framework as coordination mechanism; can exit by framing conflicts in secular terms
 *   - Community Under Military Aggression (moderate/constrained): Primary victim/beneficiary — experiences genuine coordination function (collective defense) but faces asymmetric risk distribution and constrained response options
 *   - Conscripted Soldiers Without Doctrinal Authority (powerless/trapped): Primary victim — recruitment coercion without legitimate authorization; bears mortality cost; no exit option
 *   - Non-Combatants in War Zones (powerless/trapped): Primary victim — promised immunity but de facto targeted; trapped by geography; suppressed by violence
 *   - International Humanitarian Law Alignment Movement (organized/constrained): Secondary actor — organizes around convergence between Islamic defensive jurisprudence and secular IHL norms; sees sunset pathway as norms become universal
 *   - Powerful Non-Muslim States (powerful/mobile): Tertiary actor — experience framework as both coordination (when allied states adopt defensive posture) and extraction (when framework is weaponized to delegitimize actions outside defensive template)
 *   - Classical Jurisprudential Authority Structure (institutional/arbitrage): Institutional actor — transmits interpretive tradition; increasingly performative as state replaces theological authority; experiences high theater ratio
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.38).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.52).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Jihad as Defensive Spiritual Struggle and Armed Response (Quranic Defensive Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "islamic_jurisprudence/comparative_religious_law/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__defensive_spiritual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '857384e5-9559-4088-b4c2-a19d6dc97de7').
narrative_ontology:cs_kernel_codification('857384e5-9559-4088-b4c2-a19d6dc97de7', fixed_text).
narrative_ontology:cs_authority_grounding('857384e5-9559-4088-b4c2-a19d6dc97de7', lineage).
narrative_ontology:cs_interpretation_layer_present('857384e5-9559-4088-b4c2-a19d6dc97de7').
narrative_ontology:cs_reading_relation('857384e5-9559-4088-b4c2-a19d6dc97de7', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('857384e5-9559-4088-b4c2-a19d6dc97de7', jihad_quranic_corpus__revolutionary_vanguard_reading, coexists_with).
narrative_ontology:cs_axiom('857384e5-9559-4088-b4c2-a19d6dc97de7', foundational, spiritual_struggle_primacy).
narrative_ontology:cs_axiom_status(spiritual_struggle_primacy, holdable).
narrative_ontology:cs_axiom_grounding('857384e5-9559-4088-b4c2-a19d6dc97de7', spiritual_struggle_primacy, deontological).
narrative_ontology:cs_axiom('857384e5-9559-4088-b4c2-a19d6dc97de7', foundational, state_authorization_requirement).
narrative_ontology:cs_axiom_status(state_authorization_requirement, holdable).
narrative_ontology:cs_axiom_grounding('857384e5-9559-4088-b4c2-a19d6dc97de7', state_authorization_requirement, conventional).
narrative_ontology:cs_axiom('857384e5-9559-4088-b4c2-a19d6dc97de7', foundational, non_combatant_immunity_absolute).
narrative_ontology:cs_axiom_status(non_combatant_immunity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('857384e5-9559-4088-b4c2-a19d6dc97de7', non_combatant_immunity_absolute, deontological).
narrative_ontology:cs_reference_frame('857384e5-9559-4088-b4c2-a19d6dc97de7', classical_defensive_jurisprudence_framework).
narrative_ontology:cs_drift_state('857384e5-9559-4088-b4c2-a19d6dc97de7', contemporary_post_colonial_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('857384e5-9559-4088-b4c2-a19d6dc97de7', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, legitimate_state_authority).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, community_spiritual_integrity).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, aggressive_military_actors).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, illegitimate_armed_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSCRIPTED SOLDIER WITHOUT DOCTRINAL AUTHORITY (SNARE) — Individual combatants lacking state authorization or doctrinal legitimacy are trapped by recruitment coercion, ideological capture, or economic desperation. The defensive reading frames their participation as illegitimate extraction: they bear the mortality cost while the recruitment apparatus claims religious authority without structural constraint. No exit option; maximal suppression.
constraint_indexing:constraint_classification(jihad_quranic_corpus__defensive_spiritual_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMUNITY UNDER ACTIVE MILITARY AGGRESSION (TANGLED ROPE) — Communities facing genuine military attack experience the defensive jihad framework as genuine coordination (collective defense) coupled with asymmetric extraction (some bear disproportionate risk). The framework provides legitimate means to respond but constrains how — proportionality and non-combatant immunity rules limit the response even when under attack. Mixed: real coordination function + enforcement-backed constraints.
constraint_indexing:constraint_classification(jihad_quranic_corpus__defensive_spiritual_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LEGITIMATE STATE AUTHORITY (ROPE) — States with recognized authority (caliphate, recognized sovereign, legitimate governance structure) experience the defensive jihad framework as coordination mechanism: it legitimates defensive force, channels mobilization, and constrains combatant behavior through proportionality and immunity rules. Benefits from monopoly on legitimate force; experiences minimal extraction; can arbitrage between religious and secular authority framings.
constraint_indexing:constraint_classification(jihad_quranic_corpus__defensive_spiritual_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NON-COMBATANTS IN WAR ZONES (SNARE) — Civilians caught in armed conflict zones experience maximum extraction: they are explicitly protected under this reading's immunity rules but de facto targeted or harmed through proximity, infrastructure destruction, or actor non-compliance. Trapped by geography; suppressed by military violence; no exit. The doctrinal framework promises protection but lacks enforcement against violations.
constraint_indexing:constraint_classification(jihad_quranic_corpus__defensive_spiritual_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL HUMANITARIAN LAW ALIGNMENT MOVEMENT (SCAFFOLD) — Organized actors (Islamic scholars aligning jurisprudence with IHL, human rights organizations, interfaith dialogue initiatives) see the defensive reading as temporary coordination mechanism with an exit path: as international legal norms around proportionality and non-combatant immunity become universal, the theological justification for these rules becomes redundant. Sunset: when IHL consensus is globally enforced, explicit jihad jurisprudence becomes less structurally necessary (though may persist through cultural continuity).
constraint_indexing:constraint_classification(jihad_quranic_corpus__defensive_spiritual_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: POWERFUL NON-MUSLIM STATES FACING GEOPOLITICAL COMPETITION (TANGLED ROPE) — Geopolitical powers experience this reading as both coordination (when Muslim allies adopt defensive rather than expansionist posture) and extraction (when the defensive framing is weaponized to delegitimize military action that does not fit the defensive-response template). Mobile exit: can frame conflict in secular terms or religious terms depending on strategic interest. Moderate power + mobile exit = moderate experienced extraction but with real agency.
constraint_indexing:constraint_classification(jihad_quranic_corpus__defensive_spiritual_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: CLASSICAL JURISPRUDENTIAL AUTHORITY STRUCTURE (PITON) — The institutional infrastructure that transmits and interprets classical jihad jurisprudence is increasingly performative. Modern state formations have displaced traditional caliphate authority; classical tafsir commentary is recited without updating for contemporary geopolitical complexity; the ritual of theological authorization persists even when state decision-making is conducted in secular political frameworks. Theater ratio high because the jurisprudential apparatus continues to speak as if it governs while actual war/peace decisions are made by state bureaucracies outside its framework.
constraint_indexing:constraint_classification(jihad_quranic_corpus__defensive_spiritual_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / LOGICAL NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective, some version of defensive-response framework is logically necessary for any community facing external aggression: the moral principle that proportional self-defense is legitimate appears universal across ethical systems. This perspective risks naturalizing what is actually a contingent interpretive choice within Islamic jurisprudence. The engine flags this as false summit: the defensive reading is one reading of the jihad corpus, not a natural law.
constraint_indexing:constraint_classification(jihad_quranic_corpus__defensive_spiritual_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jihad_quranic_corpus__defensive_spiritual_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jihad_quranic_corpus__defensive_spiritual_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, TR),
    TR >= 0.70.

:- end_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The defensive reading's base extraction value is below the snare threshold (0.46) because the framework genuinely includes coordination function — community collective defense is a real coordination problem and the framework solves it. However, extraction is non-zero because the armed response pathway is asymmetric: some actors (legitimate state, military leaders) capture disproportionate benefit from the authorization apparatus, while conscripted soldiers and non-combatants bear disproportionate cost. The 0.38 value reflects a framework that is primarily coordination with embedded extraction rather than primarily extraction with minimal coordination. Suppression (0.52): Moderate-high. Multiple suppression mechanisms operate: (1) enforcement of doctrinal authority — legitimate state authority is required, limiting which actors can mobilize; (2) recruitment coercion — individuals join fighting forces through economic desperation, ideological capture, or direct conscription; (3) non-combatant targeting despite immunity rule — suppression of civilian resistance options; (4) geographic entrapment — populations in conflict zones cannot exit; (5) information asymmetry — doctrinal texts are specialized, interpretation is concentrated in religious scholar class. Theater ratio (0.58): Moderate-high and increasing. The classical jurisprudential apparatus continues to authorize and comment on military action but has lost decision-making power to secular state bureaucracies. Theological legitimation is performed even when military decisions are made entirely on strategic/political grounds. The increase from 0.35 to 0.58 reflects growing professionalization of state militaries and simultaneous persistence of theological authorization rhetoric.
 *
 * PERSPECTIVAL GAP:
 *   The defensive-spiritual reading displays extreme perspectival divergence across power/exit axes. State authority (institutional/arbitrage) sees rope — coordination mechanism for legitimate defense without significant extraction. Legitimate communities (moderate/constrained) see tangled rope — real coordination plus asymmetric risk. Conscripted soldiers (powerless/trapped) see snare — coerced mobilization with maximal mortality risk. Non-combatants (powerless/trapped) see snare — promised protection that cannot be enforced. International HRL movement (organized/constrained) sees scaffold — temporary coordination with identifiable sunset. Powerful states (powerful/mobile) see tangled rope — coordination benefits from allied defensiveness but costs from framework misuse. Classical jurisprudential authority (institutional/arbitrage) sees piton — performative authorization structure maintaining theater through institutional inertia. Analytical observer (analytical/analytical) risks seeing mountain — treating defensive response as logically necessary universal principle — but the engine flags this as false summit, revealing the reading as one interpretation of an ambiguous corpus.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's structural position. Legitimate state authority (institutional/arbitrage) experiences d ≈ 0.15 — net beneficiary of authorization monopoly, can arbitrage between religious and secular framings, low experienced extraction. Community under aggression (moderate/constrained) experiences d ≈ 0.52 — mixed: genuine benefit from legitimate defense capability but constrained by proportionality rules and bearing asymmetric risk, creating symmetric extraction. Conscripted soldiers (powerless/trapped) experience d ≈ 0.95 — full victims, no arbitrage or exit options, maximum extraction (trapped status + victim group membership). Non-combatants (powerless/trapped) experience d ≈ 0.93 — near-full victims despite doctrinal immunity promise, trapped by geography, suppressed by violence. International HRL alignment movement (organized/constrained) experiences d ≈ 0.40 — moderate extraction because organizing around convergence and sunset creates constrained but real exit pathway. Powerful states (powerful/mobile) experience d ≈ 0.55 — near-symmetric because their mobile exit and powerful position permit real agency and benefits (from allied states' defensiveness) but also face costs (when framework is weaponized against them).
 *
 * MANDATROPHY ANALYSIS:
 *   The defensive-spiritual reading does NOT fully resolve mandatrophy within itself; rather, it structures the mandatrophy debate by making explicit what structural positions coexist within a single constraint. The reading simultaneously claims: (1) spiritual jihad is primary, armed jihad is secondary — (rope framing); (2) armed jihad, when authorized, is genuine collective defense — (tangled rope framing); (3) non-authorized armed actors are extractive snares. The mandatrophy is resolved through perspectival consistency: the reading does not claim one type across all contexts, but rather claims that each perspective's type follows from that perspective's structural position. A powerless trapped actor sees snare because their structural position IS snare. A state authority sees rope because their structural position IS rope. The reading's internal consistency lies in the claim that these are not contradictions but expressions of the same underlying structure from different observation sites.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_legitimacy_threshold,
    'What structural criteria define ''legitimate state authority'' sufficient to authorize armed jihad under this reading? Who determines legitimacy?',
    'Historical case analysis: which actual states have been recognized as legitimate by classical scholars and contemporary interpreters? What changed between 1400s and contemporary period regarding legitimacy criteria?',
    'If threshold is classical caliphate only: modern armed jihad lacks legitimate authorization (strengthens defensive reading''s constraint on armed response). If threshold includes modern nation-states or transnational authority: defensive reading permits broader armed mobilization. If threshold is undefined: legitimacy becomes contestable and the defensive framework becomes theater (piton classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_legitimacy_threshold, conceptual, 'Criteria and authority for determining legitimate state authorization').

omega_variable(
    proportionality_measurement,
    'How is proportionality between defensive response and initial aggression operationalized in contemporary contexts (drone strikes, cyber warfare, infrastructure targeting, economic sanctions)?',
    'Jurisprudential analysis across schools (Hanafi, Maliki, Shafi''i, Hanbali) and contemporary scholars; case studies of contested military actions evaluated under proportionality frameworks; comparison with IHL proportionality doctrine',
    'If proportionality can be operationalized with precision: defensive reading provides meaningful constraint on response (true tangled rope). If proportionality remains abstract/contested: framework becomes theater, constraints are unenforceable (piton classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement, empirical, 'Operationalization of proportionality in modern military contexts').

omega_variable(
    non_combatant_immunity_enforcement,
    'Does the classical non-combatant immunity rule actually constrain actor behavior in practice, or does it function primarily as rhetorical covering for military decisions made on other grounds?',
    'Analysis of actual military conduct by actors claiming jihad authority; statements by military commanders comparing public legal justifications with operational decision documents; post-conflict documentation of civilian harm and stated intention',
    'If enforcement mechanisms are weak and violations routine: the immunity rule is performative theater, suppression is high despite doctrinal protection (snare rather than tangled rope). If enforcement through community sanction or authority sanctions violators: framework has real constraint force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_combatant_immunity_enforcement, empirical, 'Whether non-combatant immunity rules actually constrain actor behavior').

omega_variable(
    defensive_vs_preemptive_boundary,
    'Where is the boundary between legitimate ''defensive'' response and illegitimate ''preemptive'' initiation? Who determines whether a threat is imminent enough to justify preemptive strike?',
    'Jurisprudential texts on intention (niyyah) and threat assessment; historical cases where preemptive action was claimed as defensive response; comparative analysis with secular preemption doctrine (Bush doctrine, etc.)',
    'If boundary is clear and restrictive (actual aggression only, no speculation about future threat): defensive reading meaningfully constrains armed response. If boundary is porous (imminent threat, probable threat, strategic threat): framework permits expansive interpretation, defensive label becomes cover for preemption.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(defensive_vs_preemptive_boundary, conceptual, 'Boundary between defensive response and preemptive initiation').

omega_variable(
    spiritual_jihad_actual_priority,
    'In contemporary Islamic communities, what is the actual structural priority assigned to spiritual jihad al-nafs (inner struggle) versus armed defensive jihad? Is the spiritual framing primary or secondary?',
    'Content analysis of contemporary Islamic teaching, fatwa databases, educational curricula; ethnographic study of how communities prioritize and discuss jihad concepts; comparison of preaching emphasis across theological schools',
    'If spiritual jihad is genuinely primary and armed jihad is clearly secondary: the defensive reading''s framing is accurate. If armed jihad receives disproportionate emphasis or spiritual jihad is treated as metaphor: the reading''s structural claim about primacy is contradicted by practice, suggesting theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiritual_jihad_actual_priority, empirical, 'Actual structural priority of spiritual versus armed jihad in contemporary Islam').

omega_variable(
    reading_kernel_relationship,
    'Is this reading (defensive-spiritual-constrained) one legitimate interpretation of an ambiguous Quranic corpus, or does it require selective reading and de-emphasis of particular Quranic passages?',
    'Systematic analysis of all Quranic verses mentioning jihad, qital (fighting), and related concepts; documentation of which verses are emphasized vs de-emphasized by this reading; comparison with other readings'' treatment of same passages',
    'If reading can accommodate ALL Quranic war-related passages under coherent logic: it is a defensible interpretation of the whole corpus. If reading requires dismissing or reinterpreting some passages as ''historical context only'': it is a bounded reading of a contested kernel. The engine uses this to determine whether this reading truly forecloses sibling readings or merely coexists with them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_relationship, conceptual, 'Whether defensive-spiritual reading can coherently incorporate all jihad-related Quranic passages').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jihad_def_tr_t0, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(jihad_def_tr_t5, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(jihad_def_tr_t10, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(jihad_def_be_t0, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(jihad_def_be_t5, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(jihad_def_be_t10, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(jihad_def_su_t0, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(jihad_def_su_t5, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(jihad_def_su_t10, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__defensive_spiritual_reading, 0.12).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, non_combatant_immunity_enforcement_gap).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, state_monopoly_on_legitimate_force).

% DUAL FORMULATION NOTE:
% The jihad_quranic_corpus kernel decomposes into three constraint stories, each with distinct ε values and different beneficiary/victim structures. The defensive-spiritual reading (this file) has ε=0.38 and privileges non-Muslim outsiders as neither beneficiary nor victim except when aggressive. The expansionist reading (sibling constraint) has ε ≈ 0.52 and includes broader victim sets. The vanguard reading (sibling constraint) has ε ≈ 0.68 and operates without state-authority constraint. All three interpret the same textual corpus but produce different structural constraints; they are linked by network.affects_constraints to show mutual influence and the kernel dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__defensive_spiritual_reading, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
