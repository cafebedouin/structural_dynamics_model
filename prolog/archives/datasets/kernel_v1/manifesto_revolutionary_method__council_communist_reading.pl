% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__council_communist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__council_communist_reading, []).

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
 *   constraint_id: manifesto_revolutionary_method__council_communist_reading
 *   human_readable: Workers' Councils (Soviets) as Direct Democratic Organs of Revolutionary Coordination
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   The council-communist reading instantiates the manifesto's revolutionary
 *   method as direct democratic federation of autonomous workplace
 *   assemblies, replacing both capitalist state apparatus and vanguard party
 *   hierarchy. This reading claims that workers' councils (soviets)
 *   constitute a genuine coordination mechanism for production, distribution,
 *   and conflict resolution without requiring external authority to
 *   adjudicate or enforce decisions. The constraint exhibits low base
 *   extractiveness (0.18) within councils themselves — assemblies coordinate
 *   genuine collective interests without coercive overhead — but exhibits
 *   suppression through external pressure: rival readings (vanguard-rupture,
 *   democratic-gradualism) contest the council form's legitimacy, creating
 *   structural opposition to pure council autonomy. The reading's structural
 *   claim is that federation of workplace councils solves the coordination
 *   problem that capitalist markets obscure and state hierarchies solve
 *   through bureaucratic coercion. Theater ratio (0.35) is moderate: councils
 *   require some performative authority (procedural ritual, consensus
 *   ceremonies) to maintain legitimacy, but less than state apparatuses or
 *   vanguard parties because the council's authority is genuinely rooted in
 *   worker participation rather than delegated to representatives. The
 *   measurement trajectory shows extractiveness and suppression rising over
 *   the interval (0-6), reflecting external pressure from rival revolutionary
 *   powers and the internal tension between council autonomy and the
 *   necessities of wartime or economic emergency.
 *
 * KEY AGENTS:
 *   - Autonomous Worker Collectives: Primary beneficiary (organized/mobile) — councils are the structure through which they exercise self-determination; low extraction because they control the mechanism
 *   - Federated Workplace Assemblies: Primary beneficiary (organized/mobile) — councils are the coordination organs; benefit from elimination of both capitalist and bureaucratic hierarchies
 *   - Industrial Workers (Majority): Secondary beneficiary (moderate/constrained) — benefit from workplace democracy and collective decision-making; constrained by participation requirements and majority decisions
 *   - Structural Minorities (Dissenting Groups): Primary victim (powerless/trapped) — trapped by majority-rule councils without minority protections; suppressed by assembly enforcement of collective decisions
 *   - Revolutionary Vanguard Party: Tertiary agent (institutional/constrained) — guides transition but is theoretically subordinate to councils; tension between guidance role and council autonomy
 *   - Existing State Bureaucracy: Institutional antagonist (institutional/arbitrage) — council form directly threatens state apparatus; either dissolves or persists as piton (hidden layer under apparent council authority)
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing council federation as inevitable political form; must remain alert to false-summit risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.18).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.52).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Workers' Councils (Soviets) as Direct Democratic Organs of Revolutionary Coordination").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political_philosophy/revolutionary_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, '37eece59-d316-4623-825b-2f0949d48451').
narrative_ontology:cs_kernel_codification('37eece59-d316-4623-825b-2f0949d48451', distributed).
narrative_ontology:cs_authority_grounding('37eece59-d316-4623-825b-2f0949d48451', lineage).
narrative_ontology:cs_interpretation_layer_present('37eece59-d316-4623-825b-2f0949d48451').
narrative_ontology:cs_reading_relation('37eece59-d316-4623-825b-2f0949d48451', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('37eece59-d316-4623-825b-2f0949d48451', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_axiom('37eece59-d316-4623-825b-2f0949d48451', foundational, council_autonomy_constitutive).
narrative_ontology:cs_axiom_status(council_autonomy_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('37eece59-d316-4623-825b-2f0949d48451', council_autonomy_constitutive, deontological).
narrative_ontology:cs_axiom('37eece59-d316-4623-825b-2f0949d48451', foundational, federated_rather_than_centralized).
narrative_ontology:cs_axiom_status(federated_rather_than_centralized, holdable).
narrative_ontology:cs_axiom_grounding('37eece59-d316-4623-825b-2f0949d48451', federated_rather_than_centralized, deontological).
narrative_ontology:cs_reference_frame('37eece59-d316-4623-825b-2f0949d48451', autonomous_federated_worker_councils).
narrative_ontology:cs_drift_state('37eece59-d316-4623-825b-2f0949d48451', contemporary_state_machinery, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('37eece59-d316-4623-825b-2f0949d48451', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, federated_workplace_assemblies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERATED WORKPLACE ASSEMBLY (ROPE) — Organized workers viewing the council system as a genuine coordination mechanism for production, resource distribution, and conflict resolution. Exit options exist (workers can withdraw to subsistence, migrate to other councils, or form alternative assemblies). Low experienced extraction because the assembly itself is the beneficiary — no external authority capturing value. The mechanism solves collective action problems (who decides production priorities, how are disputes resolved) without coercive overhead beyond internal consensus enforcement.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__council_communist_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: INDUSTRIAL WORKER IN TRANSITION (TANGLED ROPE) — A moderate-power agent experiencing both coordination benefits and extraction costs during the revolutionary transition. Benefits from workplace democracy, elimination of capitalist extraction, and access to collective decision-making. Costs: constrained by the necessity of participation (opting out destabilizes the collective), loss of individual economic initiative, exposure to majority decisions that may burden minorities. Experiences genuine coordination function alongside asymmetric enforcement of participation norms.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__council_communist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STRUCTURAL MINORITY / DISSENTING GROUPS (SNARE) — Minorities trapped within councils whose decisions contradict their interests: regional minorities, ideological dissenters from majority council policy, workers in essential infrastructure with zero exit. Cannot exit without abandoning community and livelihood. Suppressed by majority-rule enforcement and geographic immobility. No representation mechanism that protects minorities from majoritarian extraction of their labor or resources.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__council_communist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER — FEDERATED STRUCTURE AS NATURAL COORDINATION (MOUNTAIN) — From a civilizational vantage, federated councils may appear as an immutable natural solution to coordinating complex production: the alternative to centralized command and decentralized market chaos is necessarily federal assembly. The constraint appears as a logical inevitability of revolutionary economic organization, independent of implementation details. However, the structural data contradicts this: council systems require constant enforcement, suppress minorities, and decompose when external pressure (war, competing revolutionary powers) demands centralization.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__council_communist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: REVOLUTIONARY VANGUARD AS TRANSITIONAL GUIDE (SCAFFOLD) — An organized revolutionary party viewing councils as a temporary coordination structure with built-in sunset: councils coordinate workers during the transition away from capitalism, but as class consciousness matures and the material basis for state authority disappears, councils themselves become unnecessary. The vanguard sees its role as accelerating this transition, providing guidance until workers can organize production without external leadership. Low theater because the vanguard's legitimacy depends on actual coordination facilitation, not ritual authority. The constraint has an implicit sunset: once the transition is complete, councils should dissolve or merge into fully decentralized production coordination.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__council_communist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: STATE BUREAUCRACY AS INSTITUTIONAL DEGRADATION (PITON) — Existing state organs viewing councils as a degraded or vestigial political form: councils are how workers *think* they exercise power, but the institutional machinery of the state persists underneath, channeling council decisions into state apparatus. Theater ratio high because council meetings perform democratic legitimacy while actual resource distribution and enforcement flow through state bureaucratic channels. The council form persists as ritualized authority maintenance despite the underlying state structure's functional dominance.
constraint_indexing:constraint_classification(manifesto_revolutionary_method__council_communist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__council_communist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(manifesto_revolutionary_method__council_communist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(manifesto_revolutionary_method__council_communist_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, TR),
    TR >= 0.70.

:- end_tests(manifesto_revolutionary_method__council_communist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. Within councils themselves, the coordination mechanism imposes minimal extractive overhead. Workers collectively decide production priorities, resource distribution, and conflict resolution — there is no class of full-time extractors living off council decisions. The extractiveness is not zero (consensus procedures have cost, decision-making is time-intensive, and coordination sometimes imposes costs on minorities) but it is low compared to capitalist extraction (where capitalist class extracts surplus labor) or bureaucratic state extraction (where state officials impose their preferences). Suppression (0.52): Moderate-high. Two distinct sources: (1) internal suppression within councils (majority-rule enforcement against minorities, mandatory participation requirements, social pressure for consensus); (2) external suppression from rival readings (vanguard parties contest council autonomy, state apparatuses try to subordinate councils to bureaucratic channels, capitalist forces suppress councils militarily). The measurement trajectory shows suppression rising from 0.38 to 0.52 over the interval, reflecting that external pressure intensifies as revolutionary forces compete for control of the revolutionary method. Theater ratio (0.35): Moderate. Councils require procedural ritual (assembly procedures, voting ceremonies, federation protocols) to maintain legitimacy, but less theatrical performance than state apparatuses or party bureaucracies. The procedure is functional — it actually coordinates decisions — rather than merely performative. The measurement trajectory shows theater rising slightly (0.20 to 0.35), reflecting that as councils mature institutionally, they develop more formalized procedures (written bylaws, nested federation structures) that add performative content.
 *
 * PERSPECTIVAL GAP:
 *   The council-communist reading exhibits a dramatic perspectival gap between the organized beneficiary and the trapped minority. The federated workplace assembly sees a genuine coordination mechanism (rope) — they are solving the production and distribution problem without external coercion. The industrial worker sees mixed benefits and costs (tangled rope) — workplace democracy is a genuine gain, but participation is enforced and majorities can burden minorities. The structural minority sees pure extraction (snare) — they are trapped in councils whose decisions may extract their labor or suppress their interests without any exit or minority protection. The vanguard views councils as a temporary structure with sunset (scaffold) — revolutionary leadership guides councils toward their own obsolescence as class consciousness matures. The state apparatus views councils as degraded political forms (piton) — councils are how workers think they exercise power, but the machinery of state persists underneath, channeling council decisions into bureaucratic flows. The analytical observer risks naturalizing councils as inevitable (mountain) — the necessity of federated coordination may appear as a natural law of economics rather than a contingent institutional choice. The gap between the beneficiary rope and the minority snare is the reading's central structural tension: the same coordination mechanism that empowers organized majorities suppresses minorities who cannot exit.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading's structural data produces low extractiveness because the beneficiary group (worker collectives) IS the mechanism itself — there is no separate extractive class siphoning value from council decisions. Directionality is therefore compressed toward the low d end (beneficiary with mobile exit options → d ≈ 0.15-0.20 → f(d) ≈ -0.01 to 0.05 → low chi). For trapped minorities, directionality d is high (victim with trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high experienced extraction), but this minority perspective classifies as snare rather than being absorbed into the reading's rope classification because the reading is generated from the perspective of the beneficiary collectives, not the minorities. Each perspective's directionality reflects its structural position: beneficiaries experience low extraction because they control the mechanism; minorities experience high extraction because they are suppressed by majority decisions; the vanguard experiences moderate extraction because their role is to guide but councils may constrain their authority; the state apparatus experiences threat because councils displace its functions.
 *
 * MANDATROPHY ANALYSIS:
 *   The council-communist reading resolves the mandatrophy by demonstrating that all three readings of the manifesto kernel produce structurally coherent constraint classifications, but for different observers. The vanguard-rupture reading (external revolutionary leadership guides the transition through democratic centralism) also makes structural sense from its perspective — it sees councils as tools of the vanguard rather than autonomous agents. The democratic-gradualism reading (workers gradually acquire political power through reform) makes sense from the perspective of actors embedded in existing democratic institutions. The mandatrophy is not 'which reading is correct?' but 'which kernel commitment is primary?' The council-communist reading prioritizes worker autonomy and federation. The vanguard-rupture reading prioritizes revolutionary transformation as guided by organized leadership. The democratic-gradualism reading prioritizes the gradual incorporation of workers into existing political structures. These are not competing empirical claims about 'what workers really want' — they are competing normative commitments that produce different institutional arrangements and different beneficiary/victim structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    council_autonomy_vs_coordination,
    'Can genuinely autonomous worker councils coordinate complex interdependent production without either hierarchical re-emergence or economic inefficiency?',
    'Historical case analysis: examination of council success/failure rates in revolutionary periods (USSR 1917-1921, Paris Commune, Spanish Civil War); measurement of production efficiency and decision-making speed under pure council coordination vs. hybrid council-vanguard structures',
    'If coordination succeeds: council system is rope (genuine coordination). If coordination fails: either councils degrade to piton (theatrical ritual masking state machinery) or snare (organized minorities capture council apparatus). High sensitivity — this determines whether the reading''s core premise is viable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(council_autonomy_vs_coordination, empirical, 'Whether autonomous councils can coordinate complex production').

omega_variable(
    minority_protection_mechanisms,
    'Do councils have structural mechanisms to protect minorities from majoritarian extraction, or does majority rule inherently suppress dissenting groups?',
    'Comparative analysis of minority veto structures, nested federation models, and proportional representation within councils; measurement of minority burden (labor requisition, resource extraction, participation enforcement) over time in actual council systems',
    'If minority protections work: system is tangled rope with built-in asymmetry. If protections fail: system is snare for minorities. Determines whether the reading''s promise of democratic liberation applies universally or only to majorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_mechanisms, empirical, 'Whether council structures protect minorities from majoritarian extraction').

omega_variable(
    external_pressure_stability,
    'Under what conditions of external threat (war, economic embargo, rival revolutionary powers) do councils maintain decentralized authority vs. re-centralizing into hierarchical command?',
    'Historical case analysis: examination of council autonomy loss during wartime (USSR during civil war, Spanish Republic during Fascist invasion, Yugoslav councils during NATO intervention); identification of pressure thresholds above which councils cede authority to central command',
    'If councils maintain autonomy: reading is structurally sound. If councils systematically re-centralize under pressure: reading is aspirational; its core premise depends on absence of external threat, which is contingent rather than revolutionary. Determines whether the reading''s structural claim is real or depends on unrealized conditions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(external_pressure_stability, empirical, 'Council system stability under external military/economic pressure').

omega_variable(
    vanguard_party_subordination,
    'Can a revolutionary vanguard party remain genuinely subordinate to worker councils, or does control of armed forces, propaganda apparatus, and organizational discipline inevitably lead to party capture of council authority?',
    'Institutional analysis: comparison of formal party subordination claims vs. actual decision control in USSR (1917-1927), Yugoslavia (1945-1991), and other council-based systems; measurement of council override power and party disciplinary effects on council delegates',
    'If subordination can be maintained: vanguard perspective (scaffold) is viable; councils + parties coexist as dual structures. If capture is inevitable: vanguard becomes piton or snare; the reading''s core premise is incompatible with vanguard presence. High political stakes — determines whether this reading forecloses the vanguard reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vanguard_party_subordination, conceptual, 'Whether vanguard parties can remain subordinate to councils or inevitably capture authority').

omega_variable(
    council_reading_kernel_contradiction,
    'Is the council-communist reading a genuine alternative instantiation of the manifesto''s revolutionary method kernel, or does the kernel itself implicitly foreclose pure council autonomy by requiring revolutionary agency beyond mass spontaneity?',
    'Textual-historical analysis: examination of Marx/Engels passages on workers'' spontaneous organization, the necessity of political revolution, and the transition to communism; determination of whether the kernel''s core commitment to revolutionary transformation requires external leadership or is compatible with pure federation',
    'If kernel is genuinely ambiguous: all three readings (council-communist, vanguard-rupture, democratic-gradualism) coexist. If kernel implicitly forecloses council autonomy: this reading is aspirational rather than texturally grounded in the manifesto. Determines the reading''s status within the kernel discourse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(council_reading_kernel_contradiction, conceptual, 'Whether the manifesto kernel permits pure council autonomy or requires external revolutionary leadership').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(council_tr_t0, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(council_tr_t3, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement(council_tr_t6, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(council_be_t0, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(council_be_t3, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 3, 0.12).
narrative_ontology:measurement(council_be_t6, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 6, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(council_su_t0, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(council_su_t3, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 3, 0.45).
narrative_ontology:measurement(council_su_t6, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__council_communist_reading, 0.12).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__democratic_gradualism_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, state_apparatus_dissolution).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, worker_consciousness_maturation).

% DUAL FORMULATION NOTE:
% The council-communist reading is ONE instantiation of the manifesto's revolutionary method kernel. Three separate constraint stories (council-communist, vanguard-rupture, democratic-gradualism) represent three distinct readings of the same contested kernel. Each reading has its own ε value, its own beneficiary/victim structure, and its own perspectival set. They are not the same constraint viewed from different angles — they are different constraints that flow from different commitments within the same kernel. The network links show that the vanguard-rupture reading explicitly forecloses some features of the council reading (subordination of councils to party is incompatible with autonomous council authority), while both readings dispute the democratic-gradualism reading's commitment to evolutionary rather than revolutionary transformation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
