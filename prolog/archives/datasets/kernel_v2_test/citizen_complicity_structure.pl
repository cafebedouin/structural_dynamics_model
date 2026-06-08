% ============================================================================
% CONSTRAINT STORY: citizen_complicity_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_citizen_complicity_structure, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: citizen_complicity_structure
 *   human_readable: Citizen Complicity in Democratic Dirty Hands
 *   domain: political_philosophy/normative_ethics/applied_ethics
 *
 * SUMMARY:
 *   The citizen complicity structure in democratic dirty hands theory creates
 *   a normative constraint that distributes moral responsibility for leaders'
 *   morally costly decisions across the entire citizenry. The constraint
 *   emerges from the democratic authorization thesis: if citizens authorize
 *   leaders through voting and institutional participation, they share
 *   responsibility for those leaders' actions, including dirty hands
 *   decisions that violate ordinary moral norms for the sake of collective
 *   goods. This structure serves a genuine coordination function (maintaining
 *   democratic accountability and legitimacy for necessary but morally costly
 *   state actions) while simultaneously extracting from dissenting citizens
 *   who actively opposed the decisions in question. The constraint has
 *   intensified over the interval as democratic theory has expanded the scope
 *   of collective responsibility claims, moving from a focus on active
 *   supporters to encompassing all citizens regardless of individual dissent.
 *   Theater ratio has increased as the philosophical literature has developed
 *   increasingly elaborate justifications for collective guilt that abstract
 *   away from actual authorization mechanisms. Suppression has increased as
 *   exit costs (emigration, statelessness, loss of political voice) have
 *   become more salient in an era of hardened borders and national security
 *   states.
 *
 * KEY AGENTS:
 *   - Dissenting Citizens: Primary victims (powerless/trapped) — voted against, protested, organized opposition, yet assigned shared moral taint and restitution obligations through citizenship alone
 *   - Conscientious Objectors: Primary victims (powerless/identity_locked) — identity constituted through moral refusal of participation; complicity attribution attacks core self-concept
 *   - Ambivalent Voters: Mixed position (moderate/constrained) — voted for leader but opposed specific decision; experience both coordination (democratic accountability) and extraction (guilt for unchosen outcomes)
 *   - Political Leaders: Primary beneficiaries (institutional/arbitrage) — complicity structure diffuses concentrated accountability, legitimates dirty hands decisions through democratic authorization frame
 *   - State Apparatus: Primary beneficiaries (institutional/arbitrage) — institutional continuity protected by distributed responsibility; all actions frameable as democratically authorized
 *   - Transitional Justice Movement: Organized agents (organized/mobile) — building alternative accountability frameworks that distinguish participation from membership; sees complicity structure as temporary with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes mixed coordination-extraction structure; genuine accountability problem solved through asymmetric cost distribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(citizen_complicity_structure, 0.68).
domain_priors:suppression_score(citizen_complicity_structure, 0.72).
domain_priors:theater_ratio(citizen_complicity_structure, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(citizen_complicity_structure, extractiveness, 0.68).
narrative_ontology:constraint_metric(citizen_complicity_structure, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(citizen_complicity_structure, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(citizen_complicity_structure, snare).
narrative_ontology:human_readable(citizen_complicity_structure, "Citizen Complicity in Democratic Dirty Hands").
narrative_ontology:topic_domain(citizen_complicity_structure, "political_philosophy/normative_ethics/applied_ethics").

domain_priors:requires_active_enforcement(citizen_complicity_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(citizen_complicity_structure, political_leaders).
narrative_ontology:constraint_beneficiary(citizen_complicity_structure, state_apparatus).
narrative_ontology:constraint_victim(citizen_complicity_structure, dissenting_citizens).
narrative_ontology:constraint_victim(citizen_complicity_structure, non_consenting_minorities).
narrative_ontology:constraint_victim(citizen_complicity_structure, conscientious_objectors).
narrative_ontology:constraint_vindicates(citizen_complicity_structure, democratic_authorization_doctrine).
narrative_ontology:constraint_vindicates(citizen_complicity_structure, collective_responsibility_thesis).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSENTING CITIZEN (SNARE) — Trapped within national jurisdiction with no exit from collective guilt attribution. Voted against, protested, organized opposition — yet theorists assign shared moral taint through mere citizenship. Cannot exit the polity without extraordinary cost; cannot escape the authorization claim. Maximum extraction: bears restitution obligations for decisions they actively opposed.
constraint_indexing:constraint_classification(citizen_complicity_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSCIENTIOUS OBJECTOR (SNARE) — Identity-locked through moral commitment that makes complicity attribution existentially intolerable. Cannot exit citizenship without abandoning identity as moral agent within their community. The authorization claim attacks the core of their self-concept as someone who refused participation. Extraction operates through identity dissolution threat rather than material penalty.
constraint_indexing:constraint_classification(citizen_complicity_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: AMBIVALENT VOTER (TANGLED ROPE) — Voted for the leader but opposed the specific dirty hands decision. Experiences genuine coordination (democratic accountability mechanisms) alongside extraction (guilt for decisions they didn't authorize specifically). Constrained exit: could emigrate at high cost, could organize politically, but cannot escape the collective responsibility frame while remaining engaged. Mixed beneficiary-victim: benefits from democratic legitimacy while bearing moral taint for outcomes they didn't choose.
constraint_indexing:constraint_classification(citizen_complicity_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: POLITICAL LEADER (ROPE) — Primary beneficiary. The complicity structure diffuses moral responsibility across the citizenry, reducing concentrated accountability for dirty hands decisions. Experiences the constraint as coordination: democratic authorization legitimates necessary but morally costly actions. Arbitrage exit: can leave office, can frame decisions as collective will, can distribute blame. Net beneficiary of the authorization-complicity framework.
constraint_indexing:constraint_classification(citizen_complicity_structure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE APPARATUS (ROPE) — Benefits from legitimacy shield. Collective responsibility doctrine protects institutional continuity by preventing concentrated blame on decision-makers. Arbitrage exit: institutions persist beyond individual leaders; can frame all actions as democratically authorized. Experiences complicity structure as coordination mechanism that stabilizes governance through distributed accountability.
constraint_indexing:constraint_classification(citizen_complicity_structure, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: TRANSITIONAL JUSTICE MOVEMENT (SCAFFOLD) — Organized agents building alternative accountability frameworks that distinguish individual authorization from collective membership. Truth and reconciliation processes, international criminal law, and restorative justice models are creating pathways that assign responsibility based on actual participation rather than citizenship alone. Sees complicity structure as temporary coordination problem with sunset: as these frameworks mature, the crude collective guilt model loses normative force. Mobile exit: can operate transnationally, can build parallel institutions.
constraint_indexing:constraint_classification(citizen_complicity_structure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes genuine coordination function (democratic accountability requires some collective responsibility mechanism) alongside substantial extraction (the authorization claim overextends to non-consenting citizens). The constraint solves a real problem (how to maintain democratic legitimacy for morally costly decisions) while creating asymmetric costs (dissenters bear guilt for decisions they opposed). Analytical distance reveals the mixed structure but doesn't eliminate it.
constraint_indexing:constraint_classification(citizen_complicity_structure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(citizen_complicity_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(citizen_complicity_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(citizen_complicity_structure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(citizen_complicity_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(citizen_complicity_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts substantially from dissenting citizens who bear moral taint and restitution obligations for decisions they actively opposed. The extraction is not maximal because some citizens (active supporters, ambivalent voters) do experience genuine coordination through the democratic accountability mechanism. But for dissenters, the authorization claim is pure extraction — they gain no legitimacy benefit and bear full moral cost. Suppression (0.72): High. Exit options are severely constrained: emigration requires abandoning career, family, community, and political voice; statelessness is a human rights catastrophe; remaining within the polity means accepting the complicity frame. Dissent within the system (voting, protest, organizing) does not exempt from collective responsibility in most theoretical formulations. The suppression is not total (some emigration is possible, some theorists do recognize dissent exemptions) but is substantial for most citizens. Theater ratio (0.58): Moderate-high. Much of the philosophical literature on collective responsibility abstracts away from actual authorization mechanisms (who voted for what, who protested, who objected) and treats citizenship itself as sufficient for complicity. The elaborate justifications for collective guilt (democratic authorization, collective agency, shared fate) often function as post-hoc rationalizations for distributing blame away from decision-makers. But the theater is not total — some genuine accountability work is being done, and some theorists do grapple seriously with dissent and objection.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence based on structural position. Political leaders and state apparatus experience pure coordination (Rope) — the complicity structure legitimates their decisions and protects institutional continuity. Dissenting citizens and conscientious objectors experience pure extraction (Snare) — they bear moral costs for decisions they opposed with no benefit and no exit. Ambivalent voters experience mixed coordination-extraction (Tangled Rope) — genuine democratic accountability alongside guilt for unchosen outcomes. The transitional justice movement sees a temporary problem with sunset logic (Scaffold) — alternative frameworks are emerging that distinguish participation from membership. The analytical observer recognizes the mixed structure (Tangled Rope) but cannot eliminate it through analysis alone. The gap between the leader's Rope and the dissenter's Snare is the core extraction mechanism: what appears as coordination from above is experienced as entrapment from below.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the authorization-complicity framework. Political leaders are full beneficiaries (d ≈ 0.0): the constraint diffuses their accountability and legitimates their decisions. State apparatus is also a beneficiary (d ≈ 0.1): institutional continuity is protected. Ambivalent voters are mixed (d ≈ 0.5): they experience both coordination benefits (democratic legitimacy) and extraction costs (guilt for unchosen outcomes). Dissenting citizens are full targets (d ≈ 0.9): they bear moral taint for decisions they opposed with no offsetting benefit. Conscientious objectors are also full targets (d ≈ 0.95): the complicity claim attacks their identity as moral agents. The transitional justice movement has low directionality (d ≈ 0.2) because they have organized power and mobile exit — they can build alternative frameworks and operate transnationally. The analytical observer has moderate directionality (d ≈ 0.4) because they recognize the extraction but also see the genuine coordination function. The engine computes effective extraction (chi) from these directionality values combined with power and exit options: trapped powerless agents experience maximum chi; institutional agents with arbitrage exit experience low or negative chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by revealing that the coordination function (democratic accountability for dirty hands decisions) and the extraction mechanism (guilt attribution to non-consenting citizens) are structurally inseparable in current democratic theory. The mandate (maintain democratic legitimacy for morally costly state actions) has not outlived its function — states still need to make dirty hands decisions and still need legitimacy for them. But the mechanism (collective responsibility through citizenship alone) has become extractive because it overextends to dissenters who never authorized the decisions. The mandatrophy is not resolved by eliminating the constraint (democratic accountability is necessary) but by recognizing that the current implementation extracts from powerless agents who cannot exit. The transitional justice movement's scaffold perspective points toward resolution: alternative accountability frameworks that distinguish participation from membership could maintain the coordination function while reducing extraction from dissenters.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorization_threshold,
    'What level of participation constitutes morally relevant authorization: voting for the leader, voting for the specific policy, active support, or mere non-resistance?',
    'Comparative analysis of democratic theory traditions; empirical study of citizens'' own authorization beliefs; philosophical argument about the conditions for valid consent',
    'If threshold is high (active support required): most dissenters escape complicity, extraction drops substantially. If threshold is low (citizenship alone sufficient): complicity structure becomes pure extraction with no coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authorization_threshold, conceptual, 'Threshold for morally relevant democratic authorization').

omega_variable(
    dissent_exemption_mechanism,
    'Do documented dissent, conscientious objection, or active resistance exempt citizens from collective responsibility, or does citizenship alone establish complicity regardless of individual action?',
    'Analysis of restitution obligations in post-conflict societies; legal precedents for conscientious objector status; philosophical arguments about the limits of collective responsibility',
    'If dissent exempts: constraint becomes coordination mechanism (Rope from more perspectives) distinguishing participants from objectors. If dissent doesn''t exempt: constraint is pure extraction (Snare from more perspectives) with no escape for powerless agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissent_exemption_mechanism, conceptual, 'Whether dissent exempts from collective responsibility').

omega_variable(
    restitution_asymmetry,
    'Are restitution obligations distributed equally across all citizens, or do leaders and active supporters bear greater obligations than dissenters and objectors?',
    'Empirical analysis of post-conflict restitution programs; philosophical arguments about proportional responsibility; legal frameworks for differential accountability',
    'If obligations are equal: extraction is maximized for dissenters (they pay the same as supporters). If obligations are proportional: extraction is reduced (dissenters pay less or nothing), and the constraint shifts toward coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restitution_asymmetry, preference, 'Distribution of restitution obligations across citizen types').

omega_variable(
    exit_cost_threshold,
    'At what cost does emigration become a morally required response to complicity, versus a supererogatory sacrifice that citizens cannot be obligated to make?',
    'Philosophical analysis of the limits of moral obligation; empirical study of emigration costs (economic, social, familial); comparative analysis of refugee and emigrant status',
    'If exit cost threshold is low: citizens who remain are complicit by choice, reducing extraction. If threshold is high: most citizens are trapped, maximizing extraction for dissenters.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_cost_threshold, preference, 'Moral threshold for emigration as complicity escape').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(citizen_complicity_structure, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(complicity_theater_founding, citizen_complicity_structure, theater_ratio, 0, 0.35).
narrative_ontology:measurement(complicity_theater_midcentury, citizen_complicity_structure, theater_ratio, 25, 0.48).
narrative_ontology:measurement(complicity_theater_contemporary, citizen_complicity_structure, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(complicity_extraction_founding, citizen_complicity_structure, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(complicity_extraction_midcentury, citizen_complicity_structure, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(complicity_extraction_contemporary, citizen_complicity_structure, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(complicity_suppression_founding, citizen_complicity_structure, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(complicity_suppression_midcentury, citizen_complicity_structure, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(complicity_suppression_contemporary, citizen_complicity_structure, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(citizen_complicity_structure, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of political_exceptionalism (the claim that political leaders face unique moral permissions for dirty hands decisions). The exceptionalism constraint establishes that leaders may violate ordinary moral norms; the complicity constraint distributes responsibility for those violations across the citizenry. The two constraints are structurally linked: if leaders have no special permissions (exceptionalism is false), then there are no dirty hands decisions to distribute responsibility for (complicity structure collapses). If leaders have special permissions but citizens don't share responsibility (complicity is false), then democratic accountability fails (exceptionalism becomes pure extraction). The complicity structure is the mechanism by which democratic theory attempts to reconcile leader exceptionalism with popular sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
