% ============================================================================
% CONSTRAINT STORY: presidential_war_powers_usurpation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_presidential_war_powers_usurpation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: presidential_war_powers_usurpation
 *   human_readable: Presidential War Powers Usurpation and Congressional Erosion
 *   domain: constitutional_law/political_power/executive_authority
 *
 * SUMMARY:
 *   Presidential war powers usurpation represents a structural erosion of the
 *   War Powers Clause separation of powers, where the executive branch has
 *   accumulated effective unilateral authority to initiate military
 *   operations without formal congressional declaration. The constraint
 *   exhibits six distinct classifications from different structural
 *   positions: pure extraction (snare) from the perspective of conscripts and
 *   citizens affected by undeclared wars; mixed coordination and extraction
 *   (tangled rope) from Congress's perspective as it retains nominal
 *   authority but faces suppression; coordination mechanism (rope) from the
 *   executive's perspective enabling rapid response; degraded theatrical
 *   constraint (piton) from the international law framework; temporary
 *   institutional pathology (scaffold) from constitutional reform actors; and
 *   naturalized immutable necessity (mountain) from the civilizational
 *   analytical observer. The constraint's extractiveness (0.68) reflects
 *   asymmetric war authorization and resource extraction by the executive;
 *   suppression (0.72) reflects institutional and informational barriers
 *   preventing congressional reassertion; theater ratio (0.58) reflects the
 *   continued performative invocation of constitutional procedures alongside
 *   their substantive erosion. The measurement trajectory shows steady
 *   extractiveness accumulation over 75 years, with theater ratio rising as
 *   procedural theater (War Powers Resolution, military briefings) increases
 *   while substantive congressional control decreases.
 *
 * KEY AGENTS:
 *   - Executive Branch (Institutional/Arbitrage): Primary beneficiary — accumulates unilateral war authorization authority and military command discretion; experiences constraint as enabling coordination function
 *   - Congress (Moderate/Constrained): Primary victim-participant — retains nominal authority but faces suppression through information asymmetry, institutional inertia, and electoral costs of appearing unpatriotic; experiences constraint as mixed coordination-extraction
 *   - Military and Defense Apparatus (Organized/Arbitrage): Secondary beneficiary — direct authority expansion and operational discretion under executive delegation
 *   - Conscripts and War-Affected Citizens (Powerless/Trapped): Primary victim — conscription and military obligation without formal democratic war authorization; maximum extraction with no exit
 *   - Congress as Collective Institution (Powerless/Trapped from institutional perspective): The constitutional war-powers authority itself becomes a hollow power — retained nominally but extracted in practice
 *   - Constitutional Reform Coalition (Organized/Mobile): Tertiary actor — advocates for rebalancing through constitutional or statutory mechanisms; represents exit path (scaffold perspective)
 *   - International Law Framework (Institutional/Arbitrage): Nominal constraint appearing as coordinate through performative invocation; actual enforcement degraded (piton perspective)
 *   - Analytical Observer (Analytical/Analytical): Civilizational perspective risking naturalization of contingent power arrangement as immutable necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(presidential_war_powers_usurpation, 0.68).
domain_priors:suppression_score(presidential_war_powers_usurpation, 0.72).
domain_priors:theater_ratio(presidential_war_powers_usurpation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(presidential_war_powers_usurpation, extractiveness, 0.68).
narrative_ontology:constraint_metric(presidential_war_powers_usurpation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(presidential_war_powers_usurpation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(presidential_war_powers_usurpation, snare).
narrative_ontology:human_readable(presidential_war_powers_usurpation, "Presidential War Powers Usurpation and Congressional Erosion").
narrative_ontology:topic_domain(presidential_war_powers_usurpation, "constitutional_law/political_power/executive_authority").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(presidential_war_powers_usurpation, executive_branch_institutional_actors).
narrative_ontology:constraint_victim(presidential_war_powers_usurpation, congressional_authority).
narrative_ontology:constraint_victim(presidential_war_powers_usurpation, democratic_accountability).
narrative_ontology:constraint_victim(presidential_war_powers_usurpation, war_powers_constraint).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POWERLESS CONSCRIPT / WAR POWERS VICTIM (SNARE) — Soldiers and citizens conscripted or affected by military actions authorized without congressional declaration face maximum extraction with no exit option. The constraint extracts life, limb, and resources through wars initiated without formal congressional authorization. No alternative pathway; complete suppression of exit.
constraint_indexing:constraint_classification(presidential_war_powers_usurpation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGRESS AS RESIDUAL INSTITUTION (TANGLED ROPE) — Congress retains nominal war powers authority but faces high barriers to exercise: institutional inertia, executive information asymmetry, war-powers resolution procedures that favor delay, and electoral cost of appearing 'unpatriotic.' Congress experiences both coordination function (emergency response capability) and asymmetric extraction (authority delegated away). Constrained exit because legislative override is technically possible but carries high political cost.
constraint_indexing:constraint_classification(presidential_war_powers_usurpation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE BRANCH INSTITUTIONAL ACTORS (ROPE) — Presidents, military command, and defense apparatus experience the constraint as pure coordination: rapid response to security threats without legislative delay. Arbitrage exit available — executive can act unilaterally if Congress fails to constrain. Net beneficiary; extractiveness runs toward the executive.
constraint_indexing:constraint_classification(presidential_war_powers_usurpation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL TREATY FRAMEWORK (PITON) — NATO, UN Charter, and mutual defense obligations nominally constrain unilateral war powers, but enforcement is performative. The international legal constraint persists as theatrical invocation (treaty preambles, UN briefings) while substantive enforcement has atrophied. Theater ratio high because treaty procedures continue without changing presidential behavior.
constraint_indexing:constraint_classification(presidential_war_powers_usurpation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From civilizational/universal perspective, some executive war-powers expansion appears natural or inevitable: emergencies require rapid response, legislative processes are slow, modern warfare is continuous. This perspective risks naturalizing what is a constitutional design choice. The engine's false summit detector identifies this as naturalization of contingent institutional arrangements rather than immutable law.
constraint_indexing:constraint_classification(presidential_war_powers_usurpation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORM COALITION (SCAFFOLD) — Organized actors (judiciary, constitutional scholars, reform movements) perceive the war-powers imbalance as a temporary institutional pathology with potential sunset through constitutional amendment, Supreme Court enforcement, or statutory rebalancing (e.g., renewed War Powers Resolution enforcement). Mobile exit via constitutional reform represents a genuine sunset path, though currently unrealized. Low effective extraction because the coalition has structural agency and a plausible exit timeline (generational scope).
constraint_indexing:constraint_classification(presidential_war_powers_usurpation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(presidential_war_powers_usurpation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(presidential_war_powers_usurpation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(presidential_war_powers_usurpation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(presidential_war_powers_usurpation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(presidential_war_powers_usurpation, TR),
    TR >= 0.70.

:- end_tests(presidential_war_powers_usurpation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.68): High but not maximal. The executive extracts war-authorization authority and maintains unilateral decision-making in military operations. However, extractiveness is not at snare ceiling (0.85+) because Congress retains formal authority (can declare war, control appropriations) and does occasionally exercise constraint (Syria AUMF refusal, Iraq War resistance by some legislators). The extraction is substantial but not total — Congress's nominal authority still functions as a speed bump, not a complete bar. Suppression (0.72): High. Multiple mechanisms suppress congressional war-powers reassertion: (a) Information asymmetry — executive controls intelligence briefings and threat assessments; (b) Institutional inertia — war-powers procedures favor action over deliberation; (c) Electoral cost — voting against military action carries political risk; (d) Fait accompli — executive acts first, seeks authorization after (or not at all); (e) International commitment lock-in — treaty obligations and alliance structures constrain congressional options. Theater Ratio (0.58): Moderate-high. Congressional debate, war powers resolutions, military briefings, and constitutional invocation of checks-and-balances are all theatrical — procedures occur but lack substantive enforcement power. However, theater is not dominant (not piton-range 0.70+) because some congressional exercises of power do constrain (appropriations cuts, AUMF revisions, oversight investigations). The constraint shows erosion of function, not complete hollowing.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival divergence. From the executive's position, war-powers usurpation is a coordination mechanism (Rope) — rapid response to security threats without legislative obstruction. From Congress's position, it's mixed extraction and residual coordination (Tangled Rope) — Congress retains some leverage but faces suppression. From conscripts' position, it's pure extraction (Snare) — military obligation imposed without democratic authorization and no exit option. From the international law framework, it appears performatively maintained (Piton) — treaty obligations invoked rhetorically while substantively overridden. From constitutional reformers, it appears as a temporary institutional pathology with sunset potential (Scaffold) — rebalancing through constitutional amendment or statutory enforcement. From the civilizational analytical observer, it risks appearing as an immutable necessity (Mountain) — modern warfare requires rapid executive response — but the structural data shows this as naturalization rather than natural law. The perspectival gaps reveal that the constraint's 'necessity' is observer-dependent: what is coordination (rope) from the executive is extraction (snare) from the conscript.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from beneficiary/victim status and exit options. The executive and military apparatus are beneficiaries with arbitrage exit (can act unilaterally) — they experience low or negative d (around 0.05-0.25), producing negative or minimal effective extraction from their perspective. Congress is nominally powerful but structurally constrained — it experiences moderate d (around 0.50-0.65) reflecting its mixed position as both participant in the system and victim of its erosion. Conscripts and citizens are victims with no exit (trapped status) — they experience high d (around 0.90-0.95), producing maximal experienced extraction. The derived d values explain why the beneficiary (executive) sees coordination (Rope) while the victim (conscript) sees extraction (Snare) — identical base extractiveness (0.68) but radically different f(d) multipliers produce different classifications. Congress's constrained exit produces an intermediate classification (Tangled Rope) reflecting its partial agency.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: This constraint demonstrates that 'war powers usurpation' is not a single type but a presheaf of six types over different structural positions. The mandatrophy resolves by recognizing that (1) from the executive/military perspective, the constraint functions as genuine coordination (Rope) — it solves the real coordination problem of rapid military response without legislative delay; (2) from Congress's perspective, the same constraint is mixed coordination and extraction (Tangled Rope) because Congress benefits from emergency-response capability but loses authority asymmetrically; (3) from conscripts' perspective, it is pure extraction (Snare) because they bear costs (conscription, war risk) with no authorization input. No single type is correct — the observable-dependent classification IS the answer. The false summit detector flags the mountain perspective as naturalization: the 'immutability' of executive war powers is not a law of nature but a contingent institutional arrangement sustained by electoral incentives, information asymmetry, and constitutional ambiguity. The scaffold perspective (constitutional reform with sunset) is plausible but currently unrealized — its classification as temporary is aspirational unless concrete reform moves are underway. The snare classification from the powerless perspective is the diagnostic entry point: why are conscripts classified as snare? Because they face maximum extraction (war participation) with trapped exit and no coordination benefit. This classification survives scrutiny under all alternatives (constrained vs trapped, identity_locked vs trapped for conscripts) — the snare is robust.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergency_threshold_ambiguity,
    'What constitutes a genuine emergency requiring unilateral executive action vs. a manufactured urgency to bypass congressional oversight?',
    'Empirical analysis of presidential war-powers claims and subsequent threat assessment; comparison of declared emergency severity with actual conflict scale; tracking of ''emergency'' conflicts that become long-term entanglements',
    'If threshold is loose: legitimate emergencies justify constitutional deviation; constraint functions as safety valve (Rope). If threshold is tight: most unilateral actions are extraction masquerading as emergency; constraint is Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_threshold_ambiguity, conceptual, 'Ambiguity between emergency response and manufactured urgency').

omega_variable(
    congressional_collusion_vs_abdication,
    'To what extent has Congress voluntarily delegated war powers vs. been excluded from the process by executive fait accompli?',
    'Historical analysis of congressional voting patterns on war-powers authorizations; tracking of executive actions taken before congressional notification; analysis of legislative resistance and override attempts; voting-bloc analysis separating strategic delegation from strategic incapacity',
    'If mostly delegation: constraint is Tangled Rope (both agents participating in coordination failure). If mostly fait accompli: constraint is Snare (executive unilateral extraction). If mixed with time-varying ratio: decompose into separate stories with different epsilon values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congressional_collusion_vs_abdication, empirical, 'Whether Congress delegated powers or was bypassed by executive fact-creation').

omega_variable(
    constitutional_amendment_feasibility,
    'Is constitutional rebalancing of war powers plausible in the current political context, or is the scaffold perspective aspirational rather than structural?',
    'Analysis of amendment likelihood via Article V; tracking of state-level constitutional convention movements; assessment of supermajority political will; comparison with other constitutional corrections (e.g., 22nd Amendment timeline post-FDR)',
    'If feasible: scaffold classification confirmed with realistic sunset. If infeasible: scaffold is theater masking permanent snare; reclassify from lower perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_amendment_feasibility, empirical, 'Whether constitutional amendment to rebalance war powers is plausible').

omega_variable(
    international_constraint_enforcement,
    'Do international law constraints (UN Charter, NATO treaty obligations) actually constrain presidential war decisions, or are they purely performative invocations?',
    'Empirical analysis of presidential compliance with international law constraints; tracking of cases where international law was cited as binding vs. overridden; UN General Assembly/Security Council enforcement actions; ally pressure and defection patterns',
    'If constraining: international law provides genuine secondary check (Rope from international perspective). If performative: international constraint is piton (theater without function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_constraint_enforcement, empirical, 'Whether international law constraints have enforcement force').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(presidential_war_powers_usurpation, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pwpu_tr_t0, presidential_war_powers_usurpation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pwpu_tr_t25, presidential_war_powers_usurpation, theater_ratio, 25, 0.5).
narrative_ontology:measurement(pwpu_tr_t50, presidential_war_powers_usurpation, theater_ratio, 50, 0.58).
narrative_ontology:measurement(pwpu_tr_t75, presidential_war_powers_usurpation, theater_ratio, 75, 0.62).

% Extraction over time
narrative_ontology:measurement(pwpu_be_t0, presidential_war_powers_usurpation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pwpu_be_t25, presidential_war_powers_usurpation, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(pwpu_be_t50, presidential_war_powers_usurpation, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(pwpu_be_t75, presidential_war_powers_usurpation, base_extractiveness, 75, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(presidential_war_powers_usurpation, enforcement_mechanism).
narrative_ontology:affects_constraint(presidential_war_powers_usurpation, congressional_delegation_pathologies).
narrative_ontology:affects_constraint(presidential_war_powers_usurpation, military_industrial_complex_extraction).

% DUAL FORMULATION NOTE:
% Presidential war powers usurpation is upstream of two downstream constraints: (1) congressional delegation pathologies — the mechanisms by which Congress voluntarily abdicates authority (separate story with lower extractiveness, high theater); (2) military-industrial complex extraction — the resource capture enabled by war-powers asymmetry (separate story with potentially higher extractiveness). This story models the constitutional authority transfer mechanism itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(presidential_war_powers_usurpation, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
