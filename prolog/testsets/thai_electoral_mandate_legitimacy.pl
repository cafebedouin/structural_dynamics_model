% ============================================================================
% CONSTRAINT STORY: thai_electoral_mandate_legitimacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-12-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thai_electoral_mandate_legitimacy, []).

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
 *   constraint_id: thai_electoral_mandate_legitimacy
 *   human_readable: Thai Electoral Mandate Legitimacy
 *   domain: political_governance/electoral_systems
 *
 * SUMMARY:
 *   Thai electoral legitimacy operates as a hybrid coordination-extraction
 *   mechanism. Elections genuinely aggregate elite factional preferences and
 *   manage succession disputes among competing power centers (military,
 *   monarchy-protective coalitions, business networks, bureaucratic
 *   hierarchies). Yet electoral outcomes are systematically constrained by
 *   military veto, constitutional engineering that nullifies left-wing or
 *   redistribute-oriented governments, and super-majority requirements that
 *   prevent single-party mandate translation into policy authority. The
 *   constraint exhibits all characteristics of a tangled rope: it performs a
 *   real coordination function (elite succession management) while extracting
 *   from democratic constituencies (subordinating mass electoral preferences
 *   to elite prerogatives). Base extractiveness increased from 0.42 to 0.58
 *   over the measurement interval, driven by successive constitutional
 *   changes (2017 Constitution's appointed Senate, 2019 electoral law) that
 *   more explicitly encoded military veto into formal law rather than leaving
 *   it as extra-constitutional override threat. Theater ratio increased from
 *   0.48 to 0.64, reflecting that electoral procedures are increasingly
 *   formalized and ritualized as constitutional engineering does more of the
 *   veto work previously done by military intervention. The system has become
 *   more institutionally entrenched but less plausibly democratic — the
 *   extraction mechanism is now formalized.
 *
 * KEY AGENTS:
 *   - Military institutional actors: Primary beneficiary (institutional/arbitrage) — experience electoral system as coordinating mechanism that manages elite factionalism while protecting core military prerogatives and palace-protective mandates
 *   - Monarchy-protective coalitions: Primary beneficiary (institutional/arbitrage) — benefit from electoral system's capacity to nullify governments perceived as monarchy-threatening; see electoral system as security mechanism for institutional continuity
 *   - Democratic legitimacy principle: Primary victim (powerless/trapped) — abstract collective good that cannot exit the electoral system; bears cost of systematically overridden mandates
 *   - Electoral participation constituencies: Primary victim (powerless/trapped) — voters participate in electoral procedure but see outcomes nullified by military coup (2006, 2014) or constitutional constraints; trapped because voting is socially expected but meaningless
 *   - Pro-democracy civil society: Secondary victim (moderate/constrained) — can organize, contest, and influence within constraints but face constitutional veto on winning coalition scope and military coup risk
 *   - Elected government actors: Hybrid position (powerful/constrained) — benefit from electoral victory but constrained by appointed Senate, constitutional limits on executive scope, and coup risk
 *   - International democratic governance actors: Conditional observer (organized/mobile) — treat Thai democracy as temporarily degraded institution with implicit sunset (pressure for democratic transition) but maintain engagement
 *   - Analytical observer: Civilizational view (analytical/analytical) — sees system as functional tangled rope (elite coordination + mass extraction) rather than false democracy or pure autocracy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thai_electoral_mandate_legitimacy, 0.58).
domain_priors:suppression_score(thai_electoral_mandate_legitimacy, 0.68).
domain_priors:theater_ratio(thai_electoral_mandate_legitimacy, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thai_electoral_mandate_legitimacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(thai_electoral_mandate_legitimacy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(thai_electoral_mandate_legitimacy, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thai_electoral_mandate_legitimacy, tangled_rope).
narrative_ontology:human_readable(thai_electoral_mandate_legitimacy, "Thai Electoral Mandate Legitimacy").
narrative_ontology:topic_domain(thai_electoral_mandate_legitimacy, "political_governance/electoral_systems").

domain_priors:requires_active_enforcement(thai_electoral_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(thai_electoral_mandate_legitimacy, military_institutional_actors).
narrative_ontology:constraint_beneficiary(thai_electoral_mandate_legitimacy, monarchy_protective_coalitions).
narrative_ontology:constraint_beneficiary(thai_electoral_mandate_legitimacy, conservative_bureaucratic_networks).
narrative_ontology:constraint_victim(thai_electoral_mandate_legitimacy, democratic_legitimacy_principle).
narrative_ontology:constraint_victim(thai_electoral_mandate_legitimacy, electoral_participation_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED VOTER (SNARE) — Voters face military annulment of election results (2006, 2014) and constitutional engineering that nullifies electoral outcomes. Multiple constitutional changes and electoral law modifications eliminate meaningful exit or alternative. Suppression is maximum: voting is permitted but outcomes are overridden by extra-constitutional force. The voter is trapped within a system that performs democratic procedure while ensuring their mandate has no binding effect. Pure extraction from the powerless perspective — they bear the cost of performance without receiving legitimacy delivery.
constraint_indexing:constraint_classification(thai_electoral_mandate_legitimacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRO-DEMOCRACY ELECTORAL COALITION (TANGLED ROPE) — These agents (opposition parties, civil society groups, urban professional classes) benefit from the electoral system's coordination function: it provides a mechanism for collective preference aggregation and peaceful contestation. Yet they suffer from extraction through mandate nullification and constitutional constraints on winning coalitions. They are constrained by military veto power, not trapped — they retain ability to organize and contest, but at significant cost. The hybrid classification reflects both coordination value (the system does aggregate preferences) and structural extraction (outcomes are pre-constrained).
constraint_indexing:constraint_classification(thai_electoral_mandate_legitimacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MILITARY INSTITUTIONAL BENEFICIARIES (ROPE) — From the military's perspective, the electoral system serves coordination: it manages factional rivalry among competing elites, aggregates narrow-interest preferences (palace-protective coalitions, state-enterprise interests), and provides the legitimacy theater required for international recognition and economic stability. The military experiences the system as pure coordination — it solves the elite collective action problem of how to manage succession and elite contestation without threatening core institutional prerogatives. No binding extraction flows toward the military; they are net extractors who experience the system as genuinely functional.
constraint_indexing:constraint_classification(thai_electoral_mandate_legitimacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ELECTED GOVERNMENT ACTORS (TANGLED ROPE) — Winning coalition government members benefit from the electoral victory (executive power, patronage, policy implementation). Yet they are constrained by constitutional limits on executive scope, veto by appointed/military-influenced upper chambers, and threat of coup or extra-constitutional removal. This creates a hybrid: genuine coordination function (elections do produce governments that govern) mixed with significant extraction (scope constraints, veto points, tenure uncertainty). Elected governments see themselves as legitimate but constrained, not fully sovereign. Exit is constrained by the military veto, not foreclosed.
constraint_indexing:constraint_classification(thai_electoral_mandate_legitimacy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL DEMOCRATIC GOVERNANCE COALITION (SCAFFOLD) — International actors (ASEAN, democratic governments, development institutions) maintain engagement with Thai electoral processes as a temporary coordination mechanism with implicit sunset logic. This perspective treats Thai democracy as a degraded but recoverable institution. International conditionality, sanctions, and recognition contingency function as enforcement mechanisms that decay in salience if democratic norms do not progress. Scope is regional because neighboring democracies' internal variation affects the credibility of democratic conditionality. Theater is moderate — the international community performs democratic concern while accepting substantial constraint on enforcement.
constraint_indexing:constraint_classification(thai_electoral_mandate_legitimacy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: CONSTITUTIONAL ARCHITECTURE MAINTENANCE (PITON) — The formal constitutional framework provides electoral procedures, electoral commission structures, and legal scaffolding for elections. These institutions maintain a theater of electoral legitimacy (campaigns, voting, result tallying) despite the core function (translating votes into binding executive authority) being substantially degraded by military veto and super-majority constraints. The constitution persists through institutional inertia — successive constitutions since 1932 perform similar rituals without fundamentally shifting power distribution. Theater is high because the constitutional apparatus is maintained despite being systematically overridden.
constraint_indexing:constraint_classification(thai_electoral_mandate_legitimacy, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational analytical view, the Thai electoral mandate constraint involves genuine coordination (aggregating elite factional preferences, managing succession disputes) mixed with substantial extraction (subordinating mass electoral mandates to elite prerogatives, constraining democratic scope). The system is neither pure democracy nor pure autocracy but a hybrid that coordinates elite contestation while extracting legitimacy from popular participation. Classification: tangled_rope reflects this structural duality at the analytical level. The constraint's primary function is elite coordination; extraction is a side effect of this coordination, not its primary purpose — but extraction is nonetheless systemically large.
constraint_indexing:constraint_classification(thai_electoral_mandate_legitimacy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thai_electoral_mandate_legitimacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(thai_electoral_mandate_legitimacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thai_electoral_mandate_legitimacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(thai_electoral_mandate_legitimacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(thai_electoral_mandate_legitimacy, TR),
    TR >= 0.70.

:- end_tests(thai_electoral_mandate_legitimacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The electoral system extracts from democratic constituencies by subordinating mass preferences to elite veto, but the extraction is not maximal because (1) elections do produce governments that govern within constrained scope, (2) elite coordination genuinely requires managed contestation rather than centralized dictate, and (3) the system is not purely extractive — it does solve elite succession problems. If the system were pure extraction (0.75+), elections would be theater only; instead, they produce real if constrained governments. Suppression (0.68): High. Voters face multiple barriers: military veto threat, constitutional nullification of certain winning coalitions, appointed upper chamber veto, electoral law engineering (district size, proportionality changes), and expectation that dissenting votes may trigger elite backlash. These are structural constraints, not material physical barriers, but suppression is nonetheless high. Theater ratio (0.64): Elevated. Electoral campaigns, voting procedures, and formal tallying are performed with full procedural legitimacy theater. Yet the core function (translating votes into binding executive authority) is substantially constrained by military oversight and constitutional rules. The gap between electoral performance and actual mandate effect is substantial. Theater has risen over the measurement interval as constitutional engineering has formalized what was previously extra-constitutional military veto — the system's legitimacy theater increases as its actual democratic function decreases.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits stark perspectival divergence driven by agents' structural relationship to the extraction flow. The military institutional beneficiary sees pure coordination (rope) — the electoral system solves their collective action problem of elite succession without requiring military dictatorship. The powerless voter sees pure extraction (snare) — they perform democratic participation while their mandate is nullified. The elected government sees hybrid constraint (tangled rope) — they benefit from electoral victory but are severely constrained by military veto. The international observer sees a temporary scaffold — democratic norms are degraded but recoverable if international pressure accumulates. The analytical observer at civilizational scope sees the system's true structure: tangled rope coordinating elite factionalism while extracting from democratic legitimacy. The perspectival gap reveals that the system works perfectly for the military (coordinates their succession), works partially for elected governments (constrains but allows some governing), fails completely for democratic voters (mandates are nullified), and appears as temporary degradation to international actors (who assume eventual democratization).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position. Military institutional beneficiaries with arbitrage exit options have low d (0.10–0.20) — they benefit from the constraint, so f(d) produces negative or near-zero effective extraction (chi). Democratic constituencies with trapped exit and victim status have high d (0.90–0.98) — they cannot exit and bear extraction costs, so f(d) produces maximum experienced extractiveness (chi ≈ 1.42). Elected governments with powerful/constrained positioning have moderate-high d (0.55–0.65) — they benefit from election but are constrained by veto, so f(d) ≈ 0.75, producing chi slightly higher than base ε. International observers with organized/mobile status have moderate d (0.65–0.75) — they can withdraw engagement but choose not to, producing f(d) ≈ 1.00. Analytical observers have canonical d ≈ 0.73, producing f(d) ≈ 1.15. The derivation chain shows how the same constraint (ε = 0.58) produces experienced extractiveness ranging from negative (military beneficiaries) to maximum (trapped voters), entirely determined by structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The Thai electoral mandate constraint resolves mandatrophy by demonstrating that coordination and extraction are not mutually exclusive categories but co-present functions with different distributions across agent perspectives. The system does coordinate elite factionalism (prevents military dictatorship, manages succession, enables inter-elite contestation) — this is the coordination claim that would justify a rope classification. Yet it simultaneously extracts from democratic constituencies by subordinating mass mandates to elite prerogatives — this is the extraction that prevents a rope classification and mandates tangled rope. The mandatrophy resolution: both claims are structurally true. The system is a tangled rope because it performs real coordination while extracting real value. False natural law detection: the analytical observer might naturalize this as 'democracy always requires elite constraint' or 'succession management always requires veto systems.' But the structural data reveals that the constraints are contingent institutional choices (appointed senate could be elected, military veto could be constitutional rather than extra-constitutional, electoral laws could protect rather than nullify mandates). The naturalization would be false. The system is a tangled rope, not a mountain — it could be redesigned to reduce extraction without losing coordination function (e.g., elected senate, military subordination to constitutional bounds, direct translation of electoral mandates to executive scope). The current form extracts more than strictly necessary for elite succession coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    military_veto_mechanism_nature,
    'Is military veto over electoral outcomes a coordinating mechanism for elite stability (Rope) or an extraction mechanism subordinating electoral legitimacy to military prerogative (Snare)?',
    'Analysis of military intervention patterns: does military veto target specific winning ideologies, or does it structure the rules uniformly? Do coup threats occur symmetrically against all governments, or disproportionately against pro-democracy/pro-redistribution governments?',
    'If symmetric veto: coordination function (rope-classified from all perspectives). If asymmetric targeting: extraction mechanism (snare dominates, tangled rope downgraded). Current evidence suggests asymmetry — military intervenes primarily against elected governments perceived as monarchy-threatening or elite-redistributive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(military_veto_mechanism_nature, empirical, 'Whether military veto is coordinating mechanism or extraction mechanism').

omega_variable(
    voter_awareness_of_mandate_nullification,
    'Do Thai voters participate in elections understanding that electoral outcomes may be nullified by military/constitutional action, or do they approach voting under a belief in electoral mandate binding?',
    'Survey data on voter expectations and mandate beliefs; linguistic analysis of democratic discourse; comparison of voting participation rates with stated beliefs about election binding',
    'If voters understand nullification risk and vote anyway: electoral participation is performative (theater rises, extraction becomes more severe). If voters believe in mandate binding: electoral participation is genuinely motivated by democratic expectation (theater lower, extraction less conscious). This shifts the suppression vector.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voter_awareness_of_mandate_nullification, empirical, 'Voter cognition of electoral mandate binding versus nullification risk').

omega_variable(
    elite_factional_stability_alternative,
    'Could elite factional succession be managed through institutional mechanisms other than electoral aggregation (e.g., formal elite councils, rotation agreements, party systems with explicit veto coalitions)?',
    'Comparative analysis of non-electoral elite management systems (consultative monarchy traditions, palace council precedents, explicit veto coalition contracts); counterfactual modeling of elite conflict without electoral system',
    'If alternatives exist and could function: electoral system is not necessary coordination mechanism, only sufficient one — extraction component becomes primary. If no functional alternatives: electoral system''s coordination value is non-negotiable, and extraction is cost of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elite_factional_stability_alternative, conceptual, 'Whether electoral system is necessary for elite succession coordination').

omega_variable(
    democracy_legitimacy_circularity,
    'Does the electoral system''s coordination function depend on democratic legitimacy (voters participate because they believe in democratic process), creating circular dependency where extraction undermines coordination?',
    'Longitudinal voter participation data; analysis of voting motivation shifts in relation to mandate nullifications; measurement of trust in electoral system over time',
    'If circular: high extraction degrades coordination function over time (system becomes snare-dominated as legitimacy erodes). If independent: coordination can persist despite extraction (tangled rope stable). Current evidence shows declining voter participation in some constituencies post-coup, suggesting circularity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democracy_legitimacy_circularity, empirical, 'Whether electoral coordination depends on democratic legitimacy beliefs').

omega_variable(
    international_conditionality_enforcement_strength,
    'How binding are international sanctions and conditionality on Thai democratic progress? Does the scaffold perspective''s sunset logic hold (international pressure forces democratic transition) or is it aspirational?',
    'Analysis of ASEAN sanctions patterns; correlation between international pressure and democratic reform pace; assessment of economic cost of isolation versus benefit of veto retention for ruling elite',
    'If binding: scaffold classification holds and democratic sunset is structural (20-30 year transition horizon). If non-binding: international constraints are theatrical, scaffold is misclassified, and tangled rope persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_conditionality_enforcement_strength, empirical, 'Whether international conditionality enforces democratic transition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thai_electoral_mandate_legitimacy, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(thai_electoral_tr_t0, thai_electoral_mandate_legitimacy, theater_ratio, 0, 0.48).
narrative_ontology:measurement(thai_electoral_tr_t3, thai_electoral_mandate_legitimacy, theater_ratio, 3, 0.58).
narrative_ontology:measurement(thai_electoral_tr_t6, thai_electoral_mandate_legitimacy, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(thai_electoral_be_t0, thai_electoral_mandate_legitimacy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(thai_electoral_be_t3, thai_electoral_mandate_legitimacy, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(thai_electoral_be_t6, thai_electoral_mandate_legitimacy, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thai_electoral_mandate_legitimacy, resource_allocation).
narrative_ontology:affects_constraint(thai_electoral_mandate_legitimacy, thai_coalition_government_formation).
narrative_ontology:affects_constraint(thai_electoral_mandate_legitimacy, thai_senate_veto_mechanism).
narrative_ontology:affects_constraint(thai_electoral_mandate_legitimacy, thai_monarchy_protection_doctrine).

% DUAL FORMULATION NOTE:
% Thai electoral legitimacy is upstream of specific institutional veto mechanisms (senate override, military coup threat). Each downstream constraint has its own ε value reflecting the specific veto mechanism's extractiveness. Electoral legitimacy provides the aggregation layer that feeds into these veto structures. Decomposition: electoral legitimacy (ε=0.58, tangled rope) coordinates elite factionalism while extracting from democratic mandates. The downstream constraints (senate veto ε≈0.65, military coup threat ε≈0.70) are more purely extractive because they operate without meaningful coordination function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(thai_electoral_mandate_legitimacy, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
