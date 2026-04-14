% ============================================================================
% CONSTRAINT STORY: voting_system_path_dependence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_voting_system_path_dependence, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: voting_system_path_dependence
 *   human_readable: Voting System Path Dependence and Electoral Lock-In
 *   domain: political_institutions/electoral_systems
 *
 * SUMMARY:
 *   Voting system path dependence creates a structural lock-in where initial
 *   institutional choices (adoption of plurality rule) generate incentive
 *   structures that prevent their own replacement, even when collective
 *   preference would support alternatives. The constraint exhibits classic
 *   tangled-rope characteristics: it coordinates voter aggregation and
 *   produces decisive outcomes (genuine coordination function) while
 *   simultaneously suppressing third-party expression and trapping voters
 *   into strategic misrepresentation of preferences (asymmetric extraction).
 *   The theater ratio (0.65) reflects that much contemporary debate about
 *   voting systems invokes Arrow's Impossibility Theorem and mathematical
 *   inevitability—performative arguments that naturalize plurality voting as
 *   scientifically required—when empirical evidence from jurisdictions with
 *   ranked choice, approval voting, and proportional representation systems
 *   shows these alternatives function effectively. The constraint is
 *   maintained through both institutional enforcement (ballot access rules,
 *   debate thresholds) and emergent voter behavior (spoiler fear, strategic
 *   defection), creating a mechanism that appears self-reinforcing despite
 *   genuine practical alternatives. The extractiveness has increased over the
 *   60-year interval as third-party organizing has grown more sophisticated
 *   yet remains structurally blocked, indicating accumulating rent-seeking
 *   rather than stabilized coordination.
 *
 * KEY AGENTS:
 *   - Third-Party Voters: Primary victims (powerless/trapped) — face spoiler dilemma forcing strategic abandonment of authentic preference
 *   - Democratic/Republican Party Establishments: Primary beneficiaries (institutional/arbitrage) — capture electoral advantage and coalition-building benefits from plurality rule; exit is available but undesirable
 *   - Electoral Reform Movements: Secondary victims (moderate/constrained) — structurally blocked by ballot access, debate thresholds, and judicial spoiler doctrine; see viable exit pathways in ranked choice and proportional systems
 *   - Reform Coalition: Organized agents (organized/mobile) — building exit pathways through state-level and municipal implementation; demonstrating that path dependence is contingent, not immutable
 *   - Voting Theory Establishment: Institutional maintainers (institutional/arbitrage) — Arrow's Impossibility and Condorcet-cycle theorems provide cover story for plurality rule inevitability; increasingly seen as performative rather than explanatory
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent 1800s institutional choice as eternal mathematical fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(voting_system_path_dependence, 0.52).
domain_priors:suppression_score(voting_system_path_dependence, 0.58).
domain_priors:theater_ratio(voting_system_path_dependence, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(voting_system_path_dependence, extractiveness, 0.52).
narrative_ontology:constraint_metric(voting_system_path_dependence, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(voting_system_path_dependence, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(voting_system_path_dependence, tangled_rope).
narrative_ontology:human_readable(voting_system_path_dependence, "Voting System Path Dependence and Electoral Lock-In").
narrative_ontology:topic_domain(voting_system_path_dependence, "political_institutions/electoral_systems").

domain_priors:requires_active_enforcement(voting_system_path_dependence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(voting_system_path_dependence, dominant_two_party_system).
narrative_ontology:constraint_beneficiary(voting_system_path_dependence, incumbent_office_holders).
narrative_ontology:constraint_victim(voting_system_path_dependence, third_party_movements).
narrative_ontology:constraint_victim(voting_system_path_dependence, voter_preference_expression).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THIRD-PARTY VOTER (SNARE) — Faces the spoiler dynamic: voting sincere preference mathematically increases likelihood of worst-case outcome. No viable exit from the two-party constraint without abandoning political expression. Maximum extraction: forced choice between authentic preference and strategic damage control.
constraint_indexing:constraint_classification(voting_system_path_dependence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM MOVEMENT (TANGLED ROPE) — Constrained by institutional barriers (ballot access requirements, debate thresholds, fusion ballot restrictions) but also benefits from the electoral coordination infrastructure built around plurality rule. High-cost exit (requires constitutional amendment or state-by-state reform) alongside real benefits from existing system. Active enforcement through ballot regulations and judicial decisions maintaining spoiler doctrine.
constraint_indexing:constraint_classification(voting_system_path_dependence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR PARTY ESTABLISHMENT (ROPE) — Benefits from path-dependent electoral advantage. Sees plurality voting as pure coordination mechanism: aggregating voter will into decisive outcomes. Net beneficiary with arbitrage options—can adjust campaign strategy, coalition composition, and geographically targeted messaging. Extraction flows toward this agent.
constraint_indexing:constraint_classification(voting_system_path_dependence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ELECTORAL REFORM COALITION (SCAFFOLD) — Organized actors (ranked choice advocacy organizations, state-level reform initiatives) see path dependence as a temporary governance failure with clear exit pathway. Ranked choice voting, approval voting, and multi-member proportional representation are being implemented in cities and states. The constraint has a visible sunset: as adoption spreads, the path-dependent lock weakens. Theater is moderate because reform debate is substantive even where implementation stalls.
constraint_indexing:constraint_classification(voting_system_path_dependence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: VOTING THEORY ESTABLISHMENT (PITON) — Arrow's Impossibility Theorem and Condorcet-cycle logic persist as intellectual justification for plurality voting despite empirical evidence that ranked choice and other systems function in practice. The theoretical apparatus has become performative: it naturalizes plurality rule as inevitable rather than contingent. Piton classification reflects high theater (theoretical sophistication) masking atrophied genuine function (explaining actual electoral outcomes).
constraint_indexing:constraint_classification(voting_system_path_dependence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational frame, path dependence in voting systems appears as an immutable structural feature: once a voting rule is adopted, the incentives created by that rule prevent its own replacement. Network effects, strategic behavior, and coordination problems create a self-reinforcing lock that appears unchangeable from within the system. However, this naturalizes a contingent historical choice (plurality rule's initial adoption in 1800s Britain) as a law of politics. The engine will classify this as a false summit.
constraint_indexing:constraint_classification(voting_system_path_dependence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(voting_system_path_dependence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(voting_system_path_dependence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(voting_system_path_dependence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(voting_system_path_dependence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(voting_system_path_dependence, TR),
    TR >= 0.70.

:- end_tests(voting_system_path_dependence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from third-party voters and reform movements, but extraction is not total—major parties receive genuine coordination benefits (vote aggregation, decisive outcomes) that justify some extraction. The value reflects that the extraction is embedded in a real coordination mechanism, not pure rent-seeking. However, extractiveness has grown from 0.32 to 0.52 over the interval because the gap between available alternatives (ranked choice proving successful in Maine, Minnesota, Kansas) and constrained choices in most jurisdictions indicates increasing rent-seeking layering. Suppression (0.58): Moderate-high. Suppression operates through multiple mechanisms: strategic voter behavior (spoiler fear), institutional rules (ballot access requirements), legal doctrine (spoiler theorem used to block reform), and psychological framing (inevitability narratives). Suppression is not total—exit is theoretically possible and increasingly realized through local/state reforms—but barriers are substantial. Theater ratio (0.65): Elevated. Arrow's Impossibility Theorem is invoked as mathematical proof that plurality voting is inevitable, yet the theorem does not actually establish this (it shows no voting system satisfies all five criteria, not that plurality is optimal). This performative invocation masks the contingency of the initial choice. Contemporary debate often substitutes theoretical sophistication for engagement with empirical implementation in working ranked-choice jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival gap between beneficiary and victim perspectives. The major party establishment sees coordination (Rope)—aggregating voter will into coherent policy platforms. The third-party voter sees pure extraction (Snare)—forced strategic misrepresentation. The reform coalition sees a temporary problem with visible exit (Scaffold)—ranked choice and proportional systems provide working alternatives being implemented in practice. The voting theory canon sees mathematical inevitability (Piton with high theater)—Arrow's Impossibility naturalizes plurality voting despite theoretical implications not supporting it. The analytical observer risks a false summit (Mountain)—treating contingent institutional history as unchangeable law. The gap reveals that the constraint's classification is entirely dependent on the agent's structural position relative to extraction, not on objective features of voting rule mathematics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position. Third-party voters occupy the maximum-extraction position (trapped exit, victim status) → d ≈ 0.95 → high f(d). Major party establishments occupy low-extraction position (arbitrage exit, beneficiary status) → d ≈ 0.15 → low f(d). Reform movements occupy intermediate position (constrained exit, victim status, but growing organizational power) → d ≈ 0.65 → moderate f(d). The analytical observer's mountain classification risks placing the constraint outside the directionality frame entirely, naturalizing the extraction structure as mathematical necessity rather than indexical artifact of institutional position. This is the diagnostic signal of false summits—when a perspectival classification attempts to escape the directionality derivation rather than being derived by it.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION BY PERSPECTIVAL PLURALITY: This constraint resolves mandatrophy through empirical demonstration that path dependence is contingent, not necessary. Maine's successful transition to ranked choice voting (2018-2020), Minnesota's adoption (2024), and implementation in 50+ municipalities globally proves that exit from plurality-rule path dependence is possible under known conditions (organized pressure + supermajority reform consensus + demonstrated working alternatives). This evidence transforms the mountain perspective (path dependence is immutable) into a false summit—a naturalization of contingent institutional choice. The constraint's true classification remains Tangled Rope from the analytical perspective: it coordinates electoral outcomes while suppressing preference expression, with active enforcement (ballot rules, debate thresholds) maintaining the lock. However, the scaffold perspective is increasingly validated—the reform coalition has demonstrated that sunset clauses are real (local implementation spreading, early adoption showing viability) and that the lock is reversible. Mandatrophy is resolved by this perspectival evidence: the constraint is not an eternal feature of voting mathematics but a contingent institutional arrangement whose path dependence is actively maintained and empirically reversible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    path_dependence_reversibility,
    'Is voting system path dependence truly irreversible, or are historical examples of successful reform (Maine to ranked choice, New Zealand to MMP) proving it is contingent?',
    'Comparative historical analysis of successful vs failed reform attempts; identification of conditions enabling exit from path dependence in specific jurisdictions',
    'If reversible: constraint should reclassify from Mountain/Snare toward Scaffold across perspectives. If irreversible: mountain classification gains force; reform coalition perspective becomes analytically false.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(path_dependence_reversibility, empirical, 'Whether voting system lock-in can be reversed through institutional reform').

omega_variable(
    voter_strategic_convergence_mechanism,
    'Do voters strategically abandon third-party support primarily due to spoiler fear, or do stable two-party equilibria emerge from deeper coalition-building and policy alignment dynamics?',
    'Experimental voting studies with different rule structures; analysis of voter preference distribution vs revealed behavior; jurisdictional comparisons with same voter demographics under different voting rules',
    'If spoiler-driven: suppression is primarily psychological/strategic, not structural—constraint could weaken with rule change. If equilibrium-driven: suppression is structural, persists across rule changes; extraction mechanism is deeper than institutional form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voter_strategic_convergence_mechanism, empirical, 'Source of two-party convergence in voter behavior').

omega_variable(
    institutional_enforcement_necessity,
    'How much of the path dependence requires active institutional enforcement (ballot access restrictions, debate thresholds, spoiler doctrine judicial interpretation) vs how much is self-enforcing through voter behavior alone?',
    'Removal of specific enforcement mechanisms and measurement of resulting electoral outcomes; comparison of jurisdictions with identical voting rule but different enforcement strictness',
    'If primarily institutional enforcement: removing legal barriers could rapidly destabilize the lock. If primarily self-enforcing: legal reform is necessary but not sufficient; cultural shift required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_enforcement_necessity, empirical, 'Degree of institutional vs behavioral enforcement of two-party path dependence').

omega_variable(
    beneficiary_awareness_and_intentionality,
    'Do major party establishments actively maintain path dependence through deliberate strategy, or is it an emergent side effect of rational coalition-building within existing rules?',
    'Analysis of party documents, campaign strategy materials, and lobbying efforts around ballot access and debate rules; game-theoretic modeling of party incentives under various rule structures',
    'If intentional: major party establishment should be classified with higher moral culpability and faces stronger collective action challenge to reform. If emergent: blame attribution shifts toward the voting rule structure itself, not party actors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_awareness_and_intentionality, conceptual, 'Intentionality of major party maintenance of voting system lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(voting_system_path_dependence, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vspd_tr_t0, voting_system_path_dependence, theater_ratio, 0, 0.45).
narrative_ontology:measurement(vspd_tr_t30, voting_system_path_dependence, theater_ratio, 30, 0.58).
narrative_ontology:measurement(vspd_tr_t60, voting_system_path_dependence, theater_ratio, 60, 0.65).

% Extraction over time
narrative_ontology:measurement(vspd_be_t0, voting_system_path_dependence, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(vspd_be_t30, voting_system_path_dependence, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(vspd_be_t60, voting_system_path_dependence, base_extractiveness, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(voting_system_path_dependence, resource_allocation).
narrative_ontology:affects_constraint(voting_system_path_dependence, election_finance_concentration).
narrative_ontology:affects_constraint(voting_system_path_dependence, gerrymandering_lock_in).
narrative_ontology:affects_constraint(voting_system_path_dependence, representation_deficit_spiral).

% DUAL FORMULATION NOTE:
% Voting system path dependence is upstream of election finance and representation deficit constraints. The voting rule (plurality) creates the structural conditions enabling campaign finance concentration and gerrymandering effectiveness. Separate constraint stories model these downstream mechanisms, but all share the common root cause of plurality voting's mathematical properties and their political economy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
