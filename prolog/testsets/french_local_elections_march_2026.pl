% ============================================================================
% CONSTRAINT STORY: france_local_elections_march_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_france_local_elections_march_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: france_local_elections_march_2026
 * human_readable: March 2026 French Municipal Elections
 * domain: political
 * * SUMMARY:
 * The March 15 and 22, 2026, municipal elections serve as a temporary
 * structural "Scaffold". They provide a necessary support framework for
 * political realignment, specifically for coalitions like the "Left-Wing
 * Primary" and the "Republican Front," which must hold until the post-election
 * sunset. The system also contains extractive elements, particularly for
 * independent candidates in small towns.
 * * KEY AGENTS:
 * - Independent Candidates: Subjects (Powerless)
 * - Organized Political Parties: Beneficiaries (Institutional/Organized)
 * - Systems Auditor: Observer (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Base extractiveness: Low-Moderate (0.35)
% Rationale: Local elections are high-coordination events, but the
% "Majority Premium" (a 25% seat bonus for the winning list) extracts
% disproportionate representation.
domain_priors:base_extractiveness(france_local_elections_march_2026, 0.35).

% Suppression score: Moderate (0.45)
% Rationale: The mandatory list system for small towns (<1,000 residents)
% suppresses individual/independent candidates in favor of party-list coordination.
domain_priors:suppression_score(france_local_elections_march_2026, 0.45).

% Theater Ratio: Moderate (0.30)
% Rationale: Real policy stakes (housing, security, transport) are decided,
% preventing this from being a pure Piton.
domain_priors:theater_ratio(france_local_elections_march_2026, 0.30).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(france_local_elections_march_2026, extractiveness, 0.35).
narrative_ontology:constraint_metric(france_local_elections_march_2026, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(france_local_elections_march_2026, theater_ratio, 0.30).

% Constraint self-claim: The system is presented as a necessary constructed
% framework for democratic representation.
narrative_ontology:constraint_claim(france_local_elections_march_2026, tangled_rope).
narrative_ontology:human_readable(france_local_elections_march_2026, "March 2026 French Municipal Elections").

% Binary flags
narrative_ontology:has_sunset_clause(france_local_elections_march_2026).
domain_priors:requires_active_enforcement(france_local_elections_march_2026).

% Structural property derivation hooks:
% Beneficiaries are the parties that leverage the system for coordination.
narrative_ontology:constraint_beneficiary(france_local_elections_march_2026, organized_political_parties).
% Victims are those excluded by the mandatory list system.
narrative_ontology:constraint_victim(france_local_elections_march_2026, independent_local_candidates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE INDEPENDENT CANDIDATE (SNARE)
% The new mandatory list system blocks individual entries in small communes.
% χ = 0.35 * π(powerless, 1.5) * σ(local, 0.8) = 0.42. Borderline, but high
% suppression (0.45) pushes it to Snare for the trapped agent.
constraint_indexing:constraint_classification(france_local_elections_march_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE INSTITUTIONAL PARTY (ROPE)
% A coordination tool for "counting mayors" and building the 2027 presidential base.
% χ = 0.35 * π(institutional, -0.2) = -0.07. Felt as pure coordination.
constraint_indexing:constraint_classification(france_local_elections_march_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE POLITICAL ARCHITECT (SCAFFOLD)
% Temporary coordination that sunsets immediately after the second round.
% χ = 0.35 * π(organized, 0.4) * σ(national, 1.0) = 0.14. Low felt extraction.
constraint_indexing:constraint_classification(france_local_elections_march_2026, scaffold,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(france_local_elections_march_2026).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The system has a clear coordination function (beneficiaries), asymmetric
% extraction (victims), and requires active enforcement. This is the definition
% of a Tangled Rope.
% χ = 0.35 * π(analytical, 1.15) * σ(global, 1.2) = 0.483.
constraint_indexing:constraint_classification(france_local_elections_march_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(france_local_elections_march_2026_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(france_local_elections_march_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(france_local_elections_march_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(scaffold_sunset_clause) :-
    % Ensure Scaffold classification is valid only under the sunset clause.
    narrative_ontology:has_sunset_clause(france_local_elections_march_2026),
    constraint_indexing:constraint_classification(france_local_elections_march_2026, scaffold, context(agent_power(organized),_,_,_)).

test(tangled_rope_conditions_met) :-
    % Verify the analytical observer correctly identifies a Tangled Rope.
    domain_priors:requires_active_enforcement(france_local_elections_march_2026),
    narrative_ontology:constraint_beneficiary(france_local_elections_march_2026, _),
    narrative_ontology:constraint_victim(france_local_elections_march_2026, _),
    constraint_indexing:constraint_classification(france_local_elections_march_2026, tangled_rope, context(agent_power(analytical),_,_,_)).

:- end_tests(france_local_elections_march_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 2026 local elections are a classic hybrid constraint. For political
 * architects, they are a 'Scaffold' providing the 500-mayoral endorsements
 * required for the 2027 presidential race. Once the second round concludes,
 * the coordination lists dissolve or harden, fulfilling the sunset condition.
 * For institutional parties, it's a pure 'Rope' for consolidating power.
 * However, for independent candidates in small towns, the mandatory list
 * system is a 'Snare' that suppresses their participation.
 *
 * PERSPECTIVAL GAP:
 * The gap is stark: institutional parties see a coordination tool (Rope),
 * while powerless independents see an extractive barrier (Snare). The
 * analytical view resolves this by classifying it as a 'Tangled Rope'—a
 * system with both genuine coordination benefits and asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The system avoids 'Piton' status because of the high local policy weight.
 * The 'Tangled Rope' classification from the analytical perspective correctly
 * captures the dual nature of the system, preventing a misclassification as
 * either pure coordination (Rope) or pure extraction (Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_france_local_elections_march_2026,
    'Will LFI maintain a separate list in the second round in Paris, splitting the left-wing vote?',
    'Second-round alliance results published between March 16-22, 2026.',
    'If True, right-wing (Dati) victory probability increases significantly. If False, a unified Left likely wins Paris.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(france_local_elections_march_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required for this constraint.
% The base_extractiveness (0.35) is below the 0.46 threshold that mandates
% lifecycle drift tracking.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: Elections are a primary mechanism for allocating the
% resource of political power and representation.
narrative_ontology:coordination_type(france_local_elections_march_2026, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */