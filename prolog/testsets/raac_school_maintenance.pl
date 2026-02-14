% ============================================================================
% CONSTRAINT STORY: raac_school_maintenance
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-24
% ============================================================================

:- module(constraint_raac_school_maintenance, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: raac_school_maintenance
 *   human_readable: Systemic Response to RAAC Concrete Failures in UK Schools
 *   domain: economic/political
 *
 * SUMMARY:
 *   Reinforced Autoclaved Aerated Concrete (RAAC), a cheap building material
 *   used from the 1950s-1990s, is failing across hundreds of UK schools.
 *   This constraint models the systemic response: a complex, underfunded
 *   process of identification and remediation that imposes severe costs,
 *   risks, and disruptions on schools, while the central government defers
 *   the full fiscal burden. The system combines a necessary coordination
 *   function (a national infrastructure problem) with asymmetric extraction
 *   (costs are borne locally).
 *
 * KEY AGENTS (by structural relationship):
 *   - Schools, Pupils & Parents: Primary target (powerless/trapped) — bears safety risks, educational disruption, and the financial/administrative burden of remediation.
 *   - UK Central Government (Treasury/DfE): Primary beneficiary (institutional/arbitrage) — benefits from fiscal deferment by externalizing immediate, massive capital costs onto local actors and future budgets.
 *   - Local Authorities: Secondary actor (institutional/constrained) — caught between central government funding limits and direct responsibility for affected schools.
 *   - Analytical Observer: Systems analyst — sees the full structure of a coordination problem coupled with asymmetric cost-bearing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(raac_school_maintenance, 0.55). % High cost of remediation vs school budgets.
domain_priors:suppression_score(raac_school_maintenance, 0.75).   % Schools have no choice but to operate and seek designated funding; they cannot easily exit or find alternative capital.
domain_priors:theater_ratio(raac_school_maintenance, 0.40).       % Significant administrative activity (surveys, applications) that does not always translate to sufficient funding or timely repair.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(raac_school_maintenance, extractiveness, 0.55).
narrative_ontology:constraint_metric(raac_school_maintenance, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(raac_school_maintenance, theater_ratio, 0.40).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(raac_school_maintenance, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(raac_school_maintenance). % The process requires active surveys, funding applications, and management.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(raac_school_maintenance, uk_central_government).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(raac_school_maintenance, uk_local_education_authorities_and_schools).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (AFFECTED SCHOOLS)
% Agent who bears the most extraction. For them, this is a trap with no easy exit,
% imposing immense costs and risks.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
constraint_indexing:constraint_classification(raac_school_maintenance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (UK CENTRAL GOVERNMENT)
% Agent who benefits from fiscal deferment. From this perspective, the system is a
% coordination mechanism to manage a large liability over time.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
constraint_indexing:constraint_classification(raac_school_maintenance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the genuine coordination need and the severe asymmetric extraction.
% This perspective identifies the structure as a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(raac_school_maintenance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% Perspective 4: LOCAL AUTHORITIES
% An institutional actor, but one constrained by central funding. They experience
% both the coordination pressure and the extraction, but have more agency than
% a single school. Their exit options are highly constrained.
% Engine derives d from victim membership + constrained exit -> d ~0.90 -> f(d) ~1.35
constraint_indexing:constraint_classification(raac_school_maintenance, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(raac_school_maintenance_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(raac_school_maintenance, TypeTarget, context(agent_power(powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(raac_school_maintenance, TypeBeneficiary, context(agent_power(institutional), _, arbitrage, _)),
    assertion(TypeTarget == snare),
    assertion(TypeBeneficiary == rope),
    TypeTarget \= TypeBeneficiary.

test(perspectival_gap_inter_institutional) :-
    % Verify gap between the two institutional actors.
    constraint_indexing:constraint_classification(raac_school_maintenance, TypeGov, context(agent_power(institutional), _, arbitrage, _)),
    constraint_indexing:constraint_classification(raac_school_maintenance, TypeLA, context(agent_power(institutional), _, constrained, _)),
    assertion(TypeGov == rope),
    assertion(TypeLA == snare),
    TypeGov \= TypeLA.

test(tangled_rope_conditions_met) :-
    % The analytical observer must see a tangled_rope.
    constraint_indexing:constraint_classification(raac_school_maintenance, tangled_rope, context(agent_power(analytical), _, _, _)),
    % And the structural conditions must be met.
    narrative_ontology:constraint_beneficiary(raac_school_maintenance, _),
    narrative_ontology:constraint_victim(raac_school_maintenance, _),
    domain_priors:requires_active_enforcement(raac_school_maintenance).


:- end_tests(raac_school_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): The cost of remediation (often millions per school)
 *     is a massive financial shock, representing a huge extraction of resources/value
 *     from the local level.
 *   - Suppression (0.75): Schools are legally mandated to provide education and ensure
 *     safety. They cannot simply "exit" the problem. Their main recourse is a
 *     centrally controlled funding process, which suppresses alternative solutions.
 *   - Tangled Rope classification: The situation is not pure extraction, as there is a
 *     genuine, complex coordination problem to solve (identifying and fixing hundreds
 *     of buildings). However, the solution is structured in a way that creates a
 *     massive, asymmetric flow of cost and risk, satisfying the definition of a
 *     Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a school headteacher (powerless, trapped), the situation is a
 *   Snare: an inescapable trap imposing overwhelming costs. For a Treasury official
 *   (institutional, arbitrage), the system of surveys, applications, and phased funding
 *   is a Rope: a coordination tool to manage a large, unforeseen liability without
 *   destabilizing the national budget. The analytical view resolves this by seeing
 *   both functions operating simultaneously, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: The UK Central Government benefits by deferring a massive capital
 *     expenditure liability. By controlling the pace and criteria of funding, it
 *     smooths the fiscal impact, a benefit measured in billions of pounds.
 *   - Victim: Schools and local authorities bear the direct costs: financial shortfall,
 *     administrative burden, educational disruption, and immediate safety risks.
 *     The directionality is a clear externalization of cost and risk from the center
 *     to the periphery.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model captures the critical difference between the central government and
 *   local authorities. Both are 'institutional', but their exit options differ
 *   radically. The central government has 'arbitrage' (control over national funds and
 *   policy), making the system a Rope for them. Local authorities have 'constrained'
 *   exit; they are legally responsible but financially dependent, making the same
 *   system a Snare from their viewpoint. This highlights how power asymmetries within
 *   a state bureaucracy define the experience of a constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two errors. It doesn't mislabel the situation
 *   as a pure Snare, which would ignore the real coordination challenge the government
 *   faces. It also avoids mislabeling it as a benign Rope, which would ignore the
 *   crushing extractive burden placed on schools. The Tangled Rope classification
 *   accurately captures the dual nature of the system: it is both a coordination
 *   mechanism AND an extractive one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_raac_school_maintenance,
    'Was the initial, widespread use of RAAC in the 1950s-90s a result of negligent disregard for its known lifespan, or a reasonable cost-saving measure given the engineering knowledge of the time?',
    'Archival research into building standards, government procurement policies, and engineering literature from the period.',
    'If negligent (a known risk was ignored for cost), the entire lifecycle is a Snare-in-waiting. If reasonable (degradation was faster than expected), it began as a Rope (coordination for cheap construction) that has since degraded into a Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(raac_school_maintenance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified over time as RAAC began to fail.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time: as awareness grew without full funding, the ratio of
% performative actions (surveys, meetings) to functional actions (repairs) increased.
narrative_ontology:measurement(raac_tr_t0, raac_school_maintenance, theater_ratio, 0, 0.10).
narrative_ontology:measurement(raac_tr_t5, raac_school_maintenance, theater_ratio, 5, 0.25).
narrative_ontology:measurement(raac_tr_t10, raac_school_maintenance, theater_ratio, 10, 0.40).

% Extraction over time: the extractive potential (latent cost of repair) grew
% as the material aged and the scale of the problem became clear.
narrative_ontology:measurement(raac_ex_t0, raac_school_maintenance, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(raac_ex_t5, raac_school_maintenance, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(raac_ex_t10, raac_school_maintenance, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: the problem is fundamentally about allocating scarce
% capital to address a widespread infrastructure failure.
narrative_ontology:coordination_type(raac_school_maintenance, resource_allocation).

% Network relationships: this constraint is structurally linked to broader
% policies of public sector austerity, which limit the available funds for
% remediation.
narrative_ontology:affects_constraint(uk_public_sector_austerity, raac_school_maintenance).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation based
% on beneficiary/victim declarations and the distinct exit_options (trapped,
% constrained, arbitrage) for the different agents accurately computes the
% directionality (d) for each perspective.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */