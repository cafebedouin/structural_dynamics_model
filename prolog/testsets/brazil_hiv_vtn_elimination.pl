% ============================================================================
% CONSTRAINT STORY: brazil_hiv_vtn_elimination
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_brazil_hiv_vtn_elimination, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: brazil_hiv_vtn_elimination
 *   human_readable: Brazil's Program for Eliminating Vertical HIV Transmission
 *   domain: social / public_health
 *
 * SUMMARY:
 *   This constraint represents the coordinated public health program in Brazil
 *   aimed at eliminating the mother-to-child (vertical) transmission of HIV.
 *   The program structures the actions of patients, healthcare providers, and
 *   public institutions through standardized protocols for testing, antiretroviral
 *   treatment, and infant follow-up, all integrated within the national Unified
 *   Health System (SUS). It is a classic coordination mechanism to achieve a
 *   shared public good.
 *
 * KEY AGENTS (by structural relationship):
 *   - hiv_positive_mothers_and_infants: Primary beneficiary (powerless/constrained) — Receives the life-saving intervention.
 *   - brazilian_public_health_system: Institutional beneficiary (institutional/arbitrage) — Fulfills its public health mandate and improves national health indicators.
 *   - frontline_healthcare_workers: Operational beneficiary (moderate/mobile) — Provided with a clear, effective protocol to deliver care.
 *   - analytical_observer: Analytical observer — Sees the full structure as a low-extraction coordination system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brazil_hiv_vtn_elimination, 0.10).
domain_priors:suppression_score(brazil_hiv_vtn_elimination, 0.20).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(brazil_hiv_vtn_elimination, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brazil_hiv_vtn_elimination, extractiveness, 0.10).
narrative_ontology:constraint_metric(brazil_hiv_vtn_elimination, suppression_requirement, 0.20).
narrative_ontology:constraint_metric(brazil_hiv_vtn_elimination, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(brazil_hiv_vtn_elimination, rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(brazil_hiv_vtn_elimination). % The program requires ongoing funding, staffing, and monitoring.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(brazil_hiv_vtn_elimination, hiv_positive_mothers_and_infants).
narrative_ontology:constraint_beneficiary(brazil_hiv_vtn_elimination, brazilian_public_health_system).
narrative_ontology:constraint_beneficiary(brazil_hiv_vtn_elimination, frontline_healthcare_workers).
%
% Who bears disproportionate cost?
% No structurally defined victim group exists. The costs are socialized public
% health expenditures, not asymmetric extraction from a specific population.
% This absence is a key indicator of a Rope, not a Tangled Rope or Snare.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% This is a uniform-type constraint (Rope-only). The classification is the same
% from all relevant perspectives because the base extractiveness is low and
% there is no structurally defined victim group. The 'perspectival gap' is absent.

% PERSPECTIVE 1: THE PRIMARY BENEFICIARY (HIV-positive Mother)
% Agent who directly receives the intervention. As a declared beneficiary,
% the engine derives a low `d`, resulting in a low/negative effective
% extraction (χ), correctly classifying the program as a Rope.
constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE INSTITUTIONAL BENEFICIARY (Public Health System)
% The architect and administrator of the program. As a beneficiary with
% arbitrage exit, `d` is very low, and χ is negative.
constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context, which serves as the basis for the system's
% computed `constraint_claim`. It correctly identifies the low-extraction,
% high-coordination nature of the program.
constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: A FRONTLINE HEALTHCARE WORKER
% A participant who implements the protocol. They are a beneficiary of a
% clear, effective system. This further demonstrates the uniform Rope
% classification across different roles.
constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brazil_hiv_vtn_elimination_tests).

test(perspectival_agreement, [nondet]) :-
    % For a uniform-type constraint like this Rope, we verify that key
    % perspectives AGREE on the classification, unlike in a Snare/Tangled Rope.
    constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == rope.

test(threshold_validation) :-
    % Verify the base extractiveness is within the low-extraction range for a Rope.
    narrative_ontology:constraint_metric(brazil_hiv_vtn_elimination, extractiveness, E),
    E =< 0.45.

test(structural_data_check) :-
    % Verify that beneficiary is declared and victim is absent, consistent with a Rope.
    narrative_ontology:constraint_beneficiary(brazil_hiv_vtn_elimination, _),
    \+ narrative_ontology:constraint_victim(brazil_hiv_vtn_elimination, _).


:- end_tests(brazil_hiv_vtn_elimination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε = 0.10): The program's purpose is the provision of a
 *     public good, not rent-seeking or value extraction. The low ε reflects
 *     administrative overhead and resource costs, not asymmetric extraction.
 *   - Suppression Score (0.20): The program does not suppress viable
 *     alternatives; it represents the globally recognized standard of care.
 *     The alternative is non-treatment, which is strictly inferior.
 *   - Theater Ratio (0.15): The program is highly functional, with success
 *     measured by a clear, objective metric: the rate of vertical HIV
 *     transmission, which has been driven to near-zero.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is notable for its *lack* of a perspectival gap. It is a
 *   uniform-type constraint, classifying as a Rope from all relevant
 *   perspectives. Both the direct recipients of care (mothers) and the
 *   institutional administrators (health system) perceive it as a beneficial
 *   coordination mechanism. This agreement is a strong signal of a healthy,
 *   pure coordination system.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is driven by the clear beneficiary groups (`hiv_positive_mothers_and_infants`,
 *   `brazilian_public_health_system`) and the crucial absence of a structurally
 *   defined victim group. The costs of the program are socialized as a public
 *   health expenditure, a symmetric cost for a collective benefit, not an
 *   asymmetric cost imposed on a powerless group for the benefit of a powerful one.
 *   This structural data ensures the engine derives low `d` values for all
 *   participants, leading to the correct Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This model correctly identifies a complex, resource-intensive public health
 *   program as a Rope (pure coordination). An analysis that ignored
 *   directionality might mistakenly focus on the "coercive" aspects (e.g.,
 *   adherence to a strict medical protocol) and misclassify it as a Snare.
 *   By explicitly defining who benefits, the Deferential Realism framework
 *   distinguishes between coercive extraction (Snare) and structured
 *   coordination for a shared goal (Rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_brazil_hiv_vtn_elimination,
    'Will long-term political or economic pressures degrade the program from a functional Rope into a theatrical Piton, where compliance is tracked but outcomes are no longer achieved?',
    'Longitudinal tracking of vertical transmission rates correlated with program funding, staffing levels, and antiretroviral drug availability.',
    'If it degrades, it becomes a Piton, offering the illusion of care without preventing transmission. If it remains functional, it stands as a durable example of a public health Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(brazil_hiv_vtn_elimination, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a low-extraction constraint, so temporal data is not strictly required.
% However, we model its successful implementation over time, showing a healthy
% lifecycle of increasing efficiency (decreasing overhead/extraction and theater).

% Theater ratio over time (shows increasing effectiveness):
narrative_ontology:measurement(br_hiv_tr_t0, brazil_hiv_vtn_elimination, theater_ratio, 0, 0.25).
narrative_ontology:measurement(br_hiv_tr_t5, brazil_hiv_vtn_elimination, theater_ratio, 5, 0.20).
narrative_ontology:measurement(br_hiv_tr_t10, brazil_hiv_vtn_elimination, theater_ratio, 10, 0.15).

% Extraction over time (shows streamlining and reduced waste):
narrative_ontology:measurement(br_hiv_ex_t0, brazil_hiv_vtn_elimination, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(br_hiv_ex_t5, brazil_hiv_vtn_elimination, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(br_hiv_ex_t10, brazil_hiv_vtn_elimination, base_extractiveness, 10, 0.10).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This program is a prime example of resource allocation for public health.
narrative_ontology:coordination_type(brazil_hiv_vtn_elimination, resource_allocation).

% Network relationships (structural influence edges)
% The program is structurally dependent on the existence and funding of
% Brazil's universal healthcare system (SUS).
narrative_ontology:affects_constraint(brazil_sus_universal_healthcare, brazil_hiv_vtn_elimination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% based on the declared beneficiary groups accurately captures the
% non-extractive, cooperative nature of the system for all participants.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */