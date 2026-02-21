% ============================================================================
% CONSTRAINT STORY: commercial_data_brokerage
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_commercial_data_brokerage, []).

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
    narrative_ontology:omega_variable/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: commercial_data_brokerage
 *   human_readable: The Commercial Data Brokerage Ecosystem
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The modern data brokerage industry systematically collects, aggregates,
 *   and sells vast quantities of personal information about individuals, often
 *   without their meaningful knowledge or consent. This information is sourced
 *   from apps, web tracking, public records, and purchase histories, creating
 *   detailed profiles that are sold to other companies for marketing, risk
 *   assessment, and other purposes. The constraint is the existence of this
 *   extractive market, which operates largely outside the control of the
 *   individuals whose data constitutes the product.
 *
 * KEY AGENTS (by structural relationship):
 *   - digital_service_users: Primary target (powerless/trapped) — their data is the raw
 *     material extracted, and meaningful opt-out is structurally suppressed.
 *   - data_brokers_and_clients: Primary beneficiary (institutional/arbitrage) —
 *     they profit directly from the sale and use of the aggregated data.
 *   - app_developers_and_platforms: Inter-institutional beneficiary
 *     (institutional/constrained) — they benefit by selling user data to
 *     brokers but are also dependent on this ecosystem for revenue.
 *   - analytical_observer: Analytical observer — sees the full extractive
 *     structure and its coordination functions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commercial_data_brokerage, 0.75).
domain_priors:suppression_score(commercial_data_brokerage, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(commercial_data_brokerage, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commercial_data_brokerage, extractiveness, 0.75).
narrative_ontology:constraint_metric(commercial_data_brokerage, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(commercial_data_brokerage, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(commercial_data_brokerage, snare).

% --- Binary flags ---
domain_priors:requires_active_enforcement(commercial_data_brokerage). % Tech (SDKs) and legal (ToS)

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(commercial_data_brokerage, data_brokers_and_clients).
narrative_ontology:constraint_beneficiary(commercial_data_brokerage, app_developers_and_platforms).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(commercial_data_brokerage, digital_service_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (DIGITAL SERVICE USERS)
% Agent whose data is extracted. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%
% NOTE: Per "Dynamic Coalition" extension, this agent's power may be
% upgraded to 'organized' if privacy movements or class-action lawsuits
% reach a critical mass, potentially changing the classification.
constraint_indexing:constraint_classification(commercial_data_brokerage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (DATA BROKERS)
% Agent who profits directly. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(commercial_data_brokerage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% The engine's derived d ≈ 0.72 → f(d) ≈ 1.15 produces a high χ,
% correctly identifying the overall structure as a Snare.
constraint_indexing:constraint_classification(commercial_data_brokerage, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% Platforms are beneficiaries but are more constrained than pure brokers,
% as their business models depend heavily on this ecosystem.
% The engine differentiates via directionality: the 'constrained' exit
% produces a higher d value than the 'arbitrage' exit of the pure brokers.
constraint_indexing:constraint_classification(commercial_data_brokerage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commercial_data_brokerage_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify the core perspectival gap: Snare for the user, Rope for the broker.
    constraint_indexing:constraint_classification(commercial_data_brokerage, snare,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(commercial_data_brokerage, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(inter_institutional_view) :-
    % Verify that the constrained institutional actor (platforms) still
    % perceives the constraint as a Rope, distinct from the powerless target.
    constraint_indexing:constraint_classification(commercial_data_brokerage, rope,
        context(agent_power(institutional), _, exit_options(constrained), _)).

test(analytical_view_matches_claim) :-
    % The analytical perspective should match the declared claim.
    narrative_ontology:constraint_claim(commercial_data_brokerage, Claim),
    constraint_indexing:constraint_classification(commercial_data_brokerage, Claim,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction) :-
    narrative_ontology:constraint_metric(commercial_data_brokerage, extractiveness, E),
    narrative_ontology:constraint_metric(commercial_data_brokerage, suppression_requirement, S),
    E >= 0.46,  % Snare-level extraction
    S >= 0.60.  % Snare-level suppression

:- end_tests(commercial_data_brokerage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): The entire multi-billion dollar data brokerage
 *     industry is built on extracting the value of personal data from individuals,
 *     who receive no direct compensation. This represents a very high degree of
 *     value extraction.
 *   - Suppression Score (S=0.85): Alternatives are structurally suppressed.
 *     Participating in the modern digital economy almost requires submission to
 *     this data collection. Opt-out mechanisms are intentionally complex,
 *     incomplete, or ineffective, and there is significant industry lobbying
 *     against meaningful privacy regulation.
 *
 * PERSPECTIVAL GAP:
 *   - From the perspective of a `digital_service_user` (powerless, trapped), the system is a
 *     Snare. Their data is taken, they have no control, and no viable alternative
 *     exists. The effective extraction (χ) is extremely high.
 *   - From the perspective of a `data_broker` (institutional, arbitrage), the
 *     system is a Rope. It's a highly efficient coordination mechanism for matching
 *     data supply with demand, creating a valuable market. For them, effective
 *     extraction is negative, as the system subsidizes their business model.
 *
 * DIRECTIONALITY LOGIC:
 *   - The direction of extraction is unambiguous. Value flows from `digital_service_users`
 *     (the declared victims) to the `data_brokers_and_clients` and `app_developers`
 *     (the declared beneficiaries). This structural relationship is the primary
 *     input to the directionality `d` derivation, which correctly models the
 *     asymmetric nature of the constraint.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model distinguishes between pure-play data brokers (arbitrage exit) and
 *   platforms/app developers (constrained exit). While both are institutional
 *   beneficiaries, the platforms are more deeply embedded in the ecosystem. Their
 *   business models depend on it, giving them less freedom to exit or change
 *   terms. The directionality engine reflects this with a slightly higher `d`
 *   for the constrained actor, but the classification remains Rope for both, as
 *   they are on the benefiting side of the extractive divide.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] This classification avoids the common mandatrophy error
 *   of labeling any market-like mechanism a "Rope." By indexing to the powerless
 *   agent, the framework correctly identifies the coercive, extractive nature of
 *   the system. The fact that it provides a coordination benefit to the
 *   institutional players does not negate the Snare experienced by its targets;
 *   it explains why the Snare is so robustly defended by its beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_commercial_data_brokerage,
    'Is the non-consensual nature of data collection a fundamental, structural feature of the industry''s business model, or a correctable market failure?',
    'Passage of comprehensive privacy legislation (e.g., GDPR with fewer loopholes and stronger enforcement) and observation of industry adaptation or collapse.',
    'If fundamental, the system remains a Snare and attempts at reform will be resisted or routed around. If correctable, it could transition to a Tangled Rope where individuals gain some agency and compensation.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(
    omega_commercial_data_brokerage,
    empirical,
    'Whether the industry can adapt to a consent-based model or if its structure is fundamentally extractive.'
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(commercial_data_brokerage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.75 > 0.46), so temporal
% data is required to model its evolution from the early internet to the
% current era of ubiquitous tracking.
%
% T=0: Early internet (ca. 2000)
% T=5: Post-social media & smartphone boom (ca. 2012)
% T=10: Current mature mobile/IoT ecosystem (ca. 2024)

% Theater ratio over time (privacy policies became more prominent but not more effective):
narrative_ontology:measurement(cdb_tr_t0, commercial_data_brokerage, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cdb_tr_t5, commercial_data_brokerage, theater_ratio, 5, 0.15).
narrative_ontology:measurement(cdb_tr_t10, commercial_data_brokerage, theater_ratio, 10, 0.20).

% Extraction over time (sophistication and scope of data collection has increased dramatically):
narrative_ontology:measurement(cdb_ex_t0, commercial_data_brokerage, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(cdb_ex_t5, commercial_data_brokerage, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(cdb_ex_t10, commercial_data_brokerage, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system coordinates the allocation of resources (attention, credit,
% insurance) based on the brokered information profiles.
narrative_ontology:coordination_type(commercial_data_brokerage, resource_allocation).

% Network relationships (structural influence edges)
% The outputs of data brokerage are inputs to other algorithmic systems.
narrative_ontology:affects_constraint(commercial_data_brokerage, algorithmic_credit_scoring).
narrative_ontology:affects_constraint(commercial_data_brokerage, political_microtargeting).


/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */