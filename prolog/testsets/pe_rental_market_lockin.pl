% ============================================================================
% CONSTRAINT STORY: pe_rental_market_lockin
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_pe_rental_market_lockin, []).

:- use_module(library(plunit)).
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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pe_rental_market_lockin
 *   human_readable: Private Equity Lock-in of Single-Family Rental Market
 *   domain: economic
 *
 * SUMMARY:
 *   Following the 2008 financial crisis, large private equity firms began
 *   buying vast portfolios of single-family homes, converting them into
 *   rentals. This constraint is the resulting market structure, characterized
 *   by algorithmic rent-setting, ancillary fee extraction, and the reduction
 *   of housing stock available for private ownership. It functions as a
 *   financial instrument for investors while creating a highly extractive
 *   and constrained environment for tenants.
 *   Source: https://assets.bwbx.io/documents/users/iqjWHBFdfxIU/rRmql_jJcxb4/v0
 *
 * KEY AGENTS (by structural relationship):
 *   - Tenants in PE-owned homes: Primary target (powerless/trapped) — bears extraction via rent hikes and fees.
 *   - Prospective homebuyers: Secondary target (moderate/constrained) — priced out of markets where PE firms buy up supply.
 *   - Private Equity firms & their investors: Primary beneficiary (institutional/arbitrage) — benefits from stable, high-yield revenue streams.
 *   - Government Regulators: Inter-institutional actor (institutional/constrained) — possesses regulatory power but is often unable or unwilling to curb the practice.
 *   - Analytical Observer: Sees the full structure of coordination (housing provision) and asymmetric extraction (financialization).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pe_rental_market_lockin, 0.65).
domain_priors:suppression_score(pe_rental_market_lockin, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(pe_rental_market_lockin, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pe_rental_market_lockin, extractiveness, 0.65).
narrative_ontology:constraint_metric(pe_rental_market_lockin, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(pe_rental_market_lockin, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(pe_rental_market_lockin, tangled_rope).
narrative_ontology:human_readable(pe_rental_market_lockin, "Private Equity Lock-in of Single-Family Rental Market").

% --- Binary flags ---
domain_priors:requires_active_enforcement(pe_rental_market_lockin). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(pe_rental_market_lockin, private_equity_investors).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(pe_rental_market_lockin, tenants_in_pe_owned_homes).
narrative_ontology:constraint_victim(pe_rental_market_lockin, prospective_homebuyers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (TENANT)
% Bears the full cost of extraction via fees and rent hikes, with high
% switching costs and few alternatives in dominated markets.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% High ε, high suppression, and high χ result in a Snare classification.
constraint_indexing:constraint_classification(pe_rental_market_lockin, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (PE FIRM)
% Views the system as an efficient mechanism for deploying capital and generating
% returns, a pure coordination tool for financial goals.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
% Negative effective extraction means it is perceived as a subsidy, a pure Rope.
constraint_indexing:constraint_classification(pe_rental_market_lockin, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Recognizes both the coordination function (providing rental housing) and the
% severe, asymmetric extraction. The high ε and suppression, combined with
% the dual beneficiary/victim structure, define a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(pe_rental_market_lockin, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The government regulator is an institutional actor, but unlike the PE firm,
% its ability to exit or change the system is limited by political and economic
% pressures.

% Perspective 4: The Government Regulator
% Recognizes the problems but has limited tools or political will to act,
% making its exit 'constrained'. It sees the extractive elements more clearly
% than the beneficiary does, but still within an institutional frame.
% The derived 'd' will be higher than the PE firm's but lower than the tenant's.
constraint_indexing:constraint_classification(pe_rental_market_lockin, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pe_rental_market_lockin_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(pe_rental_market_lockin, snare,
        context(agent_power(powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(regional))),
    constraint_indexing:constraint_classification(pe_rental_market_lockin, rope,
        context(agent_power(institutional), time_horizon(generational), exit_options(arbitrage), spatial_scope(national))),
    format('OK: Perspectival gap [Snare vs Rope] confirmed.~n').

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(pe_rental_market_lockin, tangled_rope,
        context(agent_power(analytical), _, _, _)),
    narrative_ontology:constraint_claim(pe_rental_market_lockin, tangled_rope),
    format('OK: Analytical classification matches constraint claim.~n').

test(tangled_rope_structural_gates_pass) :-
    narrative_ontology:constraint_beneficiary(pe_rental_market_lockin, _),
    narrative_ontology:constraint_victim(pe_rental_market_lockin, _),
    domain_priors:requires_active_enforcement(pe_rental_market_lockin),
    format('OK: All three structural requirements for Tangled Rope are met.~n').

:- end_tests(pe_rental_market_lockin_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.65): This is set high to reflect the business model's core reliance on maximizing revenue through aggressive rent hikes, ancillary fees ("pet rent," "smart-home fees"), and minimizing maintenance costs, as detailed in the source article. It's fundamentally extractive.
 *   - Suppression Score (0.75): The strategy's effectiveness hinges on buying up a significant portion of available single-family housing stock in target neighborhoods. This directly suppresses the alternative of homeownership for potential buyers and reduces competitive rental options for tenants, locking them in.
 *   - Theater Ratio (0.20): While firms use rhetoric of "professionalizing" the rental market, the operational reality is geared toward financial extraction, not service provision. The functional aspect (extraction) heavily outweighs the performative aspect (customer service).
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For tenants (powerless/trapped), the constraint is a Snare: an inescapable trap that extracts resources and offers little recourse. For PE investors (institutional/arbitrage), it is a Rope: a brilliant coordination mechanism that transforms idiosyncratic housing assets into a predictable, scalable, and highly profitable financial product. They experience negative effective extraction because the system is designed for their benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of value flow is unambiguously from tenants to investors. Declaring 'tenants_in_pe_owned_homes' as the victim and 'private_equity_investors' as the beneficiary models this structure. The engine uses this to derive a high directionality `d` for tenants (high χ, Snare) and a very low `d` for investors (negative χ, Rope). This correctly maps the physics of the financial relationship.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model includes a 'constrained' institutional perspective for regulators. Unlike the PE firms with 'arbitrage' exit, regulators are stuck within the system. Their inability to easily change the rules makes them experience the constraint as a Tangled Rope—they see both the coordination function (a large entity managing rentals) and the negative externalities of the extraction, which they are tasked with mitigating but often cannot. This captures the dynamic of regulatory friction or capture.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope from the analytical view is crucial. It avoids two errors. It is not a pure Snare, because it does perform a coordination function (providing rental housing at scale). It is not a pure Rope, because that would ignore the immense, asymmetric extraction imposed on tenants. The Tangled Rope classification correctly identifies it as a hybrid system where a genuine coordination function has been weaponized for financial extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_pe_rental_market_lockin,
    'Is this financialized rental model a stable, long-term feature of the housing market, or a temporary arbitrage that will collapse under regulatory pressure or a significant economic downturn?',
    'Observation of the market over a full economic cycle (including a recession and/or significant interest rate changes) and tracking the legislative response at federal and state levels.',
    'If stable, it represents a permanent shift from housing-as-shelter to housing-as-financial-asset, solidifying the Tangled Rope classification. If it collapses, it was a temporary Scaffold for capital that will be dismantled, leaving behind a distorted market.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(pe_rental_market_lockin, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This model emerged after 2008 and has been refined to be more extractive.
% The timeline reflects an initial phase of acquisition (lower extraction)
% followed by a mature phase of optimized revenue extraction (higher ε).
% Base extractiveness is > 0.46, so this section is required.

% Theater ratio over time (initially higher to appear as market professionalization):
narrative_ontology:measurement(pe_rental_tr_t0, pe_rental_market_lockin, theater_ratio, 0, 0.40).
narrative_ontology:measurement(pe_rental_tr_t5, pe_rental_market_lockin, theater_ratio, 5, 0.25).
narrative_ontology:measurement(pe_rental_tr_t10, pe_rental_market_lockin, theater_ratio, 10, 0.20).

% Extraction over time (model becomes more efficient at extracting fees/rent):
narrative_ontology:measurement(pe_rental_ex_t0, pe_rental_market_lockin, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(pe_rental_ex_t5, pe_rental_market_lockin, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(pe_rental_ex_t10, pe_rental_market_lockin, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint allocates housing stock (a resource) to renters.
narrative_ontology:coordination_type(pe_rental_market_lockin, resource_allocation).

% Network relationships (structural influence edges)
% This constraint directly exacerbates the broader housing crisis.
narrative_ontology:affects_constraint(pe_rental_market_lockin, housing_affordability_crisis).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% chain, using the declared beneficiary/victim groups and the distinct
% exit_options (trapped, arbitrage, constrained), accurately models the
% directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */