% ============================================================================
% CONSTRAINT STORY: dwp_carers_allowance_cliff
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-23
% ============================================================================

:- module(constraint_dwp_carers_allowance_cliff, []).

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
    narrative_ontology:coordination_type/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dwp_carers_allowance_cliff
 *   human_readable: UK DWP Carer's Allowance Earnings Cliff
 *   domain: economic/political
 *
 * SUMMARY:
 *   The UK's Carer's Allowance system provides a small weekly payment to
 *   individuals providing at least 35 hours of unpaid care. The system
 *   imposes a strict earnings limit (£151/week as of 2024). If a carer
 *   earns even one penny over this limit, they lose 100% of the allowance.
 *   The Department for Work and Pensions (DWP) uses data-matching to
 *   retroactively identify overpayments, pursuing carers for thousands of
 *   pounds in debt, often years after the fact. This creates a severe
*    disincentive to work and a financial trap for a vulnerable population.
 *
 * KEY AGENTS (by structural relationship):
 *   - Unpaid Carers: Primary target (powerless/trapped) — bear the full extraction of the debt clawback.
 *   - The State (DWP): Primary beneficiary (institutional/arbitrage) — benefits from fiscal austerity and enforcement of rules designed to protect the public purse.
 *   - Analytical Observer: Sees the dual nature of the system as both a social support mechanism and an extractive trap.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dwp_carers_allowance_cliff, 0.85).
domain_priors:suppression_score(dwp_carers_allowance_cliff, 0.90).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(dwp_carers_allowance_cliff, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dwp_carers_allowance_cliff, extractiveness, 0.85).
narrative_ontology:constraint_metric(dwp_carers_allowance_cliff, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(dwp_carers_allowance_cliff, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(dwp_carers_allowance_cliff, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(dwp_carers_allowance_cliff). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
% The state/taxpayer benefits from the strict fiscal control.
narrative_ontology:constraint_beneficiary(dwp_carers_allowance_cliff, uk_taxpayers_via_dwp).

% Who bears disproportionate cost?
% The carers who are forced into debt for minor earnings infractions.
narrative_ontology:constraint_victim(dwp_carers_allowance_cliff, unpaid_carers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (THE UNPAID CARER)
% Bears the full cost. The system extracts 100% of their benefit for a tiny
% overage and pursues them for historic debt. This is a classic Snare.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42
% χ = 0.85 * 1.42 * 1.0 (national scope) = 1.207. With χ > 0.66, this is a clear Snare.
constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (THE DWP/STATE)
% Views the rule as a necessary coordination mechanism for fiscal control,
% ensuring benefits are correctly targeted. The extraction is seen as a feature, not a bug.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12
% χ = 0.85 * -0.12 * 1.0 = -0.102. Negative χ classifies as a Rope.
constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Recognizes both the stated coordination goal (supporting carers) and the
% highly extractive, punitive implementation. This duality, where a genuine
% coordination function is coupled with asymmetric extraction and active
% enforcement, defines a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.85 * 1.15 * 1.2 (global scope) = 1.173. This χ is very high, but because
% the structural requirements for Tangled Rope are met (beneficiary + victim +
% enforcement), it classifies as such.
constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dwp_carers_allowance_cliff_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, rope, context(agent_power(institutional), _, _, _)),
    true.

test(analytical_classification) :-
    % Verify the analytical observer sees a Tangled Rope.
    constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, tangled_rope, context(agent_power(analytical), _, _, _)),
    true.

test(tangled_rope_structural_gates) :-
    % Verify all three structural requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(dwp_carers_allowance_cliff, _),
    narrative_ontology:constraint_victim(dwp_carers_allowance_cliff, _),
    domain_priors:requires_active_enforcement(dwp_carers_allowance_cliff).

:- end_tests(dwp_carers_allowance_cliff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.85): The "cliff edge" is maximally extractive. Earning £0.01 over the threshold results in a 100% loss of the £81.90 allowance, plus the clawback of all past payments. This represents an extremely high rate of extraction relative to the infraction.
 *   - Suppression (0.90): Unpaid carers are highly constrained. The need to provide care is often non-negotiable, and alternative support systems are scarce or prohibitively expensive. This rule suppresses their ability to supplement their income, trapping them in poverty.
 *   - Theater (0.10): The enforcement is real and has severe financial consequences. This is not performative; it is an active, data-driven debt collection process.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the unpaid carer (powerless, trapped), the rule is a Snare that punishes them for trying to improve their financial situation. For the DWP (institutional, arbitrage), the same rule is a Rope—a necessary instrument of fiscal control to ensure benefits are distributed according to legislated criteria. The DWP sees itself as enforcing a fair rule; the carer experiences it as a punitive trap.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the structural roles. The `unpaid_carers` are declared as victims, as they bear the full cost of the clawbacks and are the targets of enforcement. The `uk_taxpayers_via_dwp` are the beneficiaries, as the strict enforcement protects the public purse from what the system defines as overpayments. The engine uses this data, combined with the `trapped` vs. `arbitrage` exit options, to compute the divergent directionality values (d) that drive the Rope/Snare classification gap.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   This case is a prime example of a Tangled Rope. A naive analysis might label it a pure Snare, focusing only on the victims' experience. However, this would miss the genuine (if poorly implemented) coordination function: the state *is* trying to allocate resources to support informal care. The Deferential Realism framework correctly identifies this duality, classifying it as a Tangled Rope from the analytical view. This prevents mislabeling a flawed coordination system as pure malice, while still capturing its brutally extractive nature for its targets.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_dwp_carers_allowance_cliff,
    'Is the earnings cliff a result of deliberate policy design for austerity, or a result of administrative inertia and poor legislative design?',
    'Review of internal DWP policy documents and legislative history from the rule''s creation.',
    'If deliberate, it confirms the Snare-like properties are a designed feature. If inertia, it suggests a Rope that has degraded into a Tangled Rope through neglect, making it a candidate for reform rather than abolition.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(dwp_carers_allowance_cliff, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.85 > 0.46), requiring temporal data.
% The model shows a system where the base extractive potential was always high,
% but its realization has intensified as DWP data-matching and enforcement
% capabilities improved over the last decade.

% Theater ratio over time: Rises slightly as public scrutiny requires more justification.
narrative_ontology:measurement(dwp_carers_allowance_cliff_tr_t0, dwp_carers_allowance_cliff, theater_ratio, 0, 0.05).
narrative_ontology:measurement(dwp_carers_allowance_cliff_tr_t5, dwp_carers_allowance_cliff, theater_ratio, 5, 0.08).
narrative_ontology:measurement(dwp_carers_allowance_cliff_tr_t10, dwp_carers_allowance_cliff, theater_ratio, 10, 0.10).

% Extraction over time: The fundamental rule is old, but its effective
% extraction increased with better enforcement technology.
narrative_ontology:measurement(dwp_carers_allowance_cliff_ex_t0, dwp_carers_allowance_cliff, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(dwp_carers_allowance_cliff_ex_t5, dwp_carers_allowance_cliff, base_extractiveness, 5, 0.80).
narrative_ontology:measurement(dwp_carers_allowance_cliff_ex_t10, dwp_carers_allowance_cliff, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The allowance is a mechanism for distributing state funds.
narrative_ontology:coordination_type(dwp_carers_allowance_cliff, resource_allocation).

% Network relationships: This policy is structurally linked to the broader
% state apparatus for debt collection.
narrative_ontology:affects_constraint(dwp_carers_allowance_cliff, state_debt_collection_practices).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The standard derivation from
% beneficiary/victim declarations and exit options correctly models the
% structural relationships and generates the expected perspectival gap.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */