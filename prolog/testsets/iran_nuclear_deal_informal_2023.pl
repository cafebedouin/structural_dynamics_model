% ============================================================================
% CONSTRAINT STORY: iran_nuclear_deal_informal_2023
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_iran_nuclear_deal_informal_2023, []).

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
 *   constraint_id: iran_nuclear_deal_informal_2023
 *   human_readable: Informal US-Iran Nuclear De-escalation Agreement (2023)
 *   domain: geopolitical
 *
 * SUMMARY:
 *   An unwritten, informal agreement between the United States and Iran aimed
 *   at de-escalating tensions. The deal involves Iran capping its uranium
 *   enrichment below weapons-grade levels in exchange for the US easing
 *   enforcement of some economic sanctions. It functions as a coordination
 *   mechanism to avoid direct conflict while retaining significant coercive
 *   and extractive elements of the broader sanctions regime.
 *
 * KEY AGENTS (by structural relationship):
 *   - Iranian Populace & Economy: Primary target (powerless/trapped) — bears the cost of the sanctions regime that the deal only partially alleviates.
 *   - US Government & Allies: Primary beneficiary (institutional/arbitrage) — achieves non-proliferation goals with minimal cost and maintains coercive leverage.
 *   - Iranian Government: Inter-institutional actor (institutional/constrained) — experiences both the benefits of sanctions relief and the costs of limited sovereignty.
 *   - Analytical Observer: Analytical perspective — sees the hybrid structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iran_nuclear_deal_informal_2023, 0.55).
domain_priors:suppression_score(iran_nuclear_deal_informal_2023, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(iran_nuclear_deal_informal_2023, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iran_nuclear_deal_informal_2023, extractiveness, 0.55).
narrative_ontology:constraint_metric(iran_nuclear_deal_informal_2023, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(iran_nuclear_deal_informal_2023, theater_ratio, 0.40).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(iran_nuclear_deal_informal_2023, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(iran_nuclear_deal_informal_2023). % Required for Tangled Rope. Enforced by sanctions threat & IAEA.

% --- Emergence flag (required for mountain constraints) ---
% N/A for this human-constructed constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(iran_nuclear_deal_informal_2023, us_foreign_policy_establishment).
narrative_ontology:constraint_beneficiary(iran_nuclear_deal_informal_2023, regional_powers_seeking_stability).
narrative_ontology:constraint_beneficiary(iran_nuclear_deal_informal_2023, iranian_government). % Receives sanctions relief

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(iran_nuclear_deal_informal_2023, iranian_populace_and_economy).
narrative_ontology:constraint_victim(iran_nuclear_deal_informal_2023, iranian_government). % Sovereignty cost

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

% PERSPECTIVE 1: THE IRANIAN POPULACE (SNARE)
% Agent who bears the cost of the overarching sanctions regime. This informal
% deal is a minor modification to a larger Snare, not a fundamental change.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE US GOVERNMENT (ROPE)
% Agent who achieves primary policy goals (coordination on non-proliferation)
% at low cost while retaining maximum flexibility.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context sees both the coordination function and the
% asymmetric extraction inherent in the sanctions leverage.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE IRANIAN GOVERNMENT (TANGLED ROPE - INTER-INSTITUTIONAL)
% An institutional actor that is both beneficiary (sanctions relief) and
% victim (sovereignty cost). Its constrained exit options differentiate it from
% the US perspective. The engine derives a mid-to-high d, revealing the hybrid nature.
% Derivation: (institutional, constrained, beneficiary+victim) -> d ~ 0.55 -> f(d) ~ 0.75.
% χ = 0.55 * 0.75 * 1.0 (national scope) ≈ 0.41, which is in the Tangled Rope range.
constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iran_nuclear_deal_informal_2023_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between target (populace) and beneficiary (US).
    constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(perspectival_gap_inter_institutional) :-
    % Verify gap between the two institutional actors (US vs Iran).
    constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, tangled_rope, context(agent_power(institutional), _, exit_options(constrained), _)),
    true.

test(analytical_claim_matches) :-
    constraint_indexing:constraint_classification(iran_nuclear_deal_informal_2023, Type, context(agent_power(analytical), _, _, _)),
    narrative_ontology:constraint_claim(iran_nuclear_deal_informal_2023, Type).

test(tangled_rope_gate_validation) :-
    narrative_ontology:constraint_beneficiary(iran_nuclear_deal_informal_2023, _),
    narrative_ontology:constraint_victim(iran_nuclear_deal_informal_2023, _),
    domain_priors:requires_active_enforcement(iran_nuclear_deal_informal_2023).

:- end_tests(iran_nuclear_deal_informal_2023_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): Represents the significant sovereign cost imposed on Iran (limits on its nuclear program) and the economic damage from the sanctions regime that underpins the deal.
 *   - Suppression (0.65): High. The alternatives for both sides are highly undesirable: a nuclear-armed Iran and regional war for the US/allies, or full economic collapse and military threat for Iran. The deal suppresses these outcomes through coercion.
 *   - Theater (0.40): The "unwritten" and "informal" nature of the deal elevates its performative aspect. Both governments must signal resolve to domestic audiences while de-escalating, leading to a significant amount of theatrical activity relative to verifiable function.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The US, with arbitrage exit options, perceives a low-cost coordination mechanism to manage a threat (Rope). The Iranian populace, trapped by sanctions, experiences a slightly less-crushing version of an existing economic Snare. The analytical view sees both parts, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The US foreign policy establishment achieves non-proliferation goals. Regional powers gain stability. The Iranian government itself benefits from sanctions relief, making it a beneficiary.
 *   - Victims: The Iranian populace bears the brunt of the sanctions. The Iranian government is also a victim, as its sovereignty is constrained by external enforcement.
 *   This dual beneficiary/victim status for the Iranian government is key to its Tangled Rope classification.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story is a prime example of inter-institutional dynamics. Both the US and Iranian governments are 'institutional' actors, but their relationship to the constraint is asymmetric. The US has 'arbitrage' exit (it can reimpose sanctions easily), while Iran has 'constrained' exit (leaving the deal incurs immediate, severe economic pain). This difference in exit options is what drives the difference in their derived directionality (d) and thus their classifications (Rope vs. Tangled Rope), even at the same power level.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework correctly identifies the hybrid nature of the agreement, preventing a simplistic classification. A purely extraction-focused view would miss the genuine coordination function (conflict avoidance). A purely coordination-focused view would ignore the immense coercive pressure (sanctions) that makes the deal possible. The Tangled Rope classification captures this essential duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_iran_deal_2023,
    'Is this informal deal a temporary Scaffold towards a more comprehensive, stable treaty, or a permanent Tangled Rope that normalizes a state of managed hostility and extraction?',
    'Observation over the next 5-10 years: does it lead to formal diplomatic negotiations (JCPOA 2.0), or does it persist as an ad-hoc, unwritten understanding?',
    'If Scaffold, it represents successful temporary de-escalation. If Tangled Rope, it represents the institutionalization of coercion as a permanent geopolitical tool.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(iran_nuclear_deal_informal_2023, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint (ε=0.55 > 0.46).
% Models the shift from a "maximum pressure" campaign (higher extraction, low
% theater) to an informal deal (lower extraction, higher theater).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(ind23_tr_t0, iran_nuclear_deal_informal_2023, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ind23_tr_t5, iran_nuclear_deal_informal_2023, theater_ratio, 5, 0.40).
narrative_ontology:measurement(ind23_tr_t10, iran_nuclear_deal_informal_2023, theater_ratio, 10, 0.50).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(ind23_ex_t0, iran_nuclear_deal_informal_2023, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(ind23_ex_t5, iran_nuclear_deal_informal_2023, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ind23_ex_t10, iran_nuclear_deal_informal_2023, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(iran_nuclear_deal_informal_2023, enforcement_mechanism).

% Network relationships (structural influence edges)
% This informal deal exists in the shadow of the formal JCPOA treaty.
% It serves as a de-facto, less stable replacement.
narrative_ontology:affects_constraint(jcpoa_2015, iran_nuclear_deal_informal_2023).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The automatic derivation of 'd'
% from the combination of beneficiary/victim status and the distinct
% exit_options ('arbitrage' for the US, 'constrained' for Iran) is sufficient
% to capture the asymmetric inter-institutional dynamic and produce the
% correct perspectival gap.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */