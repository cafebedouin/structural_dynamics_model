% ============================================================================
% CONSTRAINT STORY: doomsday_clock_framework
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_doomsday_clock_framework, []).

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
 *   constraint_id: doomsday_clock_framework
 *   human_readable: Global Catastrophic Risk Management Framework
 *   domain: geopolitical
 *
 * SUMMARY:
 *   This constraint represents the entire institutional framework for managing
 *   global catastrophic risks, symbolized by the Bulletin of the Atomic
 *   Scientists' "Doomsday Clock." The framework includes nuclear non-proliferation
 *   treaties, international climate agreements, and norms against the use of
 *   weapons of mass destruction. While providing a vital coordination function
 *   to prevent apocalypse, it also enables massive resource extraction for
 *   military-industrial complexes and grants significant political power to
 *   the institutions that manage the risk.
 *
 * KEY AGENTS (by structural relationship):
 *   - Global Civilian Population: Primary target (powerless/trapped) — bears the psychological and financial costs of the risk and its management.
 *   - Nuclear Powers & Defense Contractors: Primary beneficiaries (institutional/arbitrage) — benefit from the strategic power and resource allocation justified by the threat.
 *   - Scientific Advisory Bodies (e.g., Bulletin): Secondary beneficiaries (institutional/arbitrage) — gain relevance, funding, and a platform.
 *   - Aspirant Nuclear States: Secondary targets (organized/constrained) — constrained by the non-proliferation aspects of the framework.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_framework, 0.55).
domain_priors:suppression_score(doomsday_clock_framework, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(doomsday_clock_framework, 0.60).       % Piton detection (< 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_framework, extractiveness, 0.55).
narrative_ontology:constraint_metric(doomsday_clock_framework, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(doomsday_clock_framework, theater_ratio, 0.60).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(doomsday_clock_framework, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(doomsday_clock_framework). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(doomsday_clock_framework, nuclear_powers_leadership).
narrative_ontology:constraint_beneficiary(doomsday_clock_framework, defense_contractors).
narrative_ontology:constraint_beneficiary(doomsday_clock_framework, scientific_advisory_bodies).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(doomsday_clock_framework, global_civilian_population).
narrative_ontology:constraint_victim(doomsday_clock_framework, aspirant_nuclear_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0, continental=1.1, global=1.2.
   ========================================================================== */

% PERSPECTIVE 1: THE GLOBAL CIVILIAN (PRIMARY TARGET)
% Bears the cost of defense spending and existential dread with no agency.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.55 * 1.42 * 1.2 (global scope) ≈ 0.94. This is a clear Snare (χ ≥ 0.66).
constraint_indexing:constraint_classification(doomsday_clock_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE SCIENTIFIC ADVISOR (PRIMARY BENEFICIARY)
% The Bulletin of the Atomic Scientists benefits from the framework's existence.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.55 * -0.12 * 1.2 ≈ -0.08. This is a clear Rope (χ ≤ 0.35).
constraint_indexing:constraint_classification(doomsday_clock_framework, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the vital coordination function and the massive extraction.
% Engine uses canonical d for analytical ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.55 * 1.15 * 1.2 ≈ 0.76. This is a Tangled Rope (0.40 ≤ χ ≤ 0.90).
constraint_indexing:constraint_classification(doomsday_clock_framework, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: A NUCLEAR POWER'S DEFENSE ESTABLISHMENT (INTER-INSTITUTIONAL)
% A beneficiary, but also constrained by treaties and international norms.
% Exit is 'constrained', not 'arbitrage', leading to a higher d than the
% scientific advisor, reflecting the operational constraints.
% Derivation: beneficiary + constrained exit -> d ≈ 0.30 -> f(d) ≈ 0.20
% χ = 0.55 * 0.20 * 1.2 ≈ 0.13. Still a Rope, but with higher perceived extraction.
constraint_indexing:constraint_classification(doomsday_clock_framework, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_framework_tests).

test(perspectival_gap_target_vs_beneficiary, [nondet]) :-
    constraint_indexing:constraint_classification(doomsday_clock_framework, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(doomsday_clock_framework, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_classification_is_tangled_rope, [nondet]) :-
    constraint_indexing:constraint_classification(doomsday_clock_framework, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(doomsday_clock_framework, _),
    narrative_ontology:constraint_victim(doomsday_clock_framework, _),
    domain_priors:requires_active_enforcement(doomsday_clock_framework).

:- end_tests(doomsday_clock_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.55): Represents the immense global expenditure on military hardware, the political capital derived from managing existential threats, and the compliance extracted from non-nuclear states.
 *   - Suppression Score (0.75): High because alternatives to the current framework (e.g., total disarmament, world government) are systemically suppressed and deemed politically infeasible, leaving this as the only viable system.
 *   - Theater Ratio (0.60): The clock announcement is a significant media event, but it's backed by genuine (if debated) scientific and policy analysis. The theater is substantial but doesn't completely overshadow the function, keeping it below the Piton threshold.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the powerless civilian, the system is a Snare: a source of existential dread and tax burden over which they have no control. For the institutional actors who manage the framework (like the Bulletin or defense departments), it is a Rope: a necessary coordination mechanism that provides them with purpose, funding, and influence. The analytical observer sees both sides, classifying it as a Tangled Rope—a system with a genuine coordination function that has been co-opted for massive asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'nuclear_powers_leadership' and 'defense_contractors' benefit directly from the resource allocation and strategic power the framework provides. 'scientific_advisory_bodies' benefit from the relevance and platform it grants them. These groups have low derived directionality (d).
 *   - Victims: The 'global_civilian_population' bears the psychological cost and the tax burden without meaningful agency. 'aspirant_nuclear_states' are actively constrained by the non-proliferation enforcement mechanisms. These groups have high derived directionality (d).
 *
 * MANDATROPHY ANALYSIS:
 *   A simplistic analysis might label the entire framework a Snare, focusing only on military spending. The Deferential Realism framework prevents this by forcing an acknowledgment of the coordination function. Without MAD and non-proliferation treaties, the risk of nuclear exchange would be far higher. The Tangled Rope classification correctly identifies that the system is *both* a critical coordination tool *and* a vehicle for immense extraction, capturing the dual nature that makes it so stable and difficult to reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_doomsday_clock,
    'Does the GCR framework genuinely reduce net existential risk, or does it merely create a fragile, high-tension equilibrium while enabling risk-generating arms races?',
    'Counterfactual historical analysis of de-escalation events vs. periods of brinkmanship; long-term analysis of whether the framework successfully integrates new threats (AI, bio) or fractures.',
    'If it genuinely reduces risk, its Rope-like properties are dominant. If it primarily enables arms races, its Snare-like properties are dominant, and the whole system is a net negative.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(doomsday_clock_framework, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This constraint's extraction has
% remained high, while its theatrical component has grown as threats diversify
% and public communication becomes a key function.
% T=0 (Cold War peak), T=5 (Post-Cold War), T=10 (Present).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(gcr_tr_t0, doomsday_clock_framework, theater_ratio, 0, 0.40).
narrative_ontology:measurement(gcr_tr_t5, doomsday_clock_framework, theater_ratio, 5, 0.50).
narrative_ontology:measurement(gcr_tr_t10, doomsday_clock_framework, theater_ratio, 10, 0.60).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(gcr_ex_t0, doomsday_clock_framework, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(gcr_ex_t5, doomsday_clock_framework, base_extractiveness, 5, 0.45). % Post-Cold War 'peace dividend' dip
narrative_ontology:measurement(gcr_ex_t10, doomsday_clock_framework, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This framework is a prime example of global infrastructure for risk management.
narrative_ontology:coordination_type(doomsday_clock_framework, global_infrastructure).

% Network relationships (structural influence edges)
% The GCR framework is upstream of specific treaties and policy domains.
narrative_ontology:affects_constraint(doomsday_clock_framework, nuclear_nonproliferation_treaty).
narrative_ontology:affects_constraint(doomsday_clock_framework, international_climate_agreements).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations combined with the agents' differing
% exit_options (trapped, arbitrage, constrained) accurately models the
% directionality for each key perspective.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */