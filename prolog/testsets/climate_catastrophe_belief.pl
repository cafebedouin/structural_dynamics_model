% ============================================================================
% CONSTRAINT STORY: climate_catastrophe_belief
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_climate_catastrophe_belief, []).

:- use_module(library(plunit)).
:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).
:- use_module(config).

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
 *   constraint_id: climate_catastrophe_belief
 *   human_readable: "Belief in Inevitable Near-Term Climate Catastrophe"
 *   domain: social/political
 *
 * SUMMARY:
 *   Based on polling data (e.g., YouGov/Economist, Feb 2026), a significant
 *   portion of the population (nearly half of Americans) believes they will
 *   witness catastrophic impacts from climate change within their lifetimes.
 *   This belief functions as a powerful social constraint, shaping individual
 *   choices, generating psychological costs (anxiety, fatalism), and
 *   paradoxically coordinating political inaction by making solutions seem
 *   futile.
 *
 * KEY AGENTS (by structural relationship):
 *   - General Public (experiencing climate anxiety): Primary target (powerless/trapped) — bears the psychological and planning costs of perceived inevitability.
 *   - Status Quo Incumbents (e.g., fossil fuel industry): Primary beneficiary (institutional/arbitrage) — benefits from delayed action and public demobilization, which are byproducts of fatalism.
 *   - Government Policy-Makers: Secondary actor (institutional/constrained) — trapped between public anxiety, economic pressures from beneficiaries, and the physical reality of climate change.
 *   - Climate Scientists / Systems Analysts: Analytical observer — sees the full structure, including the valid scientific basis, the political economy of inaction, and the resulting social-psychological effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_catastrophe_belief, 0.52).
domain_priors:suppression_score(climate_catastrophe_belief, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(climate_catastrophe_belief, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_catastrophe_belief, extractiveness, 0.52).
narrative_ontology:constraint_metric(climate_catastrophe_belief, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_catastrophe_belief, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(climate_catastrophe_belief, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(climate_catastrophe_belief). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(climate_catastrophe_belief, status_quo_incumbents).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(climate_catastrophe_belief, general_public_experiencing_anxiety).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (THE "DOOMED" CITIZEN)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% This high effective extraction (χ ≈ 0.52 * 1.42 * 1.2 ≈ 0.88) and high
% suppression (0.65) lead to a Snare classification.
constraint_indexing:constraint_classification(climate_catastrophe_belief, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (STATUS QUO INCUMBENT)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% The constraint functions as a tool to coordinate market and political
% inaction, protecting existing business models. From this view, it is a Rope.
constraint_indexing:constraint_classification(climate_catastrophe_belief, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. Sees both the coordination function (of inaction)
% and the asymmetric extraction (psychological cost). Engine derives d ≈ 0.72.
% χ ≈ 0.52 * 1.15 * 1.2 ≈ 0.72. This, plus the declared beneficiary, victim, and
% enforcement, satisfies the criteria for a Tangled Rope.
constraint_indexing:constraint_classification(climate_catastrophe_belief, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: GOVERNMENT POLICY-MAKER
% An institutional actor, but one constrained by political and economic forces.
% This agent is a victim of the problem but also must manage the system that
% benefits incumbents. Their 'constrained' exit option yields a higher d than
% the beneficiary's 'arbitrage' exit. χ ≈ 0.52 * f(d) * 1.0. The derived d
% for institutional+victim+constrained puts χ in the Tangled Rope range.
constraint_indexing:constraint_classification(climate_catastrophe_belief, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_catastrophe_belief_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    constraint_indexing:constraint_classification(climate_catastrophe_belief, snare,
        context(agent_power(powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(global))),
    constraint_indexing:constraint_classification(climate_catastrophe_belief, rope,
        context(agent_power(institutional), time_horizon(generational), exit_options(arbitrage), spatial_scope(global))),
    assertion(snare \= rope).

test(tangled_rope_analytical_view) :-
    constraint_indexing:constraint_classification(climate_catastrophe_belief, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    narrative_ontology:constraint_beneficiary(climate_catastrophe_belief, _),
    narrative_ontology:constraint_victim(climate_catastrophe_belief, _),
    domain_priors:requires_active_enforcement(climate_catastrophe_belief).

test(high_extraction_and_suppression_scores) :-
    domain_priors:base_extractiveness(climate_catastrophe_belief, E),
    domain_priors:suppression_score(climate_catastrophe_belief, S),
    assertion(E >= 0.46),
    assertion(S >= 0.60).

:- end_tests(climate_catastrophe_belief_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.52): Represents the significant psychological cost (anxiety, depression, fatalism), planning costs (e.g., deciding not to have children), and misallocation of societal attention caused by the belief in inevitability.
 *   - Suppression (S=0.65): High because the narrative of "it's too late" actively suppresses or marginalizes alternative, solution-oriented narratives, making them appear naive or unserious.
 *   - The combination of a coordination function (demobilizing the public, which benefits incumbents) and high asymmetric extraction (costs borne by the public) makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a powerless citizen, the belief is an inescapable future reality they are trapped in, making it a Snare. For an institutional beneficiary, the same belief is a useful tool for managing public opinion and delaying costly regulation, making it a Rope. The analytical view recognizes both facets, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'status_quo_incumbents' directly profit from delayed climate action. The belief in inevitability serves their interest by reducing political pressure for change. Their arbitrage exit allows them to hedge financial risk and influence policy.
 *   - Victims: 'general_public_experiencing_anxiety' bear the non-financialized but very real costs of the belief. They are trapped geographically and existentially, with no meaningful exit. This structural relationship drives the high directionality (d) and effective extraction (χ) for this group.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The government policy-maker perspective highlights the complexity. Unlike the primary beneficiary, the state cannot fully arbitrage the risk; it is ultimately responsible for managing the consequences. Its 'constrained' exit reflects its limited ability to act due to political polarization, economic dependencies, and international pressures, placing it in a Tangled Rope situation where it must navigate both the coordination and extraction elements of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors. It does not mislabel the belief as a Mountain (it is a social, not physical, inevitability). It also avoids calling it a pure Snare, which would ignore the powerful, (perverse) coordination function it serves for beneficiaries. The Tangled Rope classification captures the essential duality of the constraint. The large number of victims also suggests the potential for a "Dynamic Coalition"; if this group becomes organized, its power level would increase, potentially shifting its classification and ability to resist the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_climate_catastrophe_belief,
    'Is the "inevitability" narrative a grassroots belief emerging from scientific reporting, or is it primarily an engineered/astroturfed narrative amplified by beneficiaries to demobilize opposition?',
    'Detailed historical analysis of fossil fuel industry PR campaigns, media content analysis of climate reporting over time, and social network analysis of narrative propagation.',
    'If primarily grassroots, it suggests a failure of public sensemaking (a Rope/Piton). If primarily engineered, it is a clear instrument of extraction (a Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_climate_catastrophe_belief, empirical, 'Degree to which climate fatalism is an engineered vs. emergent narrative.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_catastrophe_belief, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data modeling the rise of climate fatalism over an approximate 20-year period (2006-2026).
% Required because base_extractiveness (0.52) > 0.46.

% Theater ratio over time (declines as fatalism becomes a more functional belief):
narrative_ontology:measurement(ccb_tr_t0, climate_catastrophe_belief, theater_ratio, 0, 0.40).
narrative_ontology:measurement(ccb_tr_t5, climate_catastrophe_belief, theater_ratio, 5, 0.30).
narrative_ontology:measurement(ccb_tr_t10, climate_catastrophe_belief, theater_ratio, 10, 0.20).

% Extraction over time (increases as the belief becomes more widespread and impactful):
narrative_ontology:measurement(ccb_ex_t0, climate_catastrophe_belief, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(ccb_ex_t5, climate_catastrophe_belief, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(ccb_ex_t10, climate_catastrophe_belief, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The belief acts as a powerful standard for interpreting new information and
% coordinating (in)action.
narrative_ontology:coordination_type(climate_catastrophe_belief, information_standard).

% This belief has profound effects on other social and economic constraints.
narrative_ontology:affects_constraint(climate_catastrophe_belief, youth_mental_health_crisis).
narrative_ontology:affects_constraint(climate_catastrophe_belief, renewable_energy_investment).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The structural derivation chain
% (beneficiary/victim declarations + exit options) accurately models the
% directionality for the key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */