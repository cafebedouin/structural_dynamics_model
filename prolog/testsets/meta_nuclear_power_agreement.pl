% ============================================================================
% CONSTRAINT STORY: meta_nuclear_power_agreement
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_meta_nuclear_power_agreement, []).

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
 *   constraint_id: meta_nuclear_power_agreement
 *   human_readable: Meta's direct investment and offtake agreements for advanced nuclear power
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Meta, a massive energy consumer for its AI data centers, is signing direct
 *   agreements with advanced nuclear power developers (e.g., for Small Modular
 *   Reactors). This represents a contractual framework that coordinates immense
 *   capital investment with guaranteed long-term demand for carbon-free baseload
 *   power. The constraint solves a major financing hurdle for new nuclear but
 *   also suppresses alternative energy pathways and potentially externalizes
 *   long-term risks.
 *
 * KEY AGENTS (by structural relationship):
 *   - Local Communities: Primary target (powerless/trapped) — bears localized risks (environmental, financial) without commensurate control or benefit.
 *   - Legacy Renewable Providers: Secondary target (organized/constrained) — their business model is suppressed by long-term contracts locking in a major customer.
 *   - Meta Platform: Primary beneficiary (institutional/arbitrage) — secures a stable, predictable, carbon-free baseload power source, de-risking its AI expansion.
 *   - Advanced Nuclear Developers: Primary beneficiary (organized/arbitrage) — gain an anchor customer, de-risking massive capital expenditure and accelerating commercialization.
 *   - Analytical Observer: Sees the full structure, including both the coordination function and the asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meta_nuclear_power_agreement, 0.42).
domain_priors:suppression_score(meta_nuclear_power_agreement, 0.55).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(meta_nuclear_power_agreement, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meta_nuclear_power_agreement, extractiveness, 0.42).
narrative_ontology:constraint_metric(meta_nuclear_power_agreement, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(meta_nuclear_power_agreement, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(meta_nuclear_power_agreement, tangled_rope).
narrative_ontology:human_readable(meta_nuclear_power_agreement, "Meta's direct investment and offtake agreements for advanced nuclear power").

% --- Binary flags ---
domain_priors:requires_active_enforcement(meta_nuclear_power_agreement). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(meta_nuclear_power_agreement, meta_platform).
narrative_ontology:constraint_beneficiary(meta_nuclear_power_agreement, advanced_nuclear_developers).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(meta_nuclear_power_agreement, local_communities).
narrative_ontology:constraint_victim(meta_nuclear_power_agreement, legacy_renewable_providers).

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

% PERSPECTIVE 1: THE LOCAL COMMUNITY (TANGLED ROPE)
% As a victim with trapped exit, they face high effective extraction.
% They may see some benefits (jobs, tax base) but bear uncompensated risks
% (environmental, project failure), making it a Tangled Rope rather than
% a pure Snare.
% Engine derives d from: victim + trapped → d≈0.95 → f(d)≈1.42
% χ ≈ ε × f(d) × σ(S) = 0.42 * 1.42 * 0.8 (local) ≈ 0.48.
% (0.40 ≤ χ ≤ 0.90, ε ≥ 0.30, S ≥ 0.40) -> Tangled Rope.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, tangled_rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: META PLATFORM (ROPE)
% As a primary beneficiary with arbitrage exit options, Meta sees this as
% pure coordination solving a critical business need.
% Engine derives d from: beneficiary + arbitrage → d≈0.05 → f(d)≈-0.12
% χ is negative, classifying as a Rope.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the genuine, valuable coordination function (financing new nuclear)
% and the significant asymmetric extraction and suppression of alternatives.
% This dual nature is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: LEGACY RENEWABLE PROVIDERS (TANGLED ROPE)
% As a victim with constrained exit, their business model is actively suppressed.
% The coordination element (grid stability from baseload) is visible but
% secondary to the coercive market impact of losing a major customer segment.
% Engine derives d from: victim + constrained → d≈0.85 → f(d)≈1.28
% χ ≈ ε × f(d) × σ(S) = 0.42 * 1.28 * 1.0 (national) ≈ 0.54 -> Tangled Rope.
constraint_indexing:constraint_classification(meta_nuclear_power_agreement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meta_nuclear_power_agreement_tests).

test(perspectival_gap_beneficiary_vs_victim) :-
    constraint_indexing:constraint_classification(meta_nuclear_power_agreement, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(meta_nuclear_power_agreement, tangled_rope,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    true.

test(tangled_rope_conditions_met) :-
    domain_priors:base_extractiveness(meta_nuclear_power_agreement, E), E >= 0.30,
    domain_priors:suppression_score(meta_nuclear_power_agreement, S), S >= 0.40,
    domain_priors:requires_active_enforcement(meta_nuclear_power_agreement),
    narrative_ontology:constraint_beneficiary(meta_nuclear_power_agreement, _),
    narrative_ontology:constraint_victim(meta_nuclear_power_agreement, _).

:- end_tests(meta_nuclear_power_agreement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.42): The extraction is significant. It's derived from long-term technological lock-in, the potential for cost overruns to be socialized, and the externalization of tail risks (waste disposal, decommissioning, accident). It's not a pure confiscation (like a Snare), but it's a structural transfer of risk and opportunity cost.
 *   - Suppression (S=0.55): The 20-40 year offtake agreements create a powerful moat. They suppress competing bids from other energy sources (renewables, storage, geothermal) for a huge slice of predictable demand, thereby shaping future energy infrastructure development.
 *   - Coordination: The constraint's primary function is to solve the coordination problem of financing new nuclear energy, which has extremely high upfront capital costs and long build times. By providing a guaranteed buyer, Meta de-risks the investment for developers. This is a genuine and valuable coordination function.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For Meta (beneficiary), this is a Rope: a brilliant move to coordinate resources to solve their energy trilemma (clean, reliable, affordable). For local communities and renewable providers (victims), it's a Tangled Rope: a coercive structure that imposes costs, risks, and market suppression, even if it has some broader benefits like grid stability. The classification system correctly captures this divergence, rooting it in their different structural positions (beneficiary vs. victim) and exit options (arbitrage vs. trapped/constrained).
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries (Meta, Nuclear Developers): They are the architects and direct beneficiaries. Meta gets power security; developers get a bankable project. The engine correctly assigns them a low directionality `d`, resulting in a low/negative effective extraction `χ`.
 *   - Victims (Communities, Renewable Providers): They bear the costs. Communities face localized, long-term risks. Renewable providers face direct market suppression. The engine assigns them a high `d`, resulting in a high `χ`, reflecting the extractive nature of the constraint from their perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a prime example of a Tangled Rope that could be misidentified. A naive analysis might see it as a pure Rope ("win-win for clean energy") or a pure Snare ("corporate capture of public resources"). The Deferential Realism framework, by requiring analysis of both the coordination function AND the asymmetric extraction, correctly identifies it as a hybrid. The `tangled_rope` classification forces an acknowledgment that a constraint can simultaneously solve a real problem while creating real victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_meta_nuclear_power_agreement,
    'Will advanced nuclear (SMR) technology deliver on its cost, timeline, and safety promises over a multi-decade horizon?',
    'Empirical data from the first wave of deployed SMRs over their first 10-15 years of operation.',
    'If YES: The constraint is closer to a pure Rope, as the extraction (risk premium) was justified. If NO: The constraint becomes a Snare for investors and communities, locking them into a failed, costly technology.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(meta_nuclear_power_agreement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Modeling the potential for cost overruns and risk realization during the
% construction and early operational phases of a novel technology.
% While ε is not > 0.46, this is a borderline case where drift is a key risk.

% Theater ratio over time (PR value declines as real costs become known):
narrative_ontology:measurement(mnpa_tr_t0, meta_nuclear_power_agreement, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mnpa_tr_t5, meta_nuclear_power_agreement, theater_ratio, 5, 0.15).
narrative_ontology:measurement(mnpa_tr_t10, meta_nuclear_power_agreement, theater_ratio, 10, 0.15).

% Extraction over time (projected costs become real, potentially higher, costs):
narrative_ontology:measurement(mnpa_ex_t0, meta_nuclear_power_agreement, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(mnpa_ex_t5, meta_nuclear_power_agreement, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(mnpa_ex_t10, meta_nuclear_power_agreement, base_extractiveness, 10, 0.42).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(meta_nuclear_power_agreement, resource_allocation).

% Network relationships (structural influence edges)
% This new financing model for baseload power directly impacts the viability
% and political support for subsidies targeted at intermittent renewables.
narrative_ontology:affects_constraint(meta_nuclear_power_agreement, renewable_energy_subsidies).
narrative_ontology:affects_constraint(meta_nuclear_power_agreement, grid_stability_mandates).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% based on beneficiary/victim declarations and exit options accurately
% models the power dynamics of the key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */