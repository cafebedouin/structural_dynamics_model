% ============================================================================
% CONSTRAINT STORY: disney_openai_ip_exclusivity
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-24
% ============================================================================

:- module(constraint_disney_openai_ip_exclusivity, []).

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
 *   constraint_id: disney_openai_ip_exclusivity
 *   human_readable: Exclusive IP licensing for generative AI training (Disney/OpenAI)
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Based on a hypothetical 2025 scenario, Disney invests $1B in OpenAI and
 *   grants it exclusive access to its top-200 character IP library for training
 *   the Sora video generation model. This constraint represents the legal and
 *   technical framework of the deal, which coordinates the two giants while
 *   simultaneously suppressing the ability of competitors to access the same
 *   foundational training data, thereby creating a significant market moat.
 *
 * KEY AGENTS (by structural relationship):
 *   - Independent Media Creators: Primary target (powerless/trapped) — bears extraction by being locked out of the creative ecosystem.
 *   - Disney/OpenAI Partnership: Primary beneficiary (institutional/arbitrage) — benefits from market consolidation and a powerful competitive advantage.
 *   - Antitrust Regulators: Institutional observer (institutional/constrained) — sees both coordination and anti-competitive extraction.
 *   - Analytical Observer: Sees the full hybrid structure of the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(disney_openai_ip_exclusivity, 0.55).
domain_priors:suppression_score(disney_openai_ip_exclusivity, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(disney_openai_ip_exclusivity, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(disney_openai_ip_exclusivity, extractiveness, 0.55).
narrative_ontology:constraint_metric(disney_openai_ip_exclusivity, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(disney_openai_ip_exclusivity, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(disney_openai_ip_exclusivity, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(disney_openai_ip_exclusivity). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(disney_openai_ip_exclusivity, disney_openai_partnership).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(disney_openai_ip_exclusivity, independent_media_creators).
narrative_ontology:constraint_victim(disney_openai_ip_exclusivity, competing_ai_labs).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Independent creators are victims with no access to the data (trapped).
% The engine derives d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.55 * 1.42 * 1.2 (global scope) ≈ 0.94. This is a clear Snare.
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% For the Disney/OpenAI partnership, this is a pure coordination mechanism.
% They are beneficiaries with arbitrage exit.
% The engine derives d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.55 * -0.12 * 1.2 ≈ -0.08. This is a clear Rope (coordination with subsidy).
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical observer sees both the coordination and the extraction.
% With ε=0.55 and suppression=0.75, it meets the criteria for a Tangled Rope.
% The engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.55 * 1.15 * 1.2 ≈ 0.76. This is a clear Tangled Rope.
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: INTER-INSTITUTIONAL (ANTITRUST REGULATOR)
% A regulator is institutional, but constrained by existing law and political
% pressure, giving them less freedom than the deal's beneficiaries. They see
% the anti-competitive nature (extraction) alongside the business logic
% (coordination), classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(disney_openai_ip_exclusivity_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    % The integrated analytical view must be Tangled Rope.
    constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % Verify that all three conditions for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(disney_openai_ip_exclusivity, _),
    narrative_ontology:constraint_victim(disney_openai_ip_exclusivity, _),
    domain_priors:requires_active_enforcement(disney_openai_ip_exclusivity).

:- end_tests(disney_openai_ip_exclusivity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High. The value being extracted is not just
 *     licensing fees, but future market dominance in AI-generated media. The
 *     deal transfers enormous potential value from the open market to the
 *     closed partnership.
 *   - Suppression Score (s=0.75): Very high. The core of the deal is exclusivity.
 *     It actively suppresses the ability of any other entity to build a
 *     competitor model using the world's most recognizable IP, creating an
 *     almost insurmountable barrier to entry.
 *   - The constraint requires active enforcement (legal, technical) and has both
 *     clear beneficiaries and victims, meeting all structural requirements for a
 *     Tangled Rope from an analytical perspective.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the Disney/OpenAI partnership (beneficiary), the deal
 *   is a brilliant act of coordination (Rope) that unlocks immense value. For
 *   an independent creator (victim), the same deal is a market-closing Snare that
 *   makes their own creative endeavors less viable by concentrating power and data.
 *   They experience only the coercive downside.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived directly from the deal's structure. The
 *   `disney_openai_partnership` is declared the `constraint_beneficiary`,
 *   giving them a low `d` value and a low/negative effective extraction (χ).
 *   The `independent_media_creators` are the `constraint_victim` with `trapped`
 *   exit, giving them a high `d` value and thus a very high χ. This correctly
 *   models the asymmetric flow of value and opportunity.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two errors. A pure Snare analysis would
 *   miss the genuine, powerful coordination function between the two corporate
 *   giants. A pure Rope analysis would ignore the immense coercive and
 *   extractive effect on the rest of the market. The Tangled Rope classification
 *   captures the essential duality: it is a tool of coordination *for the purpose*
 *   of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_disney_openai_ip_exclusivity,
    'Will this partnership lead to a net increase or decrease in overall creative innovation in the long term?',
    'Observing the diversity, quality, and accessibility of media production tools and content 5-10 years post-deal.',
    'If innovation increases, the deal could be re-evaluated as having a stronger coordination function than initially assessed. If it decreases (market consolidation and stagnation), the Snare component is dominant.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(disney_openai_ip_exclusivity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction (ε=0.55 > 0.46) constraint.
% Models the solidification of the deal's market power over time.

% Theater ratio over time: Stays low as this is a functional, not performative, deal.
narrative_ontology:measurement(disney_openai_ip_exclusivity_tr_t0, disney_openai_ip_exclusivity, theater_ratio, 0, 0.05).
narrative_ontology:measurement(disney_openai_ip_exclusivity_tr_t5, disney_openai_ip_exclusivity, theater_ratio, 5, 0.10).
narrative_ontology:measurement(disney_openai_ip_exclusivity_tr_t10, disney_openai_ip_exclusivity, theater_ratio, 10, 0.10).

% Extraction over time: Starts high and solidifies as the competitive moat is proven.
narrative_ontology:measurement(disney_openai_ip_exclusivity_ex_t0, disney_openai_ip_exclusivity, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(disney_openai_ip_exclusivity_ex_t5, disney_openai_ip_exclusivity, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(disney_openai_ip_exclusivity_ex_t10, disney_openai_ip_exclusivity, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This deal is fundamentally about allocating an exclusive resource (IP data).
narrative_ontology:coordination_type(disney_openai_ip_exclusivity, resource_allocation).

% Network relationships (structural influence edges)
% This IP exclusivity deal directly harms the viability of independent players.
narrative_ontology:affects_constraint(disney_openai_ip_exclusivity, market_viability_of_independent_animation).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately reflects the
% power dynamics of the scenario.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */