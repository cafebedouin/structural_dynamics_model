% ============================================================================
% CONSTRAINT STORY: us_sdf_alliance_abandonment_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_us_sdf_alliance_abandonment_2026, []).

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
 *   constraint_id: us_sdf_alliance_abandonment_2026
 *   human_readable: US Strategic Alliance Abandonment (Syria 2026)
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The constraint represents the strategic alliance between the United States
 *   and the Kurdish-led Syrian Democratic Forces (SDF). Initially formed as a
 *   coordination mechanism to defeat ISIS, the alliance was fundamentally
 *   asymmetric. The US abruptly withdraws support in 2026, allowing a new
 *   Syrian government to overrun SDF territory, revealing the extractive
 *   nature of the relationship where the US transferred long-term existential
 *   risk to its local partner.
 *
 * KEY AGENTS (by structural relationship):
 *   - Kurdish SDF: Primary target (organized/trapped) — bore the risk and cost of abandonment.
 *   - Civilians in AANES: Secondary target (powerless/trapped) — ultimate victims of the instability.
 *   - US Government: Primary beneficiary (institutional/arbitrage) — achieved military goals with low cost and exited without bearing the consequences faced by its partner.
 *   - New Syrian Government & Turkey: Secondary beneficiary (institutional/mobile) — capitalized on the US withdrawal to seize territory.
 *   - Foreign Policy Analyst: Analytical observer — sees the full structure of coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_sdf_alliance_abandonment_2026, 0.65).
domain_priors:suppression_score(us_sdf_alliance_abandonment_2026, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_sdf_alliance_abandonment_2026, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_sdf_alliance_abandonment_2026, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_sdf_alliance_abandonment_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_sdf_alliance_abandonment_2026, theater_ratio, 0.40).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_sdf_alliance_abandonment_2026, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_sdf_alliance_abandonment_2026). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_sdf_alliance_abandonment_2026, us_government).
narrative_ontology:constraint_beneficiary(us_sdf_alliance_abandonment_2026, syrian_turkish_alliance).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_sdf_alliance_abandonment_2026, kurdish_sdf).
narrative_ontology:constraint_victim(us_sdf_alliance_abandonment_2026, civilians_in_aanes).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present)
%   Snare:        victim required; beneficiary optional

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

% PERSPECTIVE 1A: THE PRIMARY TARGET (THE KURDISH SDF)
% The alliance became a trap. They invested everything based on US security
% guarantees, leaving them with no alternatives when support was withdrawn.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → high χ.
% The SDF, though an 'organized' military force, is powerless relative to its
% state-level patrons and adversaries.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 1B: THE CIVILIAN POPULATION (POWERLESS)
% Civilians in the region have no agency in these geopolitical decisions but
% suffer the most direct consequences of the alliance's collapse.
% Engine derives d from: victim membership + trapped exit -> d ≈ 0.95 -> high χ.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (THE US GOVERNMENT)
% The alliance was a highly effective, low-cost coordination tool. From a US
% strategic viewpoint, it achieved its objective (destroying ISIS caliphate) and
% was terminated when it was no longer a priority.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → low/negative χ.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% A historian or foreign policy analyst sees both the genuine coordination
% function and the severe asymmetric extraction, classifying it as a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% This highlights the difference between the two main parties to the alliance.

% Perspective 4A: Kurdish SDF Command (Organized Actor, Trapped)
% Same as Perspective 1A, but explicitly framed as an institutional actor.
% For them, the alliance's collapse is an existential threat.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% Perspective 4B: US State Dept/Pentagon (Institutional Actor, Arbitrage)
% Same as Perspective 2. For them, the alliance is one of many foreign
% policy tools in a global portfolio, to be used or discarded as needed.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_sdf_alliance_abandonment_2026_tests).

test(perspectival_gap_organized_beneficiary) :-
    % Verify the core perspectival gap between the SDF and the US.
    constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, snare, context(agent_power(organized), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(perspectival_gap_powerless_beneficiary) :-
    % Verify the gap between the most vulnerable population and the beneficiary.
    constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements) :-
    % Verify that all structural requirements for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(us_sdf_alliance_abandonment_2026, _),
    narrative_ontology:constraint_victim(us_sdf_alliance_abandonment_2026, _),
    domain_priors:requires_active_enforcement(us_sdf_alliance_abandonment_2026).

:- end_tests(us_sdf_alliance_abandonment_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.65): High. This value represents the massive
 *     existential risk transferred from the US to the SDF. The US achieved its
 *     counter-terrorism goals at a fraction of the cost in blood and treasure
 *     it would have taken to do so alone, while the SDF paid for the alliance
 *     with its long-term security and political viability.
 *   - Suppression (S=0.75): High. US patronage was conditional, actively
 *     discouraging the SDF from pursuing alternative alliances or deals (e.g.,
 *     with the Damascus government) that might have provided a long-term
 *     survival path. This reliance on a single patron was a core feature of
 *     the trap.
 *   - Theater (T=0.40): Moderate. The alliance had a real, functional purpose for
 *     many years. However, the rhetoric of "steadfast partnership" became
 *     increasingly theatrical as US strategic priorities shifted, masking the
 *     contingent and temporary nature of the commitment.
 *
 * PERSPECTIVAL GAP:
 *   - The SDF (organized) and civilians (powerless) experience the alliance as a
 *     Snare because they were induced to over-invest in a partnership where they
 *     had no leverage and no safe exit. The final outcome, abandonment, is the
 *     realization of the trap.
 *   - The US experiences it as a Rope because, from its perspective, it was a
 *     successful, voluntary coordination mechanism that achieved its stated
 *     goals. The US's ability to exit at will (arbitrage) means it never
 *     experiences the coercive, extractive aspects of the constraint.
 *   - The Analytical observer sees both sides and correctly identifies it as a
 *     Tangled Rope: a structure with a genuine coordination function that is
 *     also built on a foundation of severe, asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `us_government`. The US benefited by outsourcing its ground
 *     war against ISIS, saving American lives and resources. `syrian_turkish_alliance`
 *     benefited secondarily from the power vacuum created by the withdrawal.
 *   - Victim: `kurdish_sdf` and `civilians_in_aanes`. They bore the vast majority
 *     of the combat casualties and were ultimately sacrificed for broader
 *     geopolitical shifts.
 *   - The engine uses this data to derive directionality: SDF and civilians as victims with
 *     `trapped` exit get a high `d` (~0.95), making effective extraction χ very
 *     high. The US as a beneficiary with `arbitrage` exit gets a low `d` (~0.05),
 *     making χ low or negative. This mathematical gap reflects the structural reality.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors. It does not dismiss
 *   the alliance as a pure Snare from the outset, acknowledging the real
 *   coordination (the Rope element) that defeated ISIS. It also avoids framing
 *   it as a simple Rope, which would ignore the profound power asymmetry and
 *   the extractive risk transfer that was always present. The Tangled Rope
 *   classification captures the dual nature that is essential to understanding
 *   such geopolitical arrangements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_us_sdf_alliance_abandonment_2026,
    'Was the eventual abandonment an intended outcome from the start (a cynical use of a proxy), or the result of strategic drift and shifting priorities within the US government?',
    'Access to classified US strategic planning documents from the 2014-2018 period.',
    'If intended, the constraint was always a Snare masked as a Rope. If drift, it was a Rope that degraded into a Tangled Rope before the final collapse.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_sdf_alliance_abandonment_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The constraint degraded over its lifecycle. It began as a more functional
% partnership and became more extractive and theatrical over time.
% Base extractiveness is high (>0.46), so temporal data is required.

% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(us_sdf_tr_t0, us_sdf_alliance_abandonment_2026, theater_ratio, 0, 0.10).
narrative_ontology:measurement(us_sdf_tr_t5, us_sdf_alliance_abandonment_2026, theater_ratio, 5, 0.30).
narrative_ontology:measurement(us_sdf_tr_t10, us_sdf_alliance_abandonment_2026, theater_ratio, 10, 0.40).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(us_sdf_ex_t0, us_sdf_alliance_abandonment_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(us_sdf_ex_t5, us_sdf_alliance_abandonment_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(us_sdf_ex_t10, us_sdf_alliance_abandonment_2026, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It allocated military resources and enforced a security zone.
narrative_ontology:coordination_type(us_sdf_alliance_abandonment_2026, resource_allocation).
narrative_ontology:coordination_type(us_sdf_alliance_abandonment_2026, enforcement_mechanism).

% Network relationships: This constraint is deeply coupled with other regional power dynamics.
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, turkish_border_security).
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, iranian_regional_influence).
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, syrian_civil_war_resolution).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The structural derivation chain
% (using beneficiary/victim declarations and exit_options) correctly computes
% the directionality `d` for all key agents, reflecting the asymmetric power
% dynamic at the heart of the constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */