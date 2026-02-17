% ============================================================================
% CONSTRAINT STORY: strategic_deep_sea_rare_earth_mining
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_strategic_deep_sea_rare_earth_mining, []).

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
 *   constraint_id: strategic_deep_sea_rare_earth_mining
 *   human_readable: Strategic Deep-Sea Mining for Rare Earth Minerals
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   This constraint represents Japan's state-led initiative to develop
 *   deep-sea mining technology to extract rare earth elements (REEs) from its
 *   exclusive economic zone. The goal is to create a secure, domestic supply
 *   chain for critical high-tech manufacturing, thereby reducing geopolitical
 *   and economic dependence on China, the current dominant supplier. This
 *   initiative involves coordinating massive public and private investment
 *   while externalizing significant, and largely unquantified, environmental
 *   costs onto marine ecosystems.
 *
 * KEY AGENTS (by structural relationship):
 *   - Japanese State & High-Tech Industry: Primary beneficiary (institutional/arbitrage) — gains geopolitical leverage and supply chain security.
 *   - Marine Ecosystems & Future Generations: Primary target (powerless/trapped) — bears the direct, irreversible environmental costs. Represented by environmental advocacy groups.
 *   - Japanese Taxpayers: Secondary target (moderate/constrained) — bears the financial risk and cost of public investment.
 *   - China (Incumbent REE Supplier): Secondary institutional actor (institutional/arbitrage) — benefits from the status quo; this initiative is a constraint on their market power.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strategic_deep_sea_rare_earth_mining, 0.55).
domain_priors:suppression_score(strategic_deep_sea_rare_earth_mining, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(strategic_deep_sea_rare_earth_mining, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strategic_deep_sea_rare_earth_mining, extractiveness, 0.55).
narrative_ontology:constraint_metric(strategic_deep_sea_rare_earth_mining, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(strategic_deep_sea_rare_earth_mining, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(strategic_deep_sea_rare_earth_mining, tangled_rope).
narrative_ontology:human_readable(strategic_deep_sea_rare_earth_mining, "Strategic Deep-Sea Mining for Rare Earth Minerals").

% --- Binary flags ---
domain_priors:requires_active_enforcement(strategic_deep_sea_rare_earth_mining). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(strategic_deep_sea_rare_earth_mining, japanese_state).
narrative_ontology:constraint_beneficiary(strategic_deep_sea_rare_earth_mining, japanese_high_tech_industry).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(strategic_deep_sea_rare_earth_mining, global_marine_ecosystems).
narrative_ontology:constraint_victim(strategic_deep_sea_rare_earth_mining, japanese_taxpayers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present)

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Represented by environmental groups advocating for marine ecosystems.
% They are powerless against state action, trapped with the consequences,
% and view the initiative as pure, destructive extraction.
% Engine derives d from victim + trapped → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.55 * 1.42 * 1.2 (global scope) ≈ 0.94, which is a clear Snare.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The Japanese state and affiliated high-tech industries. They see this
% as a vital coordination effort to solve a national security problem.
% Engine derives d from beneficiary + arbitrage → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.55 * -0.12 * 1.0 (national scope) ≈ -0.07, a clear Rope.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the genuine coordination function (solving supply chain dependency)
% and the severe, asymmetric extraction (environmental/financial costs).
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.55 * 1.15 * 1.2 (global scope) ≈ 0.76, classifying as Tangled Rope.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE JAPANESE TAXPAYER (TANGLED ROPE)
% Funds the high-risk venture through taxes but receives diffuse benefits.
% They are a victim group but have more power/exit than the ecosystem.
% Engine derives d from victim + constrained exit.
% This results in a high-but-not-maximal χ, likely a Tangled Rope.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strategic_deep_sea_rare_earth_mining_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, snare, context(agent_power(powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, rope, context(agent_power(institutional), _, arbitrage, _)),
    format('... Perspectival gap validated (Snare vs Rope).~n').

test(analytical_observer_sees_tangled_rope) :-
    % The analytical view must resolve the gap to Tangled Rope.
    constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_compliance) :-
    % Verify all three structural conditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(strategic_deep_sea_rare_earth_mining, _),
    narrative_ontology:constraint_victim(strategic_deep_sea_rare_earth_mining, _),
    domain_priors:requires_active_enforcement(strategic_deep_sea_rare_earth_mining).

:- end_tests(strategic_deep_sea_rare_earth_mining_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High, reflecting the massive externalization of environmental costs and the concentration of geopolitical/economic benefits. The cost of potential ecosystem collapse is treated as an externality.
 *   - Suppression (0.70): High. The constraint arises from the lack of viable alternatives to China's REE dominance. This initiative suppresses other potential solutions (e.g., radical recycling R&D, circular economy mandates) by consuming vast capital and political will.
 *   - Classification: The constraint is a textbook Tangled Rope. It has a legitimate, high-value coordination function (securing national supply chains) intertwined with severe, asymmetrically distributed extraction (environmental degradation, public financial risk).
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the Japanese state (beneficiary), this is a Rope—a tool for national security and economic stability. For the marine ecosystem (victim), this is a Snare—an extractive process with catastrophic, irreversible consequences and no possibility of exit. The analytical observer resolves this by recognizing that both descriptions are true simultaneously, which is the definition of a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The Japanese state and high-tech industry directly capture the upside of a secure REE supply. Their `arbitrage` exit option (continuing to buy from China) gives them leverage, leading to a low derived directionality `d` and a Rope classification from their perspective.
 *   - Victims: The marine ecosystem is the primary cost-bearer. It is `trapped` with no recourse. This maximal victimhood drives `d` towards 1.0, resulting in a high effective extraction (χ) and a Snare classification. Japanese taxpayers are secondary victims, `constrained` by the tax system funding the project.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework correctly identifies the dual nature of the policy. A simplistic analysis might label it purely as "good" (national security) or "bad" (environmental destruction). The Tangled Rope classification prevents this by forcing an acknowledgment of both its coordination function and its extractive nature. It shows that one does not negate the other; they coexist. This avoids the mandatrophy error of mistaking a self-serving extractive project for a purely coordinative public good.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_strategic_deep_sea_rare_earth_mining,
    'Will the long-term environmental damage from deep-sea mining outweigh the geopolitical and economic benefits of supply chain independence?',
    'Longitudinal (multi-decadal) ecological studies of mined seabeds vs. analysis of high-tech sector stability and pricing.',
    'If damage outweighs benefit, the constraint is fundamentally a net-negative Snare masquerading as a Tangled Rope. If benefits are substantial and damage is manageable, the Tangled Rope classification holds.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(strategic_deep_sea_rare_earth_mining, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection for this high-extraction (ε=0.55) constraint.
% The timeline models the shift from a high-theater planning phase to a
% high-extraction operational phase.

% Theater ratio over time (starts high with announcements, drops as it becomes operational):
narrative_ontology:measurement(sdsrem_tr_t0, strategic_deep_sea_rare_earth_mining, theater_ratio, 0, 0.30).
narrative_ontology:measurement(sdsrem_tr_t5, strategic_deep_sea_rare_earth_mining, theater_ratio, 5, 0.25).
narrative_ontology:measurement(sdsrem_tr_t10, strategic_deep_sea_rare_earth_mining, theater_ratio, 10, 0.20).

% Extraction over time (starts lower during R&D, increases as physical mining begins):
narrative_ontology:measurement(sdsrem_ex_t0, strategic_deep_sea_rare_earth_mining, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(sdsrem_ex_t5, strategic_deep_sea_rare_earth_mining, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(sdsrem_ex_t10, strategic_deep_sea_rare_earth_mining, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a classic example of securing inputs for an industrial base.
narrative_ontology:coordination_type(strategic_deep_sea_rare_earth_mining, resource_allocation).

% Network relationships (structural influence edges)
% This initiative is structurally linked to global tech supply chains and
% geopolitical tensions with China.
narrative_ontology:affects_constraint(strategic_deep_sea_rare_earth_mining, semiconductor_supply_chain).
narrative_ontology:affects_constraint(strategic_deep_sea_rare_earth_mining, geopolitical_dependency_china).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation from
% beneficiary/victim declarations and exit options accurately models the
% structural relationships between the agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */