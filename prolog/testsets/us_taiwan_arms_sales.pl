% ============================================================================
% CONSTRAINT STORY: us_taiwan_arms_sales
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_us_taiwan_arms_sales, []).

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
 *   constraint_id: us_taiwan_arms_sales
 *   human_readable: US Arms Sales Policy toward Taiwan
 *   domain: geopolitical
 *
 * SUMMARY:
 *   This constraint represents the long-standing US policy of selling
 *   defensive arms to Taiwan, as codified by the Taiwan Relations Act. While
 *   serving a genuine coordination function (deterrence, regional stability),
 *   it also creates a captive market where significant financial value is
 *   extracted by defense contractors and geopolitical costs are imposed on
 *   both Taiwan and the People's Republic of China. The policy requires
 *   active enforcement and suppresses alternative security arrangements.
 *
 * KEY AGENTS (by structural relationship):
 *   - Taiwanese Taxpayers: Ultimate source of extracted funds (powerless/trapped)
 *   - Taiwan (ROC): Primary target of financial extraction, secondary beneficiary of security coordination (powerful/constrained)
 *   - US Defense Contractors: Primary beneficiary of financial extraction (institutional/arbitrage)
 *   - US Government (DoD/State): Institutional architect and beneficiary of geopolitical influence (institutional/constrained)
 *   - People's Republic of China (PRC): Institutional target of geopolitical containment (institutional/constrained)
 *   - Analytical Observer: Sees the full dual-function structure (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_taiwan_arms_sales, 0.52).
domain_priors:suppression_score(us_taiwan_arms_sales, 0.75).   % Structural property (raw, unscaled). High due to lack of alternative suppliers.
domain_priors:theater_ratio(us_taiwan_arms_sales, 0.25).       % Piton detection (< 0.70). High diplomatic theater, but weapons are functional.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_taiwan_arms_sales, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_taiwan_arms_sales, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_taiwan_arms_sales, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint type.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_taiwan_arms_sales, tangled_rope).
narrative_ontology:human_readable(us_taiwan_arms_sales, "US Arms Sales Policy toward Taiwan").

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_taiwan_arms_sales). % Required for Tangled Rope. Policy is actively managed and enforced.

% --- Emergence flag (required for mountain constraints) ---
% N/A. This is a human-constructed policy.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_taiwan_arms_sales, us_defense_contractors).
narrative_ontology:constraint_beneficiary(us_taiwan_arms_sales, us_geopolitical_interests).
narrative_ontology:constraint_beneficiary(us_taiwan_arms_sales, taiwan_security_apparatus). % Dual role

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_taiwan_arms_sales, taiwan_treasury). % Financial cost
narrative_ontology:constraint_victim(us_taiwan_arms_sales, prc_containment_target). % Geopolitical cost

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

% PERSPECTIVE 1: THE POWERLESS TARGET (TAIWANESE TAXPAYER)
% As the ultimate source of funds for the arms sales ('taiwan_treasury' victim
% group) and with no ability to exit the national system, the individual
% taxpayer experiences this as a pure extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(us_taiwan_arms_sales, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TAIWAN (THE POWERFUL TARGET/PARTICIPANT)
% As a member of the 'taiwan_treasury' victim group with 'constrained' exit,
% the engine derives a high d, leading to a high effective extraction (χ).
% The financial cost and geopolitical lock-in make it appear as a Snare,
% even though they also benefit from the security aspect.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: US DEFENSE CONTRACTORS (THE PRIMARY BENEFICIARY)
% As a beneficiary with 'arbitrage' exit, the engine derives a very low d,
% leading to a negative effective extraction (χ). From their perspective, this
% is a pure coordination mechanism that generates revenue; it's a Rope.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The analytical observer sees both the coordination function (security) and
% the asymmetric extraction (financial, geopolitical). The high base extraction,
% high suppression, and active enforcement lead to a Tangled Rope classification.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 5A: US GOVERNMENT (THE ARCHITECT)
% As a beneficiary ('us_geopolitical_interests') but with 'constrained' exit
% due to treaty obligations and policy inertia, the US government sees a useful
% coordination tool. The derived d is higher than for contractors but still low.
% For them, it is a functional, if complex, Rope.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5B: PEOPLE'S REPUBLIC OF CHINA (THE EXTERNAL TARGET)
% As a designated victim ('prc_containment_target') with 'constrained' exit,
% the PRC experiences the policy as pure geopolitical coercion with no
% coordination benefit. The derived d is high, making this a clear Snare.
constraint_indexing:constraint_classification(us_taiwan_arms_sales, snare,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(constrained),
            spatial_scope(continental))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_taiwan_arms_sales_tests).

test(perspectival_gap_beneficiary_vs_target, [nondet]) :-
    % Verify the gap between the powerless target and the institutional beneficiary.
    constraint_indexing:constraint_classification(us_taiwan_arms_sales, TypeTarget, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_taiwan_arms_sales, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    assertion(TypeTarget == snare),
    assertion(TypeBeneficiary == rope),
    TypeTarget \= TypeBeneficiary.

test(inter_institutional_gap_us_vs_prc) :-
    % Find all classifications for institutional actors with constrained exit.
    findall(Type,
            constraint_indexing:constraint_classification(us_taiwan_arms_sales, Type,
                context(agent_power(institutional), _, exit_options(constrained), _)),
            Types),
    % Check that both rope (US Gov) and snare (PRC) are present.
    assertion(member(rope, Types)),
    assertion(member(snare, Types)).

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(us_taiwan_arms_sales, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(us_taiwan_arms_sales, _),
    narrative_ontology:constraint_victim(us_taiwan_arms_sales, _),
    domain_priors:requires_active_enforcement(us_taiwan_arms_sales).

:- end_tests(us_taiwan_arms_sales_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.52): This reflects the non-competitive nature
 *     of the arms market. The price includes not just production costs and
 *     standard profit, but a significant premium for political access,
 *     integration, and the lack of alternative suppliers.
 *   - Suppression (0.75): Extremely high. The US government actively prevents
 *     Taiwan from acquiring equivalent top-tier systems from other nations,
 *     creating a captive market.
 *   - The combination of a genuine security function (beneficiary: taiwan_security_apparatus)
 *     and high, asymmetric extraction (victim: taiwan_treasury) with active
 *     enforcement is the definitional signature of a Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. US defense contractors, with arbitrage exit and beneficiary
 *   status, see a pure Rope (a beneficial coordination system). Taiwanese taxpayers,
 *   trapped and bearing the financial cost, experience it as a Snare. This
 *   accurately models the tension where Taiwan as a state needs the security
 *   but its people resent the cost and dependency. The PRC, being the target
 *   of the containment policy, also sees a clear Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the explicit beneficiary/victim declarations.
 *   - `us_defense_contractors`: Beneficiary + arbitrage exit -> lowest d -> negative χ (Rope)
 *   - `us_geopolitical_interests`: Beneficiary + constrained exit -> low d -> low χ (Rope)
 *   - `taiwan_treasury`: Victim + trapped/constrained exit -> high d -> high χ (Snare)
 *   - `prc_containment_target`: Victim + constrained exit -> high d -> high χ (Snare)
 *   This structural derivation correctly assigns directionality without needing manual
 *   overrides, reflecting the complex, multi-polar nature of the constraint.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model captures the dynamic between three institutional actors: the US,
 *   Taiwan, and the PRC. Although both the US government and the PRC are
 *   'institutional' with 'constrained' exit, their classification diverges
 *   based on their structural relationship to the constraint. The US is a
 *   beneficiary (architect of a system that projects its power), seeing a Rope.
 *   The PRC is a victim (target of containment), seeing a Snare. This demonstrates
 *   how directionality disambiguates perspectives even when P, T, E, S are similar.
 *
 * MANDATROPHY ANALYSIS:
 *   This model prevents mandatrophy by refusing to simplify the situation.
 *   A purely cynical analysis would label it a Snare, ignoring the real
 *   deterrence function. A purely idealistic analysis would label it a Rope,
 *   ignoring the immense financial extraction and geopolitical lock-in.
 *   The Tangled Rope classification from the analytical perspective correctly
 *   identifies and holds both realities in tension, which is the core
 *   purpose of the category.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_us_taiwan_arms_sales,
    'Is the security coordination a net deterrent, or does it primarily escalate regional tension, creating the very demand it purports to service?',
    'Counterfactual historical analysis of regional conflict trajectories without the Taiwan Relations Act (epistemically inaccessible).',
    'If net deterrent, the "Rope" aspect is stronger. If net escalator, the "Snare" aspect is stronger, and ε is likely underestimated.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_taiwan_arms_sales, 1979, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This policy began with the Taiwan Relations Act in 1979. Over time, the
% sophistication and cost of weapons systems have increased, raising the
% base extractiveness. ε > 0.46, so this section is required.

% Theater ratio over time (slight increase in diplomatic signaling)
narrative_ontology:measurement(us_taiwan_arms_sales_tr_t0, us_taiwan_arms_sales, theater_ratio, 1979, 0.15).
narrative_ontology:measurement(us_taiwan_arms_sales_tr_t23, us_taiwan_arms_sales, theater_ratio, 2002, 0.20).
narrative_ontology:measurement(us_taiwan_arms_sales_tr_t46, us_taiwan_arms_sales, theater_ratio, 2025, 0.25).

% Extraction over time (increasing cost and technological dependency)
narrative_ontology:measurement(us_taiwan_arms_sales_ex_t0, us_taiwan_arms_sales, base_extractiveness, 1979, 0.40).
narrative_ontology:measurement(us_taiwan_arms_sales_ex_t23, us_taiwan_arms_sales, base_extractiveness, 2002, 0.47).
narrative_ontology:measurement(us_taiwan_arms_sales_ex_t46, us_taiwan_arms_sales, base_extractiveness, 2025, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This policy is fundamentally about enforcing a security arrangement.
narrative_ontology:coordination_type(us_taiwan_arms_sales, enforcement_mechanism).

% Network relationships (structural influence edges)
% This policy is inextricably linked to the broader US-China geopolitical
% strategy and the global technology supply chain.
narrative_ontology:affects_constraint(us_taiwan_arms_sales, us_strategic_ambiguity_policy).
narrative_ontology:affects_constraint(us_taiwan_arms_sales, global_semiconductor_chokepoint).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation chain,
% using the beneficiary/victim declarations and exit options, accurately
% models the directionality for all key agents and correctly differentiates
% the inter-institutional perspectives.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */