% ============================================================================
% CONSTRAINT STORY: us_isolationism_policy_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_us_isolationism_policy_2026, []).

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
 *   constraint_id: us_isolationism_policy_2026
 *   human_readable: US Withdrawal from International Organizations under "America First" Doctrine
 *   domain: geopolitical
 *
 * SUMMARY:
 *   This constraint models the policy of a potential second Trump administration
 *   to withdraw the United States from key international organizations (like the
 *   UN, WHO) and treaties. This action is framed as reclaiming national
 *   sovereignty and redirecting funds domestically, but it fundamentally alters
 *   the post-WWII global order by removing its primary architect and sponsor.
 *   The constraint is the new reality imposed on the international system,
 *   extracting US resources and participation from it.
 *
 * KEY AGENTS (by structural relationship):
 *   - US Executive Branch (Architects): Primary beneficiary (institutional/arbitrage) — benefits by consolidating power and redirecting resources.
 *   - US Nationalist Base: Secondary beneficiary (organized/mobile) — sees policy as fulfilling campaign promises.
 *   - International Organizations (UN, WHO): Primary target (institutional/constrained) — lose funding, legitimacy, and their most powerful member.
 *   - Allied Nations (NATO, G7): Secondary target (institutional/constrained) — forced to restructure alliances and security guarantees in a less stable world.
 *   - Analytical Observer: Sees the full structure of extraction and coordination collapse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_isolationism_policy_2026, 0.68). % Represents withheld funds, redirected political capital, and reclaimed regulatory freedom.
domain_priors:suppression_score(us_isolationism_policy_2026, 0.75).   % Structural property (raw, unscaled). Alternatives to the US-led order are actively suppressed by the vacuum created.
domain_priors:theater_ratio(us_isolationism_policy_2026, 0.20).       % Piton detection (>= 0.70). Low, as the actions have concrete, non-performative effects.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_isolationism_policy_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_isolationism_policy_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_isolationism_policy_2026, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_isolationism_policy_2026, snare).

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_isolationism_policy_2026). % Withdrawal requires continuous policy action and defiance of international norms.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_isolationism_policy_2026, us_executive_branch).
narrative_ontology:constraint_beneficiary(us_isolationism_policy_2026, us_nationalist_base).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_isolationism_policy_2026, international_organizations).
narrative_ontology:constraint_victim(us_isolationism_policy_2026, allied_nations).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (as powerless proxy)
% An NGO or smaller nation completely dependent on the international system.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(us_isolationism_policy_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (US EXECUTIVE)
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(us_isolationism_policy_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15. With ε=0.68 and σ(global)=1.2,
% χ = 0.68 * 1.15 * 1.2 ≈ 0.94, which is a clear Snare.
constraint_indexing:constraint_classification(us_isolationism_policy_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% This highlights the gap between institutions with different relationships to the policy.

% Perspective 4A: International Organizations (e.g., UN, WHO)
% As victims with constrained exit, their derived `d` is high, leading to a Snare classification.
constraint_indexing:constraint_classification(us_isolationism_policy_2026, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 4B: Allied Nations (e.g., a NATO member)
% Also victims, they see the withdrawal as a Snare that undermines collective security.
% Their exit is constrained because they cannot easily replace the US security guarantee.
constraint_indexing:constraint_classification(us_isolationism_policy_2026, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_isolationism_policy_2026_tests).

test(perspectival_gap_beneficiary_vs_victim) :-
    % Verify the core perspectival gap between the US Executive and an international body.
    constraint_indexing:constraint_classification(us_isolationism_policy_2026, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(us_isolationism_policy_2026, snare,
        context(agent_power(institutional), _, exit_options(constrained), _)).

test(analytical_view_is_snare) :-
    constraint_indexing:constraint_classification(us_isolationism_policy_2026, snare,
        context(agent_power(analytical), _, _, _)).

test(snare_metric_thresholds_met) :-
    domain_priors:base_extractiveness(us_isolationism_policy_2026, E), E >= 0.46,
    domain_priors:suppression_score(us_isolationism_policy_2026, S), S >= 0.60.

:- end_tests(us_isolationism_policy_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.68): High, representing the tangible withdrawal of funding, personnel, and political backing from the global system. This value is extracted from the international community and redirected towards domestic priorities or executive discretion.
 *   - Suppression Score (0.75): High. The US withdrawal actively dismantles the primary mechanism for global coordination, suppressing the ability of other nations to address transnational problems effectively and creating a power vacuum that is difficult to fill.
 *   - The constraint is classified as a Snare from the analytical view because it lacks a genuine, shared coordination function between its beneficiaries (US nationalists) and its victims (the international community). Instead, it imposes high costs on the latter for the benefit of the former through coercive means (defunding, treaty abandonment).
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The US Executive, as the architect, views the policy as a Rope: a tool to coordinate national resources for its own benefit, freeing it from unwanted obligations (negative extraction). Conversely, international bodies and allied nations experience it as a Snare: a destructive, coercive act that extracts vital resources and stability from the system, leaving them trapped with no good alternatives. This is a classic example of how directionality (d) driven by structural relationships (beneficiary vs. victim) and exit options (arbitrage vs. constrained) creates radically different classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The `us_executive_branch` and `us_nationalist_base` directly benefit. The policy grants the executive more unilateral power and is perceived by the base as reclaiming sovereignty. Their low directionality score (`d`) reflects this.
 *   - Victims: `international_organizations` and `allied_nations` bear the full cost. They lose funding, security guarantees, and a stable framework for cooperation. Their high directionality score (`d`) reflects their position as the targets of extraction. The derivation chain correctly captures this asymmetry.
 *
* INTER-INSTITUTIONAL DYNAMICS:
 *   This is a prime example of inter-institutional conflict. Both the US Executive and the UN are `institutional` actors, but their relationship to the constraint is inverted. The US Executive has `arbitrage` exit (it chooses what to leave), while the UN and allies have `constrained` exit (they must endure the consequences). The system correctly assigns different `d` values, resulting in the Rope vs. Snare classification gap between them, even though they share the same power level.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework prevents mislabeling the policy as a mere "political disagreement." By quantifying the high extraction (ε) and suppression, and modeling the perspectival gap, it identifies the policy's structure as a Snare. It avoids the trap of accepting the beneficiary's "coordination" narrative (this is a Rope for us) at face value, instead revealing the asymmetric extraction at its core.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_us_isolationism_policy_2026,
    'Will the withdrawal ultimately strengthen US sovereignty and economic standing, or will it create a power vacuum filled by adversaries, leading to greater instability and net harm to the US?',
    'Observation of geopolitical power balances, global trade flows, and US domestic economic indicators over the decade following the policy implementation (2026-2036).',
    'If the policy is beneficial, it may be re-classified as a harsh but effective Scaffold for a new domestic order. If harmful, it remains a Snare that also trapped its architect.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_isolationism_policy_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the policy's implementation over time, showing a shift from
% theatrical threats to concrete extractive actions.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (starts high with rhetoric, drops as action is taken):
narrative_ontology:measurement(us_iso_tr_t0, us_isolationism_policy_2026, theater_ratio, 0, 0.60).
narrative_ontology:measurement(us_iso_tr_t5, us_isolationism_policy_2026, theater_ratio, 5, 0.20).
narrative_ontology:measurement(us_iso_tr_t10, us_isolationism_policy_2026, theater_ratio, 10, 0.15).

% Extraction over time (increases as withdrawals are finalized and funds redirected):
narrative_ontology:measurement(us_iso_ex_t0, us_isolationism_policy_2026, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(us_iso_ex_t5, us_isolationism_policy_2026, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(us_iso_ex_t10, us_isolationism_policy_2026, base_extractiveness, 10, 0.72).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: This policy is a mechanism to enforce a new nationalist order.
narrative_ontology:coordination_type(us_isolationism_policy_2026, enforcement_mechanism).

% Network relationships: This policy directly impacts numerous other global constraints.
narrative_ontology:affects_constraint(us_isolationism_policy_2026, climate_accords_paris).
narrative_ontology:affects_constraint(us_isolationism_policy_2026, global_supply_chains).
narrative_ontology:affects_constraint(us_isolationism_policy_2026, iran_nuclear_deal_jcpoa).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation chain,
% using beneficiary/victim declarations and exit_options, accurately models the
% directionality for all key agents. The stark contrast between the `arbitrage`
% exit of the beneficiary and the `constrained` exit of the victims is the
% primary driver of the perspectival gap, and the engine captures this correctly.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */