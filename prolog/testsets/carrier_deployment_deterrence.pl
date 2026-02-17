% ============================================================================
% CONSTRAINT STORY: carrier_deployment_deterrence
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_carrier_deployment_deterrence, []).

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
 *   constraint_id: carrier_deployment_deterrence
 *   human_readable: US Carrier Strike Group Deployment as Regional Deterrent
 *   domain: geopolitical/military
 *
 * SUMMARY:
 *   This constraint models the deployment of a U.S. Navy carrier strike group
 *   to a contested region (e.g., the Middle East) as a deterrent against
 *   hostile actions by regional adversaries. The deployment functions as both a
 *   coordinating signal to allies and a coercive threat to opponents, creating
 *   a classic Tangled Rope structure where a genuine coordination function is
 *   coupled with asymmetric extraction (in the form of suppressed strategic options
 *   for adversaries).
 *
 * KEY AGENTS (by structural relationship):
 *   - civilian_populations_in_region: Powerless victims (powerless/trapped) - bear the cost of heightened tension and risk of conflict.
 *   - iran_hezbollah_axis: Primary target (organized/trapped) — bears the cost of suppressed options and heightened threat.
 *   - us_military_command: Primary beneficiary (institutional/arbitrage) — gains strategic options and deterrence capability.
 *   - israeli_government: Secondary beneficiary (institutional/constrained) — benefits from the security umbrella but cannot control the asset.
 *   - gulf_state_allies: Inter-institutional actor (institutional/constrained) - benefits from regional stability signal but dependent on US presence.
 *   - geopolitical_analyst: Analytical observer — sees the dual coordination/extraction structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(carrier_deployment_deterrence, 0.55). % Extraction is geopolitical: suppressed options, forced countermeasures.
domain_priors:suppression_score(carrier_deployment_deterrence, 0.75).   % Structural property (raw, unscaled). High, as it forecloses adversaries' actions.
domain_priors:theater_ratio(carrier_deployment_deterrence, 0.20).       % Piton detection (>= 0.70). Low, the asset is highly functional.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(carrier_deployment_deterrence, extractiveness, 0.55).
narrative_ontology:constraint_metric(carrier_deployment_deterrence, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(carrier_deployment_deterrence, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(carrier_deployment_deterrence, tangled_rope).
narrative_ontology:human_readable(carrier_deployment_deterrence, "US Carrier Strike Group Deployment as Regional Deterrent").

% --- Binary flags ---
domain_priors:requires_active_enforcement(carrier_deployment_deterrence). % A carrier group is the definition of active enforcement.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(carrier_deployment_deterrence, us_military_command).
narrative_ontology:constraint_beneficiary(carrier_deployment_deterrence, israeli_government).
narrative_ontology:constraint_beneficiary(carrier_deployment_deterrence, gulf_state_allies).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(carrier_deployment_deterrence, iran_hezbollah_axis).
narrative_ontology:constraint_victim(carrier_deployment_deterrence, civilian_populations_in_region).

% Gate requirements met for Tangled Rope:
%   - constraint_beneficiary/2 (coordination function)
%   - constraint_victim/2 (asymmetric extraction)
%   - requires_active_enforcement/1 (coercive element)

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE POWERLESS VICTIM (SNARE)
% For civilian populations in the region, the deployment is an overwhelming coercive
% threat that increases the risk of conflict, from which they cannot escape.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% χ ≈ 0.55 * 1.42 * 0.9 (regional) ≈ 0.70. This meets the snare threshold (χ ≥ 0.66).
constraint_indexing:constraint_classification(carrier_deployment_deterrence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY TARGET (SNARE)
% From the perspective of Iran/Hezbollah, the deployment is a pure coercive threat.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE PRIMARY BENEFICIARY (ROPE)
% For U.S. command, it's a tool of policy and coordination.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function for allies and the coercive
% extraction imposed on adversaries. This dual nature defines a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ ≈ 0.55 * 1.15 * 1.2 (global) ≈ 0.76. Fits Tangled Rope (0.40 ≤ χ ≤ 0.90, ε≥0.3, supp≥0.4).
constraint_indexing:constraint_classification(carrier_deployment_deterrence, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The constraint is experienced differently by various allied institutional actors
% based on their degree of control and dependency.

% PERSPECTIVE 5A: Secondary Beneficiary (ROPE, but higher χ)
% For Israel, the deployment is a coordination Rope, but they have 'constrained' exit
% as they rely on it but don't control it. This results in a higher directionality
% (d) than the primary beneficiary, reflecting their dependency.
% Beneficiary + constrained exit -> d ~ 0.25 -> f(d) ~ 0.14.
% χ ≈ 0.55 * 0.14 * 0.9 (regional) ≈ 0.07. Still a Rope, but the extraction is positive.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained), % Cannot recall the asset themselves
            spatial_scope(regional))).

% PERSPECTIVE 5B: Regional Allies (ROPE, similar to 5A)
% For Gulf States, the calculus is similar to Israel's: it's a stabilizing coordination
% mechanism (Rope) they benefit from but don't control, making their exit 'constrained'.
% The derived d and resulting χ are identical to Perspective 5A.
constraint_indexing:constraint_classification(carrier_deployment_deterrence, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(carrier_deployment_deterrence_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify gap between the target (Snare) and primary beneficiary (Rope).
    constraint_indexing:constraint_classification(carrier_deployment_deterrence, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(carrier_deployment_deterrence, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Target sees Snare, Beneficiary sees Rope. Gap confirmed.~n').

test(analytical_view_is_tangled_rope) :-
    % The analytical observer must see the full tangled structure.
    constraint_indexing:constraint_classification(carrier_deployment_deterrence, tangled_rope, context(agent_power(analytical), _, _, _)).

test(inter_institutional_exit_difference) :-
    % Verify that different exit options for institutional actors are modeled.
    constraint_classification(carrier_deployment_deterrence, _, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_classification(carrier_deployment_deterrence, _, context(agent_power(institutional), _, exit_options(constrained), _)).

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(carrier_deployment_deterrence, _),
    narrative_ontology:constraint_victim(carrier_deployment_deterrence, _),
    domain_priors:requires_active_enforcement(carrier_deployment_deterrence).

:- end_tests(carrier_deployment_deterrence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (ε=0.55) reflects the significant geopolitical cost imposed on
 *   adversaries—it's not monetary, but it severely constrains their strategic calculus.
 *   Suppression (0.75) is high because the explicit purpose is to make certain actions
 *   prohibitively risky, thereby suppressing them. Theater (0.20) is low because while
 *   there is a signaling component, the asset is primarily functional and credible.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the US/allies (beneficiaries), it's a coordination tool
 *   (Rope) that enhances security and strategic options. For Iran/Hezbollah (victims),
 *   it is a direct coercive threat that limits their sovereignty and options (Snare).
 *   This Snare classification is also shared by powerless civilian populations who bear
 *   the risk of conflict. The analytical view reconciles these by identifying the
 *   structure as a Tangled Rope, acknowledging both the valid coordination function
 *   and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'us_military_command' has arbitrage exit, as it controls the asset,
 *     leading to a very low `d` and negative effective extraction (χ). 'israeli_government'
 *     and 'gulf_state_allies' benefit but have constrained exit, reflecting their
 *     dependency. This correctly yields a higher `d` and a small positive χ, still a Rope but
 *     quantifiably different.
 *   - Victims: 'iran_hezbollah_axis' and 'civilian_populations_in_region' are the targets.
 *     With trapped/constrained exit options, the engine derives a very high `d` (≈0.95),
 *     maximizing the effective extraction and producing a Snare classification from their perspectives.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story highlights the system's ability to model nuanced institutional relationships.
 *   Both the US and Israel are institutional beneficiaries, but their different `exit_options`
 *   (`arbitrage` vs. `constrained`) lead to different derived directionality values (`d`).
 *   This captures the structural reality: the US wields the tool, while Israel is a
 *   dependent beneficiary of the security it provides. The classification remains Rope for
 *   both, but the underlying perspectival gap in χ is now measurable.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors. It does not dismiss the
 *   deployment as pure aggression (a Snare from all views), because it recognizes the
 *   genuine coordination and stabilization function for allies. Conversely, it does not
 *   accept the purely defensive/coordinating narrative (a Rope from all views), because
 *   it quantifies the severe coercive extraction imposed on its targets. The Tangled Rope
 *   classification is essential for capturing this dual-function reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_carrier_deployment_deterrence,
    'Is the carrier deployment a genuine deterrent (effective Tangled Rope) or an escalatory provocation that will be ignored or trigger a wider conflict (failed function)?',
    'Empirical observation of actions by Iran and its proxies during the deployment period. Absence of escalation supports the deterrent function; an attack would negate it.',
    'If it successfully deters, the Tangled Rope classification is confirmed. If it fails and provokes escalation, its function was misjudged and it acted as a Snare on regional stability itself.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing. Interval represents a typical 10-month deployment.
narrative_ontology:interval(carrier_deployment_deterrence, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Modeling a 10-month deployment cycle. Required as base_extractiveness > 0.46.
% Theater ratio might slightly increase as the deployment becomes a static part of the
% landscape, shifting from acute threat to persistent signaling.
% Extraction remains constant as the geopolitical constraint is unwavering.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(carrier_tr_t0, carrier_deployment_deterrence, theater_ratio, 0, 0.15).
narrative_ontology:measurement(carrier_tr_t5, carrier_deployment_deterrence, theater_ratio, 5, 0.20).
narrative_ontology:measurement(carrier_tr_t10, carrier_deployment_deterrence, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(carrier_ex_t0, carrier_deployment_deterrence, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(carrier_ex_t5, carrier_deployment_deterrence, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(carrier_ex_t10, carrier_deployment_deterrence, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The deployment is a physical enforcement mechanism.
narrative_ontology:coordination_type(carrier_deployment_deterrence, enforcement_mechanism).

% Network relationships (structural influence edges)
% The deployment is a direct response to, and an attempt to constrain,
% a regional conflict's potential to escalate.
narrative_ontology:affects_constraint(carrier_deployment_deterrence, regional_conflict_escalation_risk).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The standard derivation from
% beneficiary/victim declarations and exit options correctly models the
% structural relationships between the agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */