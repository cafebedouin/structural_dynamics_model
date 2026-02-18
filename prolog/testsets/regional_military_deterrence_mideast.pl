% ============================================================================
% CONSTRAINT STORY: regional_military_deterrence_mideast
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-18
% ============================================================================

:- module(constraint_regional_military_deterrence_mideast, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: regional_military_deterrence_mideast
 *   human_readable: US/Israeli Military Deterrence Posture against Iran
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The deployment of significant US and Israeli naval and air assets in the
 *   Middle East, ostensibly to deter aggression from Iran and its regional
 *   proxies. This posture functions as a constraint on Iran's strategic
 *   options by creating a credible threat of overwhelming retaliatory force.
 *   While framed as a stabilizing or coordinating measure, it imposes immense
 *   costs on the target and carries catastrophic risks for regional civilians.
 *
 * KEY AGENTS (by structural relationship):
 *   - Iranian State & Proxies: Primary target (organized/constrained) — bears strategic and economic extraction.
 *   - Regional Civilians: Secondary target (powerless/trapped) — bears the ultimate risk of kinetic conflict.
 *   - US/Israeli Security State: Primary beneficiary (institutional/arbitrage) — benefits from regional power projection and strategic dominance.
 *   - Analytical Observer: Sees the full structure of coercive coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regional_military_deterrence_mideast, 0.65).
domain_priors:suppression_score(regional_military_deterrence_mideast, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(regional_military_deterrence_mideast, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regional_military_deterrence_mideast, extractiveness, 0.65).
narrative_ontology:constraint_metric(regional_military_deterrence_mideast, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(regional_military_deterrence_mideast, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint type.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(regional_military_deterrence_mideast, tangled_rope).
narrative_ontology:human_readable(regional_military_deterrence_mideast, "US/Israeli Military Deterrence Posture against Iran").
narrative_ontology:topic_domain(regional_military_deterrence_mideast, "geopolitical").

% --- Binary flags ---
domain_priors:requires_active_enforcement(regional_military_deterrence_mideast). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(regional_military_deterrence_mideast, us_israeli_security_state).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(regional_military_deterrence_mideast, iranian_state_and_proxies).
narrative_ontology:constraint_victim(regional_military_deterrence_mideast, regional_civilians).

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

% PERSPECTIVE 1A: THE PRIMARY STATE TARGET (SNARE)
% Iran and its proxies see a coercive trap limiting their sovereignty.
% Engine derives d from: victim membership + constrained exit -> high d -> high chi
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 1B: THE CIVILIAN TARGET (SNARE)
% Regional civilians experience this as an existential threat with no escape.
% Engine derives d from: victim membership + trapped exit -> d ~0.95 -> max chi
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The US/Israeli security state sees this as a tool for managing regional stability.
% Engine derives d from: beneficiary membership + arbitrage exit -> d ~0.05 -> negative chi
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view recognizes both the coordinating intent and the immense,
% asymmetric extraction and coercion, classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(regional_military_deterrence_mideast, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regional_military_deterrence_mideast_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify the core perspectival gap: Snare for the target, Rope for the beneficiary.
    constraint_indexing:constraint_classification(regional_military_deterrence_mideast, snare, context(agent_power(organized), _, _, _)),
    constraint_indexing:constraint_classification(regional_military_deterrence_mideast, rope, context(agent_power(institutional), _, _, _)).

test(analytical_synthesis_is_tangled_rope) :-
    % Verify the analytical perspective correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(regional_military_deterrence_mideast, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_for_tangled_rope) :-
    % Tangled Rope requires high base extraction and suppression.
    domain_priors:base_extractiveness(regional_military_deterrence_mideast, E),
    domain_priors:suppression_score(regional_military_deterrence_mideast, S),
    E >= 0.30,
    S >= 0.40.

:- end_tests(regional_military_deterrence_mideast_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.65): High. This represents the immense cost imposed on Iran (economic sanctions enforced by military threat, strategic paralysis) and the existential risk imposed on regional populations. The "benefit" of regional dominance for the US/Israel is extracted from the suppressed autonomy and safety of others.
 *   - Suppression (s=0.80): Very high. The military posture is explicitly designed to foreclose Iran's options. Alternatives like direct military challenge are made prohibitively costly, forcing them into asymmetric or diplomatic channels under extreme duress.
 *   - Theater Ratio (θ=0.20): Low. While there is significant public messaging, the constraint's power comes from real, deployed military hardware. The carriers and jets are functional, not merely theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound and defines the constraint. For the US/Israeli security state (institutional beneficiary), the military presence is a "Rope" for coordinating regional behavior to ensure stability and protect its interests. The negative effective extraction (χ) from their perspective reflects that they see it as a net good that costs resources but produces security.
 *   For Iran and regional civilians (organized/powerless victims), it is a "Snare." The "coordination" is perceived as a pretext for coercion, containment, and strategic strangulation. The high positive χ reflects the immense costs and risks they bear.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived directly from the structural roles. The 'us_israeli_security_state' is the declared beneficiary, having arbitrage exit options (they can redeploy forces), leading to a very low directionality score `d`. 'iranian_state_and_proxies' and 'regional_civilians' are victims with constrained or trapped exit options, leading to very high `d` values. This accurately models the flow of power and cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the hybrid nature of the constraint, preventing mandatrophy. A purely extraction-focused analysis might miss the genuine (though coercive) coordinating function and label it a simple Snare. A purely coordination-focused analysis would miss the immense asymmetric costs and mislabel it a Rope. The Tangled Rope classification captures the reality that it is a system of *coordination through the threat of extraction*.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_regional_military_deterrence_mideast,
    'Is the primary strategic goal deterrence (coordination) or creating a pretext for conflict (extraction)?',
    'Declassification of strategic planning documents from US/Israeli military and political leadership.',
    'If deterrence is primary, it remains a (highly coercive) Tangled Rope. If pretext is primary, the coordination function is theater, and it is structurally a pure Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(regional_military_deterrence_mideast, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has intensified following the US withdrawal from the JCPOA.
% This data models the accumulation of extraction over the last decade.
% Required because base_extractiveness (0.65) > 0.46.

% Theater ratio over time (stable/low):
narrative_ontology:measurement(rmdm_tr_t0, regional_military_deterrence_mideast, theater_ratio, 0, 0.30).
narrative_ontology:measurement(rmdm_tr_t5, regional_military_deterrence_mideast, theater_ratio, 5, 0.25).
narrative_ontology:measurement(rmdm_tr_t10, regional_military_deterrence_mideast, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(rmdm_ex_t0, regional_military_deterrence_mideast, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(rmdm_ex_t5, regional_military_deterrence_mideast, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(rmdm_ex_t10, regional_military_deterrence_mideast, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The constraint's function is to enforce a specific
% set of behaviors through the threat of force.
narrative_ontology:coordination_type(regional_military_deterrence_mideast, enforcement_mechanism).

% Network relationships: This military posture is a key driver of other
% geopolitical and economic constraints.
narrative_ontology:affects_constraint(regional_military_deterrence_mideast, global_oil_prices).
narrative_ontology:affects_constraint(regional_military_deterrence_mideast, iranian_nuclear_program_limits).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation
% based on declared beneficiaries/victims and their exit options accurately
% captures the structural power dynamics of the scenario.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */