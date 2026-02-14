% ============================================================================
% CONSTRAINT STORY: cn_tech_decoupling_security_software
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_cn_tech_decoupling_security_software, []).

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
 *   constraint_id: cn_tech_decoupling_security_software
 *   human_readable: "Mandate for Chinese SOEs to replace US security software"
 *   domain: technological
 *
 * SUMMARY:
 *   A state mandate requires Chinese state-owned enterprises (SOEs) to replace
 *   US-origin network security software with domestic alternatives. The policy
 *   is framed as a national security imperative but also functions as a
 *   protectionist measure to foster a domestic software industry, creating a
 *   captive market for local vendors at the expense of SOE autonomy and the
 *   market access of foreign firms.
 *
 * KEY AGENTS (by structural relationship):
 *   - Individual SOE Employees: Primary target (powerless/trapped) - must use mandated software without choice.
 *   - Chinese State-Owned Enterprises (SOEs): Secondary target (organized/trapped) — bear migration costs and loss of choice.
 *   - Domestic Chinese Software Vendors: Primary beneficiary (powerful/arbitrage) — gain a protected, state-created market.
 *   - US Security Software Vendors: Secondary target/victim (powerful/constrained) — lose market access due to the mandate.
 *   - Chinese State Security Apparatus: Institutional beneficiary (institutional/arbitrage) — achieves technological sovereignty goals.
 *   - Analytical Observer: Sees the full structure of national security coordination and economic extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cn_tech_decoupling_security_software, 0.55).
domain_priors:suppression_score(cn_tech_decoupling_security_software, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(cn_tech_decoupling_security_software, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cn_tech_decoupling_security_software, extractiveness, 0.55).
narrative_ontology:constraint_metric(cn_tech_decoupling_security_software, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(cn_tech_decoupling_security_software, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(cn_tech_decoupling_security_software, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(cn_tech_decoupling_security_software). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(cn_tech_decoupling_security_software, domestic_chinese_software_vendors).
narrative_ontology:constraint_beneficiary(cn_tech_decoupling_security_software, chinese_state_security_apparatus).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(cn_tech_decoupling_security_software, chinese_state_owned_enterprises).
narrative_ontology:constraint_victim(cn_tech_decoupling_security_software, us_security_software_vendors).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET

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

% PERSPECTIVE 1: THE INDIVIDUAL SOE EMPLOYEE (POWERLESS TARGET)
% Individual employees within the SOEs have no say in the software they must use.
% They are trapped by their employment and the state mandate.
% Engine derives d from canonical powerless fallback: d ≈ 1.0 -> f(d) ≈ 1.42
%   χ = 0.55 * 1.42 * 1.0 = 0.781. High χ + high suppression -> Snare.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY TARGET (CHINESE SOEs)
% As victims with no ability to exit the national legal framework, they
% experience the mandate as pure coercion.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42
%   χ = 0.55 * 1.42 * 1.0 = 0.781. High χ + high suppression -> Snare.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PRIMARY BENEFICIARY (DOMESTIC VENDORS)
% As beneficiaries with arbitrage, they see a pure coordination mechanism
% that creates a profitable, protected market for them.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12
%   χ = 0.55 * -0.12 * 1.0 = -0.066. Negative χ -> Rope.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The analytical view sees both the coordination function (beneficiaries exist)
% and the severe asymmetric extraction, classifying it as a Tangled Rope.
% Engine derives d ≈ 0.73 → f(d) ≈ 1.15 for analytical perspective.
%   χ = 0.55 * 1.15 * 1.2 (global scope) = 0.759.
%   With ε=0.55, s=0.85, and χ > 0.40, this is a canonical Tangled Rope.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 5A: The State Security Apparatus (Institutional Beneficiary)
% Views the mandate as a successful coordination tool for national security.
% Engine derives d from: beneficiary + institutional + arbitrage -> d ≈ 0.05 -> Rope
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5B: Chinese SOEs (Institutional Victim)
% SOEs are themselves powerful state institutions, but they are constrained by
% the central government's directive. They experience this as extraction.
% Engine derives d from: victim + institutional + constrained -> higher d than beneficiary -> Snare
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cn_tech_decoupling_security_software_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify gap between powerless target and institutional beneficiary.
    constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(perspectival_gap_inter_institutional) :-
    % Verify gap between state apparatus (beneficiary) and SOEs (victim).
    constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, snare, context(agent_power(institutional), _, exit_options(constrained), _)).

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(cn_tech_decoupling_security_software, _),
    narrative_ontology:constraint_victim(cn_tech_decoupling_security_software, _),
    domain_priors:requires_active_enforcement(cn_tech_decoupling_security_software).

:- end_tests(cn_tech_decoupling_security_software_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High. This reflects the significant economic value transfer. SOEs face migration costs, potential feature loss, and retraining overhead. US vendors lose a major market. Domestic vendors receive state-guaranteed revenue streams (rents).
 *   - Suppression Score (s=0.85): Very High. The constraint is a direct mandate that explicitly forbids the use of a class of alternatives (US software). Choice is maximally suppressed.
 *   - Theater Ratio (τ=0.10): Low. This is a functional policy with clear, enforced objectives, not a performative gesture.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the beneficiary domestic firms, it is a pure Rope: a coordination device that creates a lucrative, protected market. For the targeted SOEs and their employees, it is a Snare: a costly, coercive mandate that removes their autonomy to select the best tools for their needs. This difference is driven entirely by their structural relationship to the constraint, which the directionality 'd' parameter captures.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `domestic_chinese_software_vendors` and the `chinese_state_security_apparatus`. They gain market share and strategic control, respectively. Their low derived `d` values result in low or negative effective extraction (χ), classifying the constraint as a Rope.
 *   - Victims: `chinese_state_owned_enterprises` and `us_security_software_vendors`. They bear the costs: SOEs through forced migration and reduced choice, US firms through lost market access. Their high derived `d` values result in high χ, classifying the constraint as a Snare.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story highlights a classic inter-institutional conflict within a single state. Both the state security apparatus and the SOEs are institutional actors. However, their exit options and relationship to the constraint differ dramatically. The security apparatus has 'arbitrage' exit (it can leverage the policy for strategic gain), while the SOEs have 'constrained' exit (they must comply). The engine correctly models this by assigning different `d` values, leading to the security apparatus seeing a Rope and the SOEs seeing a Snare, despite both being 'institutional'.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a canonical Tangled Rope. A naive analysis might label it a pure Snare (focusing on the SOEs) or a pure Rope (focusing on the national security claim). The Deferential Realism framework correctly identifies its dual nature by indexing to different agents. The analytical perspective integrates these views, recognizing that a genuine coordination function (national security) coexists with severe asymmetric extraction (protectionism), which defines a Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cn_tech_decoupling,
    'Is the primary driver of this mandate a legitimate, verifiable increase in national security, or is it primarily a mechanism for economic protectionism and rent-seeking by domestic firms?',
    'A long-term (5+ year) comparative audit of cyber incidents in SOEs using domestic software vs. a control group, combined with analysis of domestic vendor profit margins and market concentration.',
    'If primarily security-driven, the coordination function is strong, validating the Tangled Rope classification. If primarily protectionism, the coordination claim is weak, and the constraint is structurally closer to a pure Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cn_tech_decoupling_security_software, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This policy represents an intensification over a decade (e.g., 2016-2026).
% The measurements model a shift from encouragement to a hard mandate.
% Base extractiveness is high (>0.46), so temporal data is required.

% Theater ratio over time (consistently low and functional):
narrative_ontology:measurement(cn_tech_decoupling_tr_t0, cn_tech_decoupling_security_software, theater_ratio, 0, 0.20).
narrative_ontology:measurement(cn_tech_decoupling_tr_t5, cn_tech_decoupling_security_software, theater_ratio, 5, 0.15).
narrative_ontology:measurement(cn_tech_decoupling_tr_t10, cn_tech_decoupling_security_software, theater_ratio, 10, 0.10).

% Extraction over time (policy hardens, extraction increases):
narrative_ontology:measurement(cn_tech_decoupling_ex_t0, cn_tech_decoupling_security_software, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(cn_tech_decoupling_ex_t5, cn_tech_decoupling_security_software, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(cn_tech_decoupling_ex_t10, cn_tech_decoupling_security_software, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The mandate is primarily an enforcement tool to achieve a new standard.
narrative_ontology:coordination_type(cn_tech_decoupling_security_software, enforcement_mechanism).

% Network relationships: This software mandate is part of a larger strategic push
% for technological independence, which also affects hardware supply chains.
narrative_ontology:affects_constraint(cn_tech_decoupling_security_software, global_semiconductor_supply_chain).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The structural derivation from
% beneficiary/victim declarations and exit options accurately captures the
% power dynamics and perspectival gaps between the agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */