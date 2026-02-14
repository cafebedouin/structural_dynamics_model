% ============================================================================
% CONSTRAINT STORY: nyc_metrocard_art_licensing
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_nyc_metrocard_art_licensing, []).

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
 *   constraint_id: nyc_metrocard_art_licensing
 *   human_readable: NYC MetroCard Art Licensing Agreement
 *   domain: economic
 *
 * SUMMARY:
 *   The licensing agreement between the MTA and artists for using their artwork on MetroCards allows the MTA to profit from the artwork's popularity while potentially limiting the artists' control and compensation. This represents a potential extraction of value from artists who may have limited bargaining power. The art elevates the perceived value of the metrocard and is an important draw.
 *
 * KEY AGENTS (by structural relationship):
 *   - MTA: Primary beneficiary (institutional/arbitrage) — benefits from revenue and image enhancement
 *   - Emerging Artists: Primary target (powerless/trapped) — bears extraction through limited control and potentially low compensation due to high prestige of the opportunity.
 *   - Established Artists: Secondary target (moderate/mobile) - experiences extraction but has more bargaining power and exit options.
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nyc_metrocard_art_licensing, 0.48).
domain_priors:suppression_score(nyc_metrocard_art_licensing, 0.45).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(nyc_metrocard_art_licensing, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nyc_metrocard_art_licensing, extractiveness, 0.48).
narrative_ontology:constraint_metric(nyc_metrocard_art_licensing, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(nyc_metrocard_art_licensing, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(nyc_metrocard_art_licensing, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(nyc_metrocard_art_licensing, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(nyc_metrocard_art_licensing, tangled_rope).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(nyc_metrocard_art_licensing).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(nyc_metrocard_art_licensing). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(nyc_metrocard_art_licensing).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(nyc_metrocard_art_licensing, mta).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(nyc_metrocard_art_licensing, artists).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ESTABLISHED ARTIST (TANGLED ROPE)
% An artist with more bargaining power and other opportunities (mobile exit)
% still experiences extraction but sees the coordination benefit more clearly.
constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nyc_metrocard_art_licensing_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nyc_metrocard_art_licensing, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nyc_metrocard_art_licensing, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(nyc_metrocard_art_licensing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set to 0.48, reflecting that while artists receive some compensation and exposure, the MTA likely benefits more significantly from the arrangement. Suppression is set to 0.45 to meet the Tangled Rope threshold, suggesting that while alternative avenues for artists exist, the MTA's platform is a unique and powerful gatekeeper, limiting practical alternatives for this scale of public exposure. The theater ratio is low because the licensing primarily serves a functional purpose (generating revenue and providing art).
 *
 * PERSPECTIVAL GAP:
 *   Emerging artists (powerless/trapped), for whom this is a career-making opportunity with no real alternative, perceive the arrangement as a Snare, extracting value and control from them. The MTA (institutional/arbitrage), which can choose from many artists, views it as a Rope, facilitating a beneficial coordination. Established artists (moderate/mobile) and analytical observers see both sides, classifying it as a Tangled Rope that combines genuine public art coordination with asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The MTA benefits through increased revenue and improved public image. The artists bear the costs of limited control over their artwork and potentially lower-than-market compensation, an asymmetry captured by the beneficiary/victim declarations.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   N/A - No clear inter-institutional dynamic other than the contractual agreement.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification acknowledges both the coordination function (providing art for public benefit) and the asymmetric extraction (MTA benefits more than the artists). It prevents mislabeling this as pure extraction (Snare) by recognizing the coordination function and prevents mislabeling as pure coordination (Rope) by acknowledging the asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_nyc_metrocard_art_licensing,
    'What is the true bargaining power of the artists relative to the MTA?',
    'Analyzing the contract terms and artist testimonies.',
    'If artists have significant bargaining power, the classification shifts towards a Rope. If they have little power, it remains a Tangled Rope or a Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(nyc_metrocard_art_licensing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(nyc_metrocard_art_licensing_tr_t0, nyc_metrocard_art_licensing, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nyc_metrocard_art_licensing_tr_t5, nyc_metrocard_art_licensing, theater_ratio, 5, 0.20).
narrative_ontology:measurement(nyc_metrocard_art_licensing_tr_t10, nyc_metrocard_art_licensing, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(nyc_metrocard_art_licensing_ex_t0, nyc_metrocard_art_licensing, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(nyc_metrocard_art_licensing_ex_t5, nyc_metrocard_art_licensing, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(nyc_metrocard_art_licensing_ex_t10, nyc_metrocard_art_licensing, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(nyc_metrocard_art_licensing, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(nyc_metrocard_art_licensing, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(nyc_metrocard_art_licensing, [other_constraint_id]).

% --- Network Decomposition (Constraint Families) ---
% When a natural-language label covers multiple constraints with different ε
% values, each gets its own file. Link family members with affects_constraint:
%
% DUAL FORMULATION NOTE:
% This constraint is one of [N] stories decomposed from [colloquial label].
% Decomposed because ε differs across observables (ε-invariance principle).
% Related stories:
%   - [sibling_constraint_1] (ε=[value], [Type])
%   - [sibling_constraint_2] (ε=[value], [Type])
%
% narrative_ontology:affects_constraint(nyc_metrocard_art_licensing, [sibling_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Use ONLY when the automatic derivation (beneficiary/victim + exit → d)
% would produce an inaccurate directionality value. The derivation chain
% priority is: override > structural > canonical fallback.
%
% Format: directionality_override(ConstraintID, PowerAtom, D_Value)
%   D_Value in [0.0, 1.0]: 0.0 = full beneficiary, 1.0 = full target
%
% Common override scenarios:
%   - Regulatory capture: institution that appears to benefit but is
%     actually partly captured → override d upward (0.25-0.40)
%   - Indirect beneficiary: agent in victim group who actually benefits
%     through secondary effects → override d downward
%   - Asymmetric institutional: two institutional actors that the
%     derivation can't distinguish → override to differentiate
%
% Example (uncomment if needed):
% constraint_indexing:directionality_override(nyc_metrocard_art_licensing, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */