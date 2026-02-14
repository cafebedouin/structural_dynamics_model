% ============================================================================
% CONSTRAINT STORY: streaming_bundling_mandate
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_streaming_bundling_mandate, []).

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
 *   constraint_id: streaming_bundling_mandate
 *   human_readable: Mandatory Streaming Bundling
 *   domain: economic
 *
 * SUMMARY:
 *   This constraint describes a hypothetical regulatory mandate forcing streaming
 *   services to offer their content only as part of a bundled package,
 *   prohibiting standalone subscriptions. This aims to support struggling
 *   content creators but potentially harms consumers and smaller streaming platforms.
 *
 * KEY AGENTS (by structural relationship):
 *   - Consumers: Primary target (powerless/trapped) — bears extraction
 *   - Established Streaming Giants: Primary beneficiary (institutional/arbitrage) — benefits from constraint
 *   - Independent Content Creators: Secondary beneficiary (moderate/constrained)
 *   - Regulators: Intermediary (institutional/constrained)
 *   - Analytical observer — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(streaming_bundling_mandate, 0.55).
domain_priors:suppression_score(streaming_bundling_mandate, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(streaming_bundling_mandate, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(streaming_bundling_mandate, extractiveness, 0.55).
narrative_ontology:constraint_metric(streaming_bundling_mandate, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(streaming_bundling_mandate, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(streaming_bundling_mandate, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(streaming_bundling_mandate). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(streaming_bundling_mandate, established_streaming_giants).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(streaming_bundling_mandate, consumers).
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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE/MOUNTAIN)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%
% NOTE: Per "Dynamic Coalition" extension, this agent's power may be
% upgraded to 'organized' if the constraint is a snare with a critical
% mass of victims, potentially changing the classification.
%
% UNIFORM-TYPE EXCEPTION: For natural law constraints (mountain-only) or pure
% coordination constraints (rope-only), perspectives 1 and 2 may use any power
% atoms — the classification is the same from all perspectives. Include at
% least 2-3 perspectives to demonstrate the invariance.
constraint_indexing:constraint_classification(streaming_bundling_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(streaming_bundling_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(streaming_bundling_mandate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES (declare when applicable) ---
% When a constraint operates between institutional actors with different
% structural relationships, declare separate perspectives for each.
% The engine differentiates via directionality: different exit_options
% produce different d values even for the same power atom.
%
% Example — Regulatory capture:
%
% Perspective 4A: Regulators (institutional, constrained exit) - They are bound by political pressures and lobbying
constraint_indexing:constraint_classification(streaming_bundling_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(streaming_bundling_mandate_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(streaming_bundling_mandate, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(streaming_bundling_mandate, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(streaming_bundling_mandate, ExtMetricName, E),
    E >= 0.46. % High-extraction Tangled Rope/Snare.

:- end_tests(streaming_bundling_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The scores are based on the assumption that consumers are negatively impacted
 *   by being forced into bundles, while larger streaming services benefit from
 *   increased stability and market share.  Suppression is high due to the limited
 *   availability of standalone options. Theater ratio is moderate because it's
 *   assumed that while there might be official justifications, the underlying motive
 *   is to consolidate the market.
 *
 * PERSPECTIVAL GAP:
 *   Consumers see this as a Snare because they are forced to pay for content they
 *   don't necessarily want.  Established streaming giants see this as a Rope
 *   because it provides market stability and reduces competition.
 *
 * DIRECTIONALITY LOGIC:
 *   Consumers are the victims as they have reduced choice and potentially higher
 *   costs. Established streaming giants are the beneficiaries because they have
 *   a guaranteed revenue stream and can crowd out smaller competitors. The regulators
 *   are in a constrained position.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *  The regulators are balancing the interests of large streaming companies, smaller content creators, and consumers.
 *  They may be swayed by lobbying efforts from large companies. Their exit option is constrained by political pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling this as pure extraction by acknowledging
 *   the potential coordination function of supporting independent content creators,
 *   but highlighting the asymmetric extraction from consumers.  The high suppression score
 *   distinguishes it from a genuinely beneficial coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_streaming_bundling_mandate,
    'To what extent does mandatory bundling stifle innovation in the streaming market?',
    'Empirical analysis of content diversity and new platform entry post-mandate.',
    'If innovation is significantly stifled, the classification shifts towards a pure Snare. If innovation remains robust, the classification remains Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(streaming_bundling_mandate, 0, 10).

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
narrative_ontology:measurement(streaming_bundling_mandate_tr_t0, streaming_bundling_mandate, theater_ratio, 0, 0.20).
narrative_ontology:measurement(streaming_bundling_mandate_tr_t5, streaming_bundling_mandate, theater_ratio, 5, 0.30).
narrative_ontology:measurement(streaming_bundling_mandate_tr_t10, streaming_bundling_mandate, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(streaming_bundling_mandate_ex_t0, streaming_bundling_mandate, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(streaming_bundling_mandate_ex_t5, streaming_bundling_mandate, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(streaming_bundling_mandate_ex_t10, streaming_bundling_mandate, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(streaming_bundling_mandate, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override([id], [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint([id], [other_constraint_id]).

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
% constraint_indexing:directionality_override([id], institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */