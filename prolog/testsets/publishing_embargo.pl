% ============================================================================
% CONSTRAINT STORY: publishing_embargo
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_publishing_embargo, []).

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
 *   constraint_id: publishing_embargo
 *   human_readable: Academic Publishing Embargo
 *   domain: social
 *
 * SUMMARY:
 *   Academic publishing embargoes restrict researchers from publicly sharing
 *   their findings before the official publication date in a peer-reviewed
 *   journal. This constraint aims to ensure quality control and establish priority
 *   of discovery, but also delays the dissemination of potentially valuable knowledge.
 *
 * KEY AGENTS (by structural relationship):
 *   - Early-career researchers: Primary target (powerless/trapped) — bears extraction (delayed dissemination, career risk)
 *   - Journal publishers: Primary beneficiary (institutional/arbitrage) — benefits from embargo (exclusivity, revenue)
 *   - Established researchers: Secondary target (moderate/constrained) — constrained but with more exit options
 *   - Funding agencies: Secondary actor (powerful/constrained) — incentivizes embargo compliance for career advancement metrics
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(publishing_embargo, 0.35).
domain_priors:suppression_score(publishing_embargo, 0.50).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(publishing_embargo, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(publishing_embargo, extractiveness, 0.35).
narrative_ontology:constraint_metric(publishing_embargo, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(publishing_embargo, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(publishing_embargo, tangled_rope).
narrative_ontology:human_readable(publishing_embargo, "Academic Publishing Embargo").

% --- Binary flags ---
domain_priors:requires_active_enforcement(publishing_embargo).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(publishing_embargo, journal_publishers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(publishing_embargo, researchers).
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

% PERSPECTIVE 1: THE PRIMARY TARGET (TANGLED ROPE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% With ε=0.35, σ=1.2, χ ≈ 0.35 * 1.42 * 1.2 ≈ 0.596. This is in the Tangled Rope
% range (0.40-0.90), not high enough for Snare (>=0.66).
constraint_indexing:constraint_classification(publishing_embargo, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(publishing_embargo, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(publishing_embargo, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ESTABLISHED RESEARCHER (TANGLED ROPE)
% A secondary target with more exit options than the powerless agent.
% Engine derives d from: victim membership + constrained exit -> d ~0.85 -> f(d) ~1.15
constraint_indexing:constraint_classification(publishing_embargo, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(publishing_embargo_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(publishing_embargo, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(publishing_embargo, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(publishing_embargo, ExtMetricName, E),
    E >= 0.30, % Tangled Rope requires ε >= 0.30
    E < 0.90.

:- end_tests(publishing_embargo_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set to 0.35 because the embargo delays information
 *   but doesn't completely prevent its dissemination (e.g., via pre-prints). The
 *   suppression score of 0.50 reflects that while alternative publishing options
 *   exist, powerful career incentives enforce compliance with the embargo system.
 *   The low theater ratio reflects that the primary function (curation, priority)
 *   is genuinely maintained, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   Early-career researchers (powerless, trapped) experience the embargo as a
 *   Tangled Rope, a necessary system for career progression that nonetheless
 *   extracts from them by delaying their work's impact. Journals (institutional,
 *   arbitrage) perceive it as a pure Rope, a coordination mechanism that ensures
 *   quality control and establishes a stable business model. The Analytical
 *   Observer agrees with the target's classification, seeing a system with both
 *   a genuine coordination function and asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Journal publishers are the primary beneficiaries, gaining exclusivity which
 *   drives subscriptions and prestige. Researchers, particularly those early in
 *   their careers, are the primary victims, bearing the cost of delayed
 *   dissemination and the risk of being "scooped" during the long review process.
 *
 * INTER-INSTUTIONAL DYNAMICS:
 *   Journal publishers and researchers operate within a system governed by
 *   incentives set by funding agencies and universities. These institutions
 *   implicitly endorse the embargo by prioritizing publications in high-impact
 *   factor journals for career advancement, creating a high-suppression environment.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling the embargo as pure
 *   extraction (Snare). While it extracts from researchers, it also serves a
 *   coordination function (quality control, establishing priority) that benefits
 *   the scientific community. The key insight is that both functions coexist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_publishing_embargo,
    'Is the quality control function of the embargo system still primary, or has it become secondary to the revenue-generation function for publishers?',
    'Analysis of publisher profit margins vs. investment in peer review infrastructure; surveys of researcher perceptions on review quality over time.',
    'If revenue is primary, the constraint is drifting towards a Snare. If quality control is primary, it remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(publishing_embargo, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Not strictly required as base_extractiveness < 0.46, but included for
% modeling the system's evolution.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(publishing_embargo_tr_t0, publishing_embargo, theater_ratio, 0, 0.10).
narrative_ontology:measurement(publishing_embargo_tr_t5, publishing_embargo, theater_ratio, 5, 0.15).
narrative_ontology:measurement(publishing_embargo_tr_t10, publishing_embargo, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(publishing_embargo_ex_t0, publishing_embargo, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(publishing_embargo_ex_t5, publishing_embargo, base_extractiveness, 5, 0.30).
narrative_ontology:measurement(publishing_embargo_ex_t10, publishing_embargo, base_extractiveness, 10, 0.35).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(publishing_embargo, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% directionality for each agent group.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */