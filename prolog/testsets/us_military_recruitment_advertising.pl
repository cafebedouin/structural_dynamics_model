% ============================================================================
% CONSTRAINT STORY: us_military_recruitment_advertising
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_us_military_recruitment_advertising, []).

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
 *   constraint_id: us_military_recruitment_advertising
 *   human_readable: US Military Recruitment Advertising
 *   domain: political
 *
 * SUMMARY:
 *   US military recruitment advertising presents a curated and often idealized image of military life to prospective recruits, particularly young adults. This constraint influences young adults' decisions to enlist, potentially obscuring the risks and realities of military service, while serving the interests of the military-industrial complex. The Keezy Young comic "Hello Sunshine" depicts the power of targeted ads.
 *
 * KEY AGENTS (by structural relationship):
 *   - Young Adults: Primary target (powerless/constrained) — bears extraction (risk of injury, death, psychological trauma)
 *   - US Military: Primary beneficiary (institutional/organized) — benefits from increased recruitment numbers
 *   - Defense Contractors: Secondary beneficiary (institutional/arbitrage) — benefits from military spending fueled by recruitment
 *   - Analytical Observer: Sees the full structure (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_military_recruitment_advertising, 0.55).
domain_priors:suppression_score(us_military_recruitment_advertising, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_military_recruitment_advertising, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_military_recruitment_advertising, extractiveness, 0.55).
narrative_ontology:constraint_metric(us_military_recruitment_advertising, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(us_military_recruitment_advertising, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_military_recruitment_advertising, tangled_rope).
narrative_ontology:human_readable(us_military_recruitment_advertising, "US Military Recruitment Advertising").
narrative_ontology:topic_domain(us_military_recruitment_advertising, "political").

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_military_recruitment_advertising). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_military_recruitment_advertising, us_military).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_military_recruitment_advertising, young_adults).

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
%   victim membership + constrained exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%
% NOTE: Per "Dynamic Coalition" extension, this agent's power may be
% upgraded to 'organized' if the constraint is a snare with a critical
% mass of victims, potentially changing the classification.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + organized power + constrained exit → d ≈ 0.40 → f(d) ≈ 0.40 → low/negative χ
constraint_indexing:constraint_classification(us_military_recruitment_advertising, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, tangled_rope,
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
% % Perspective 4A: Captured regulator (institutional, constrained exit)
% constraint_indexing:constraint_classification([id], [type],
%     context(agent_power(institutional),
%             time_horizon(generational),
%             exit_options(constrained),
%             spatial_scope(national))).
%
% % Perspective 4B: Regulated company (institutional, arbitrage exit)
% constraint_indexing:constraint_classification([id], [type],
%     context(agent_power(institutional),
%             time_horizon(generational),
%             exit_options(arbitrage),
%             spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_military_recruitment_advertising_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(us_military_recruitment_advertising, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_military_recruitment_advertising, TypeBeneficiary, context(agent_power(organized), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(us_military_recruitment_advertising, ExtMetricName, E),
    E >= 0.46. % High-extraction Tangled Rope

:- end_tests(us_military_recruitment_advertising_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set to 0.55 because while the military provides benefits (training, pay), it also extracts significant risk and potential harm from recruits. The suppression score is 0.70 because recruitment advertising actively shapes perceptions and limits awareness of the downsides.
 *
 * PERSPECTIVAL GAP:
 *   Young adults perceive the advertising as a potential snare due to the risks they face after enlisting. The military views it as a rope because it coordinates recruitment and manpower. The analytical observer sees it as a tangled rope because it involves both coordination (recruitment) and asymmetric extraction (risk to recruits).
 *
 * DIRECTIONALITY LOGIC:
 *   Young adults bear the costs in terms of potential injury, death, and psychological trauma. The US military and defense contractors benefit from increased recruitment, which supports military spending and operations. The beneficiary/victim declarations accurately reflect this power dynamic.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by acknowledging the coordination aspect (recruitment) while also highlighting the asymmetric extraction (risks borne by recruits).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_us_military_recruitment_advertising,
    'To what extent does recruitment advertising accurately reflect the realities of military service?',
    'Longitudinal studies comparing recruits perceptions before and after service.',
    'If True: Classification moves towards Rope. If False: Classification remains Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_military_recruitment_advertising, 0, 10).

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
narrative_ontology:measurement(us_military_recruitment_advertising_tr_t0, us_military_recruitment_advertising, theater_ratio, 0, 0.30).
narrative_ontology:measurement(us_military_recruitment_advertising_tr_t5, us_military_recruitment_advertising, theater_ratio, 5, 0.40).
narrative_ontology:measurement(us_military_recruitment_advertising_tr_t10, us_military_recruitment_advertising, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(us_military_recruitment_advertising_ex_t0, us_military_recruitment_advertising, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(us_military_recruitment_advertising_ex_t5, us_military_recruitment_advertising, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(us_military_recruitment_advertising_ex_t10, us_military_recruitment_advertising, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(us_military_recruitment_advertising, information_standard).

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