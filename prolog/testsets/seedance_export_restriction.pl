% ============================================================================
% CONSTRAINT STORY: seedance_export_restriction
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-30
% ============================================================================

:- module(constraint_seedance_export_restriction, []).

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
 *   constraint_id: seedance_export_restriction
 *   human_readable: US Export Restrictions on ByteDance's SeeDance AI
 *   domain: political/technological/economic
 *
 * SUMMARY:
 *   The US government imposes export restrictions on ByteDance's SeeDance AI,
 *   preventing its widespread use and potential transfer of technology to foreign
 *   entities. This is framed as a measure to prevent China from gaining undue
 *   influence in AI-driven entertainment and data analytics.
 *
 * KEY AGENTS (by structural relationship):
 *   - ByteDance (SeeDance): Primary target (institutional/constrained) — bears primary extraction
 *   - International developers and small businesses: Secondary target (powerless/trapped) — choices are constrained by the restriction
 *   - US Government: Primary beneficiary (institutional/arbitrage) — benefits from restriction
 *   - US AI Companies: Secondary beneficiary (organized/mobile)
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(seedance_export_restriction, 0.55).
domain_priors:suppression_score(seedance_export_restriction, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(seedance_export_restriction, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(seedance_export_restriction, extractiveness, 0.55).
narrative_ontology:constraint_metric(seedance_export_restriction, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(seedance_export_restriction, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(seedance_export_restriction, tangled_rope).
narrative_ontology:human_readable(seedance_export_restriction, "US Export Restrictions on ByteDance's SeeDance AI").
narrative_ontology:topic_domain(seedance_export_restriction, "political/technological/economic").

% --- Binary flags ---
domain_priors:requires_active_enforcement(seedance_export_restriction).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(seedance_export_restriction, us_government).
narrative_ontology:constraint_beneficiary(seedance_export_restriction, us_ai_companies).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(seedance_export_restriction, bytedance).
narrative_ontology:constraint_victim(seedance_export_restriction, international_developers).

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

% PERSPECTIVE 1: THE POWERLESS TARGET (International Developers)
constraint_indexing:constraint_classification(seedance_export_restriction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ByteDance (PRIMARY INSTITUTIONAL TARGET)
constraint_indexing:constraint_classification(seedance_export_restriction, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: US Government (PRIMARY BENEFICIARY)
constraint_indexing:constraint_classification(seedance_export_restriction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: US AI Companies (SECONDARY BENEFICIARY)
constraint_indexing:constraint_classification(seedance_export_restriction, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER
constraint_indexing:constraint_classification(seedance_export_restriction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(seedance_export_restriction_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between the powerless target and institutional beneficiary.
    constraint_indexing:constraint_classification(seedance_export_restriction, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(seedance_export_restriction, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget == snare,
    TypeBeneficiary == rope.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(seedance_export_restriction, ExtMetricName, E),
    E >= 0.46. % high-extraction Tangled Rope.

:- end_tests(seedance_export_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The US government's export restrictions on ByteDance's SeeDance AI exhibit
 *   characteristics of a Tangled Rope. While there is a genuine coordination
 *   function (protecting national security and fostering domestic AI
 *   innovation), it also involves asymmetric extraction from ByteDance and other international actors, limiting
 *   their market access and technological development. The suppression score is
 *   high due to the significant barriers to circumventing the restrictions.
 *   The base extractiveness of 0.55 reflects the economic impact and
 *   technological constraint on its targets.
 *
 * PERSPECTIVAL GAP:
 *   ByteDance, along with powerless international developers, perceives the restriction as a Snare, limiting their global
 *   expansion and stifling innovation. The US government and domestic AI
 *   companies, on the other hand, view it as a Rope, fostering a safer AI
 *   landscape and promoting domestic competitiveness. The analytical view sees both functions, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The US government and domestic AI companies benefit from the restrictions,
 *   gaining a competitive advantage and bolstering national security.
 *   ByteDance and international developers bear the costs, facing limited market access and technological
 *   constraints. The beneficiary/victim declarations accurately reflect these
 *   structural relationships. The US Government is declared as an institutional actor with an arbitrage exit option, due to its ability to modify the rules to its benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by
 *   recognizing the legitimate national security concerns and the potential
 *   benefits for domestic AI innovation. However, it also acknowledges the
 *   asymmetric extraction and suppression of ByteDance's technological
 *   development. Without the coordination element, this would be a pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_seedance,
    'Is the US government truly motivated by national security, or primarily by economic competition?',
    'Analysis of declassified government documents and lobbying efforts by domestic AI companies.',
    'If true (national security), then the coordination function is more genuine. If false (economic competition), the constraint leans more towards pure extraction.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(seedance_export_restriction, 0, 10).

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
narrative_ontology:measurement(seedance_export_restriction_tr_t0, seedance_export_restriction, theater_ratio, 0, 0.20).
narrative_ontology:measurement(seedance_export_restriction_tr_t5, seedance_export_restriction, theater_ratio, 5, 0.25).
narrative_ontology:measurement(seedance_export_restriction_tr_t10, seedance_export_restriction, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(seedance_export_restriction_ex_t0, seedance_export_restriction, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(seedance_export_restriction_ex_t5, seedance_export_restriction, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(seedance_export_restriction_ex_t10, seedance_export_restriction, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(seedance_export_restriction, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(seedance_export_restriction, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(seedance_export_restriction, [other_constraint_id]).

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
% constraint_indexing:directionality_override(seedance_export_restriction, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */