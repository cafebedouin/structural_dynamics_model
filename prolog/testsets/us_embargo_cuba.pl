% ============================================================================
% CONSTRAINT STORY: us_embargo_cuba
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-28
% ============================================================================

:- module(constraint_us_embargo_cuba, []).

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
 *   constraint_id: us_embargo_cuba
 *   human_readable: US Embargo of Cuba
 *   domain: political/economic
 *
 * SUMMARY:
 *   The US Embargo of Cuba is a long-standing economic, commercial, and financial embargo imposed by the United States on Cuba. It has severely restricted Cuba's access to international markets, hindering its economic development and impacting the daily lives of Cuban citizens. The embargo has been in place for over six decades and continues to be a source of tension between the two countries.
 *
 * KEY AGENTS (by structural relationship):
 *   - Cuban citizens: Primary target (powerless/trapped) — bears extraction
 *   - US Government: Primary enforcer/beneficiary (institutional/constrained) — benefits politically
 *   - US Companies: Secondary victim (institutional/mobile) — loses trade opportunities
 *   - Cuban Government: Primary target (organized/constrained) — suffers constrained economic development
 *   - Analytical observer: Analytical observer — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_embargo_cuba, 0.65).
domain_priors:suppression_score(us_embargo_cuba, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_embargo_cuba, 0.30).       % Low theater; enforcement is functional, not performative.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_embargo_cuba, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_embargo_cuba, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_embargo_cuba, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not a mountain constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_embargo_cuba, snare).
narrative_ontology:human_readable(us_embargo_cuba, "US Embargo of Cuba").
narrative_ontology:topic_domain(us_embargo_cuba, "political/economic").

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_embargo_cuba). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Not a naturally emerging constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_embargo_cuba, us_government_political_actors).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_embargo_cuba, cuban_citizens).
narrative_ontology:constraint_victim(us_embargo_cuba, cuban_government).
narrative_ontology:constraint_victim(us_embargo_cuba, us_companies_seeking_trade).
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
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (CUBAN CITIZENS)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(us_embargo_cuba, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY ENFORCER/BENEFICIARY (US GOVERNMENT)
% Agent who benefits politically. Engine derives d from:
%   beneficiary membership + constrained exit → d ≈ 0.25 → f(d) ≈ 0.12 → moderate χ
% The 'constrained' exit reflects diplomatic and economic costs, making this a
% complex Tangled Rope, not a simple Rope.
constraint_indexing:constraint_classification(us_embargo_cuba, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. The high extraction and suppression, targeting a
% trapped population, lead to a Snare classification.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(us_embargo_cuba, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 4: US COMPANIES (SECONDARY VICTIM)
% US companies are victims as they lose trade opportunities, but have mobile
% exit (they can invest elsewhere). This makes it a Tangled Rope for them.
constraint_indexing:constraint_classification(us_embargo_cuba, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CUBAN GOVERNMENT (ORGANIZED TARGET)
% The Cuban government is a primary target, but with more power than citizens.
% The constraint remains a Snare due to the severe economic impact.
constraint_indexing:constraint_classification(us_embargo_cuba, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: GLOBAL TRADE PARTNERS
% Countries trading with Cuba face risks of secondary sanctions but have
% arbitrage options. For them, it's a Tangled Rope involving coordination and risk.
constraint_indexing:constraint_classification(us_embargo_cuba, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_embargo_cuba_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(us_embargo_cuba, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_embargo_cuba, TypeBeneficiary, context(agent_power(institutional), _, exit_options(constrained), _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget == snare,
    TypeBeneficiary == tangled_rope.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(us_embargo_cuba, ExtMetricName, E),
    E >= 0.46. % High-extraction Snare/Tangled.

:- end_tests(us_embargo_cuba_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The US Embargo of Cuba is scored as a high-extraction, high-suppression
 *   constraint. Base extractiveness (ε=0.65) reflects the severe hindrance to
 *   Cuba's economic activities. Suppression score (0.75) reflects the lack of
 *   alternatives for the Cuban economy and its citizens. The theater ratio (0.30)
 *   is low because the embargo is actively and functionally enforced, not merely
 *   performative. These metrics make it a clear Snare from the perspective of
 *   its targets.
 *
 * PERSPECTIVAL GAP:
 *   The gap is significant. For Cuban citizens (powerless/trapped), it is an
 *   inescapable Snare that extracts economic potential and well-being. For the
 *   US government (institutional/constrained), it is a Tangled Rope; it serves
 *   a domestic political coordination function but incurs diplomatic and economic
 *   costs, making it a complex, costly policy to maintain. For US companies and
 *   global trade partners, it is also a Tangled Rope, a barrier to be navigated
 *   rather than an absolute trap.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `us_government_political_actors` benefit from the domestic
 *     political signaling and coalition-building the policy enables.
 *   - Victims: `cuban_citizens` and the `cuban_government` bear the direct
 *     economic costs. `us_companies_seeking_trade` are also victims, losing
 *     market access. This distribution of costs and benefits drives the
 *     directionality calculations and the resulting perspectival gaps.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Snare from the target's perspective correctly
 *   identifies the constraint as primarily extractive, lacking any meaningful
 *   coordination function for the Cuban population. The `tangled_rope`
 *   classification from other perspectives (like the US government or trade
 *   partners) captures the complex coordination and enforcement dynamics
 *   required to maintain the embargo. This prevents a monolithic 'pure
 *   extraction' label for the entire system while still acknowledging the
 *   severe extractive impact on the primary target.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_us_embargo_cuba,
    'Is the primary driver of the embargo domestic political coordination (a Tangled Rope) or pure geopolitical extraction (a Snare)?',
    'Declassification of internal US policy documents and analysis of lobbying efforts vs. geopolitical outcomes.',
    'If primarily coordination, its removal depends on domestic political shifts. If primarily extraction, its removal depends on geopolitical power shifts.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_embargo_cuba, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. The embargo has intensified over its
% ~60 year history, which is modeled here on a 10-point scale.
% Required for high-extraction constraints (base_extractiveness > 0.46).
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(us_embargo_cuba_tr_t0, us_embargo_cuba, theater_ratio, 0, 0.10).
narrative_ontology:measurement(us_embargo_cuba_tr_t5, us_embargo_cuba, theater_ratio, 5, 0.20).
narrative_ontology:measurement(us_embargo_cuba_tr_t10, us_embargo_cuba, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(us_embargo_cuba_ex_t0, us_embargo_cuba, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(us_embargo_cuba_ex_t5, us_embargo_cuba, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(us_embargo_cuba_ex_t10, us_embargo_cuba, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The embargo is a tool of foreign policy enforcement.
narrative_ontology:coordination_type(us_embargo_cuba, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */