% ============================================================================
% CONSTRAINT STORY: nato_arctic_defense_cooperation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_nato_arctic_defense_cooperation, []).

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
 *   constraint_id: nato_arctic_defense_cooperation
 *   human_readable: NATO Arctic Defense Cooperation
 *   domain: political
 *
 * SUMMARY:
 *   NATO's increased focus on Arctic defense cooperation aims to counter Russian influence and protect critical infrastructure in the region. This involves joint military exercises, enhanced surveillance, and infrastructure development, creating a complex constraint with both coordination and potential extraction elements.
 *
 * KEY AGENTS (by structural relationship):
 *   - Russia: Primary target (institutional/constrained) — bears extraction
 *   - NATO Member States: Primary beneficiary (institutional/arbitrage) — benefits from increased security and resource access
 *   - Arctic Indigenous Communities: Secondary victim (powerless/trapped) — bears costs due to military activities and resource exploitation
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nato_arctic_defense_cooperation, 0.40).
domain_priors:suppression_score(nato_arctic_defense_cooperation, 0.50).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(nato_arctic_defense_cooperation, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nato_arctic_defense_cooperation, extractiveness, 0.40).
narrative_ontology:constraint_metric(nato_arctic_defense_cooperation, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(nato_arctic_defense_cooperation, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(nato_arctic_defense_cooperation, tangled_rope).
narrative_ontology:human_readable(nato_arctic_defense_cooperation, "NATO Arctic Defense Cooperation").

% --- Binary flags ---
domain_priors:requires_active_enforcement(nato_arctic_defense_cooperation). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(nato_arctic_defense_cooperation, nato_member_states).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(nato_arctic_defense_cooperation, russia).
narrative_ontology:constraint_victim(nato_arctic_defense_cooperation, arctic_indigenous_communities).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (RUSSIA)
% As an institutional actor being contained, Russia perceives a Snare.
constraint_indexing:constraint_classification(nato_arctic_defense_cooperation, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (NATO MEMBERS)
% NATO as an institution benefits from collective security, viewing it as a Rope.
constraint_indexing:constraint_classification(nato_arctic_defense_cooperation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the coordination (Rope) and extraction (Snare) functions.
constraint_indexing:constraint_classification(nato_arctic_defense_cooperation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: ARCTIC INDIGENOUS COMMUNITIES (SECONDARY VICTIM)
% Increased military activity and resource exploration negatively impacts their way of life.
constraint_indexing:constraint_classification(nato_arctic_defense_cooperation, tangled_rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 5: CANADIAN PERSPECTIVE
% Canada is both a NATO member and an Arctic nation, experiencing both benefits and constraints.
constraint_indexing:constraint_classification(nato_arctic_defense_cooperation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained), % Constrained by NATO obligations and Arctic sovereignty.
            spatial_scope(national))).

% PERSPECTIVE 6: US PERSPECTIVE
% The US is a major driver of NATO expansion, leveraging its position for strategic advantage.
constraint_indexing:constraint_classification(nato_arctic_defense_cooperation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage), % Can choose its level of engagement.
            spatial_scope(national))).

% PERSPECTIVE 7: FINNISH PERSPECTIVE
% Finland is a new member bordering Russia and the Arctic.
constraint_indexing:constraint_classification(nato_arctic_defense_cooperation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained), % Newly constrained by NATO obligations.
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nato_arctic_defense_cooperation_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between the primary target (Russia) and beneficiary (NATO).
    constraint_indexing:constraint_classification(nato_arctic_defense_cooperation, TypeTarget,
        context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(nato_arctic_defense_cooperation, TypeBeneficiary,
        context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTarget == snare,
    TypeBeneficiary == rope,
    TypeTarget \= TypeBeneficiary.

test(tangled_rope_metrics_validation) :-
    % Verify that the base metrics meet the minimum requirements for a Tangled Rope.
    narrative_ontology:constraint_claim(nato_arctic_defense_cooperation, tangled_rope),
    domain_priors:base_extractiveness(nato_arctic_defense_cooperation, E),
    domain_priors:suppression_score(nato_arctic_defense_cooperation, S),
    E >= 0.30,
    S >= 0.40.

:- end_tests(nato_arctic_defense_cooperation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (ε=0.40) and suppression (0.50) are set to reflect the core function of a Tangled Rope: a genuine coordination mechanism (collective defense for NATO members) that simultaneously imposes significant costs and limits alternatives for a targeted actor (Russia) and has negative externalities for other groups (Indigenous communities). The low theater ratio (0.30) indicates the constraint's actions are primarily functional, not performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For NATO members, the constraint is a Rope, a pure coordination tool for collective security. For Russia, it is a Snare, a coercive mechanism designed to contain its geopolitical influence. For Arctic Indigenous communities, it is a Tangled Rope, as they may experience some benefits from infrastructure but bear uncompensated costs from environmental disruption and militarization. The analytical view resolves this by identifying both the coordination and extraction functions, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `nato_member_states`. They gain security and strategic advantage. This declaration enables the `has_coordination_function` gate for the Tangled Rope classification.
 *   - Victims: `russia` and `arctic_indigenous_communities`. Russia is the direct target of the containment strategy. Indigenous communities are indirect victims of the increased activity in their territories. These declarations enable the `has_asymmetric_extraction` gate.
 *   The combination of beneficiary, victim, and `requires_active_enforcement` satisfies the three structural requirements for a Tangled Rope.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The perspectives for the US, Canada, and Finland highlight nuances within the beneficiary group. The US, with `exit_options(arbitrage)`, has maximum flexibility and benefits most. Canada and Finland, with `exit_options(constrained)`, benefit from the security but are also bound by alliance obligations and geography, experiencing a less purely beneficial version of the Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two potential errors. It is not a pure Rope, because that would ignore the clear, coercive extraction imposed on Russia and the negative externalities on local communities. It is not a pure Snare, because that would deny the genuine, effective security coordination it provides for its members. The Tangled Rope classification captures this essential duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_nato_arctic,
    'To what extent will NATO''s Arctic strategy prioritize environmental protection and Indigenous rights over military-strategic goals?',
    'Monitoring NATO policies, resource allocation for environmental mitigation, and formal engagement with Indigenous governing bodies.',
    'If prioritization is high: Reduced extraction on Indigenous communities, ε might decrease. If prioritization is low: Increased extraction, solidifying the Snare/Tangled Rope classification for that group.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(nato_arctic_defense_cooperation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is not > 0.46, so temporal data is not strictly required.
% However, it is provided to model the intensification of the constraint
% following geopolitical shifts.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(nato_arctic_defense_cooperation_tr_t0, nato_arctic_defense_cooperation, theater_ratio, 0, 0.20).
narrative_ontology:measurement(nato_arctic_defense_cooperation_tr_t5, nato_arctic_defense_cooperation, theater_ratio, 5, 0.30).
narrative_ontology:measurement(nato_arctic_defense_cooperation_tr_t10, nato_arctic_defense_cooperation, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(nato_arctic_defense_cooperation_ex_t0, nato_arctic_defense_cooperation, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(nato_arctic_defense_cooperation_ex_t5, nato_arctic_defense_cooperation, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(nato_arctic_defense_cooperation_ex_t10, nato_arctic_defense_cooperation, base_extractiveness, 10, 0.40).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(nato_arctic_defense_cooperation, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */