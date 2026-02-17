% ============================================================================
% CONSTRAINT STORY: paxsilica_framework
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_paxsilica_framework, []).

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
 *   constraint_id: paxsilica_framework
 *   human_readable: "PaxSilica AI and Silicon Governance Framework"
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   Based on a US announcement, "PaxSilica" is a proposed international
 *   framework for coordinating policy on artificial intelligence and the
 *   high-end silicon chips required to develop it. The framework aims to
 *   establish standards for AI safety and create a "bulwark against
 *   technological authoritarianism" by coordinating the actions of member
 *   states, while implicitly excluding and suppressing strategic rivals.
 *
 * KEY AGENTS (by structural relationship):
 *   - Individual researchers/startups in excluded nations: Ultimate target (powerless/trapped) — face total exclusion with no recourse.
 *   - Excluded nations' tech sectors (e.g., China): Primary target (organized/constrained) — bears the cost of technological suppression and exclusion.
 *   - US government and tech sector: Primary beneficiary (institutional/arbitrage) — architects the framework, reinforces technological leadership, and sets global standards.
 *   - Invited member nations (e.g., India): Secondary beneficiary/constrained party (powerful/constrained) — gains access and a "seat at the table" but cedes policy autonomy.
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees the dual function of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paxsilica_framework, 0.48).
domain_priors:suppression_score(paxsilica_framework, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(paxsilica_framework, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paxsilica_framework, extractiveness, 0.48).
narrative_ontology:constraint_metric(paxsilica_framework, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(paxsilica_framework, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(paxsilica_framework, tangled_rope).
narrative_ontology:human_readable(paxsilica_framework, "PaxSilica AI and Silicon Governance Framework").

% --- Binary flags ---
domain_priors:requires_active_enforcement(paxsilica_framework). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(paxsilica_framework, us_government_and_tech_sector).
narrative_ontology:constraint_beneficiary(paxsilica_framework, invited_member_nations).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(paxsilica_framework, excluded_nations_tech_sector).
% Member nations are also victims in the sense of lost autonomy, but the
% primary structural victim is the excluded group.

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

% PERSPECTIVE 1: THE INDIVIDUAL IN AN EXCLUDED NATION (Ultimate Target)
% For an individual researcher or small startup in a targeted nation, the
% framework is an insurmountable barrier. They are powerless and trapped,
% experiencing it as a pure Snare that forecloses their professional future.
constraint_indexing:constraint_classification(paxsilica_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE FRAMEWORK ARCHITECT (Primary Beneficiary)
% The US government sees the framework as a tool for coordinating allies and
% setting beneficial standards. The engine derives a very low d from its
% beneficiary status and arbitrage exit, resulting in low/negative effective
% extraction (χ) and a Rope classification.
constraint_indexing:constraint_classification(paxsilica_framework, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE EXCLUDED NATION (Organized Target)
% As the target of suppression, the organized tech sector of an excluded nation
% experiences the framework as a coercive instrument of containment. The engine
% derives a high d from their victim status and constrained exit, leading to
% high effective extraction (χ) and a Snare classification.
constraint_indexing:constraint_classification(paxsilica_framework, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE INVITED MEMBER NATION (e.g., India)
% Invited members gain benefits (technology access, security) but pay a price
% (policy constraints, reduced sovereignty). They are both beneficiaries and
% victims. This hybrid position, combined with a constrained exit (leaving
% the alliance is costly), reveals the constraint's dual nature. The engine
% derives a mid-range d, resulting in a Tangled Rope classification.
constraint_indexing:constraint_classification(paxsilica_framework, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER
% The analytical view recognizes both the genuine coordination function and the
% asymmetric extraction. It sees the structure as a whole, classifying it as a
% Tangled Rope. This is the basis for the constraint_claim.
constraint_indexing:constraint_classification(paxsilica_framework, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paxsilica_framework_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(paxsilica_framework, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(paxsilica_framework, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(perspectival_gap_member_architect) :-
    constraint_indexing:constraint_classification(paxsilica_framework, tangled_rope, context(agent_power(powerful), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(paxsilica_framework, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(tangled_rope_gates_pass) :-
    narrative_ontology:constraint_beneficiary(paxsilica_framework, _),
    narrative_ontology:constraint_victim(paxsilica_framework, _),
    domain_priors:requires_active_enforcement(paxsilica_framework).

:- end_tests(paxsilica_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This high value reflects that the constraint is not merely about coordination. It imposes significant costs on the excluded target (technological suppression) and extracts policy sovereignty from its members.
 *   - Suppression Score (0.65): The framework's primary geopolitical function is to suppress the technological advancement of a strategic rival by controlling access to critical components and knowledge. This requires high coercion and foreclosure of alternatives for members.
 *   - requires_active_enforcement: This is true because the framework depends on continuous diplomatic pressure, export controls, and monitoring to prevent technology leakage and ensure member compliance.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The US (architect) sees a Rope, a tool for global stability and coordination. An excluded nation (target) sees a Snare, a geopolitical cage designed to strangle its growth. An invited member sees a Tangled Rope, a complex bargain trading sovereignty for security and access. The powerless researcher inside an excluded nation experiences the most extreme version of the Snare. This divergence is the key signature of a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The 'us_government_and_tech_sector' benefits directly by setting the rules of the game. 'invited_member_nations' are also beneficiaries, gaining security and technology.
 *   - Victims: The 'excluded_nations_tech_sector' is the primary victim, bearing the direct cost of suppression. The framework's existence is predicated on extracting from this group.
 *   The engine uses this data to compute directionality (d). The architect has arbitrage exit + beneficiary status, yielding a low d. The target has constrained exit + victim status, yielding a high d. The member has constrained exit + beneficiary status, yielding an intermediate d. This correctly models the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two common errors. A naive analysis focusing on the "AI safety" and "international cooperation" narrative would misclassify this as a pure Rope. Conversely, an analysis focusing only on the anti-China containment aspect would misclassify it as a pure Snare. The Tangled Rope classification is more accurate because it acknowledges that the legitimate coordination function is being used as a vehicle for coercive, asymmetric extraction. The promise of stability (Rope) is the justification for the geopolitical containment (Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_paxsilica_framework,
    'Is the primary long-term function of PaxSilica coordination for safety or geopolitical containment?',
    'Analysis of enforcement actions, technology-sharing agreements, and budget allocations over a 5-10 year period.',
    'If primarily coordination -> ε drifts down, becomes a Rope. If primarily containment -> theater rises, coordination function atrophies, may become a Piton or pure Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(paxsilica_framework, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the constraint's potential evolution from a more
% coordinative initial state to a more extractive mature state. This is
% required as base_extractiveness (0.48) > 0.46.

% Theater ratio over time (potential for Goodhart drift):
narrative_ontology:measurement(paxsilica_tr_t0, paxsilica_framework, theater_ratio, 0, 0.20).
narrative_ontology:measurement(paxsilica_tr_t5, paxsilica_framework, theater_ratio, 5, 0.20).
narrative_ontology:measurement(paxsilica_tr_t10, paxsilica_framework, theater_ratio, 10, 0.20).

% Extraction over time (potential for extraction accumulation):
narrative_ontology:measurement(paxsilica_ex_t0, paxsilica_framework, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(paxsilica_ex_t5, paxsilica_framework, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(paxsilica_ex_t10, paxsilica_framework, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It establishes rules and access for a critical global
% technology infrastructure.
narrative_ontology:coordination_type(paxsilica_framework, global_infrastructure).

% Network relationships: This framework directly impacts and attempts to
% control the global semiconductor supply chain.
narrative_ontology:affects_constraint(paxsilica_framework, semiconductor_supply_chain).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation chain
% (beneficiary/victim declarations + exit options) accurately models the
% directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */