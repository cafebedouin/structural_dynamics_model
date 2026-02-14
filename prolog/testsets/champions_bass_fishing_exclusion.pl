% ============================================================================
% CONSTRAINT STORY: champions_bass_fishing_exclusion
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_champions_bass_fishing_exclusion, []).

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
 *   constraint_id: champions_bass_fishing_exclusion
 *   human_readable: Champions Bass Fishing Tournament Exclusionary Practices
 *   domain: economic
 *
 * SUMMARY:
 *   This constraint refers to the practices of Champions Bass Fishing (CBF) tournaments. CBF is invite-only and requires significant resources to participate, effectively creating a closed loop for elite anglers. This structure excludes smaller organizations or individual anglers from the primary path to professional success and high-value sponsorships, suppressing alternative competitive avenues.
 *
 * KEY AGENTS (by structural relationship):
 *   - Smaller Anglers/Organizations: Primary target (powerless/trapped) — bears extraction from limited competitive opportunities and suppressed career paths.
 *   - Champions Bass Fishing Organization: Primary beneficiary (institutional/arbitrage) — benefits from a consolidated, high-prestige brand and concentrated sponsorship deals.
 *   - Sponsors: Secondary actor (institutional/arbitrage) — benefit from association with prominent anglers and high brand visibility in a controlled environment.
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(champions_bass_fishing_exclusion, 0.48).
domain_priors:suppression_score(champions_bass_fishing_exclusion, 0.60).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(champions_bass_fishing_exclusion, 0.2).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(champions_bass_fishing_exclusion, extractiveness, 0.48).
narrative_ontology:constraint_metric(champions_bass_fishing_exclusion, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(champions_bass_fishing_exclusion, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(champions_bass_fishing_exclusion, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(champions_bass_fishing_exclusion, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(champions_bass_fishing_exclusion, tangled_rope).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(champions_bass_fishing_exclusion).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(champions_bass_fishing_exclusion). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(champions_bass_fishing_exclusion).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(champions_bass_fishing_exclusion, champions_bass_fishing_organization).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(champions_bass_fishing_exclusion, smaller_anglers_organizations).
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
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(champions_bass_fishing_exclusion_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(champions_bass_fishing_exclusion, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(champions_bass_fishing_exclusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is 0.48, signifying that a substantial portion of the value generated by the tournament circuit is captured by the organizing body and its chosen partners, rather than being accessible to the broader field of potential competitors. The suppression score is raised to 0.60 to reflect that this tournament structure is the dominant path to professional success, highly suppressing the viability of alternative competitive avenues. The low theater_ratio of 0.2 indicates the tournament is a functional, value-generating enterprise, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   Smaller anglers/organizations perceive a Snare. For them, the high barriers to entry and invite-only structure trap them outside the primary ecosystem for career advancement and sponsorship, making their own efforts less viable. Champions Bass Fishing perceives a Rope, as they are coordinating a high-value sporting event that benefits their organization, top-tier members, and sponsors through a consolidated, premium product.
 *
 * DIRECTIONALITY LOGIC:
 *   The CBF organization and its sponsors are the primary beneficiaries, capturing value from exclusivity and brand prestige. Smaller anglers and organizations are the victims, bearing the cost of exclusion from this value-capture system, which limits their income and exposure. The `trapped` exit option for victims reflects that there are no alternative circuits with comparable prestige or financial reward.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids mislabeling the system as pure extraction. The `tangled_rope` classification from an analytical view correctly identifies that CBF provides a genuine coordination function (organizing large-scale events). However, this coordination is coupled with an extractive structure that asymmetrically benefits the organizers and excludes a class of participants. It is not a pure Snare because the event itself is functional, but it is not a pure Rope because the benefits of coordination are not distributed symmetrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_champions_bass_fishing_exclusion,
    'Is the exclusionary practice a deliberate strategy to maintain a monopoly on talent and sponsorship, or an emergent consequence of the organization\'s scale and prestige?',
    'Review internal CBF communications, entry criteria evolution, and financial data on revenue sharing with anglers.',
    'If deliberate, it confirms the high suppression score and Snare/Tangled Rope classifications. If emergent, it might suggest a lower suppression score, shifting the powerless perspective from Snare to Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(champions_bass_fishing_exclusion, 0, 10).

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
narrative_ontology:measurement(champions_bass_fishing_exclusion_tr_t0, champions_bass_fishing_exclusion, theater_ratio, 0, 0.1).
narrative_ontology:measurement(champions_bass_fishing_exclusion_tr_t5, champions_bass_fishing_exclusion, theater_ratio, 5, 0.15).
narrative_ontology:measurement(champions_bass_fishing_exclusion_tr_t10, champions_bass_fishing_exclusion, theater_ratio, 10, 0.2).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(champions_bass_fishing_exclusion_ex_t0, champions_bass_fishing_exclusion, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(champions_bass_fishing_exclusion_ex_t5, champions_bass_fishing_exclusion, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(champions_bass_fishing_exclusion_ex_t10, champions_bass_fishing_exclusion, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(champions_bass_fishing_exclusion, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(champions_bass_fishing_exclusion, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(champions_bass_fishing_exclusion, [other_constraint_id]).

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
% narrative_ontology:affects_constraint(champions_bass_fishing_exclusion, [sibling_constraint_id]).

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
% constraint_indexing:directionality_override(champions_bass_fishing_exclusion, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */