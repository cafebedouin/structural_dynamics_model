% ============================================================================
% CONSTRAINT STORY: us_canada_geopolitical_asymmetry
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_us_canada_geopolitical_asymmetry, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_canada_geopolitical_asymmetry
 *   human_readable: The Geopolitical Constraint of US Proximity on Canadian Sovereignty
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The vast and structural power imbalance between the United States and Canada
 *   imposes a permanent constraint on Canadian strategic autonomy. This constraint,
 *   highlighted by Canadian military planning for a potential US invasion (even as a
 *   low-probability, high-impact event), forces resource allocation towards defense,
 *   limits foreign policy options, and creates a dynamic of asymmetric dependency.
 *   While the relationship has coordination benefits (e.g., NORAD), it is
 *   underpinned by the latent threat of overwhelming US power.
 *
 * KEY AGENTS (by structural relationship):
 *   - Canadian State: Primary target (organized/trapped) — bears the costs of suppressed autonomy and forced strategic alignment.
 *   - United States Government: Primary beneficiary (institutional/arbitrage) — benefits from a secure northern border, strategic depth, and inherent leverage over its neighbor.
 *   - Canadian Citizen: Powerless agent (powerless/trapped) - experiences the constraint as an unchangeable background condition.
 *   - Geopolitical Analyst: Analytical observer — sees the full structure of coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_canada_geopolitical_asymmetry, 0.48).
domain_priors:suppression_score(us_canada_geopolitical_asymmetry, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_canada_geopolitical_asymmetry, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_canada_geopolitical_asymmetry, extractiveness, 0.48).
narrative_ontology:constraint_metric(us_canada_geopolitical_asymmetry, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(us_canada_geopolitical_asymmetry, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_canada_geopolitical_asymmetry, tangled_rope).
narrative_ontology:human_readable(us_canada_geopolitical_asymmetry, "The Geopolitical Constraint of US Proximity on Canadian Sovereignty").
narrative_ontology:topic_domain(us_canada_geopolitical_asymmetry, "geopolitical").

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_canada_geopolitical_asymmetry). % The latent threat of US power is a form of continuous, passive enforcement.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_canada_geopolitical_asymmetry, us_government).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_canada_geopolitical_asymmetry, canadian_state).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)

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

% PERSPECTIVE 1: THE PRIMARY TARGET (THE CANADIAN STATE)
% As a nation-state, Canada has organizational capacity, but is geographically
% 'trapped' next to a superpower. The engine derives a high d from its
% victim status and trapped exit, leading to high effective extraction (χ) and
% a Snare classification. The coordination benefits are overshadowed by the
% suppression of sovereign alternatives.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (THE UNITED STATES)
% The US government benefits from a stable, compliant northern border at low
% cost. The engine derives a very low d from its beneficiary status and
% arbitrage exit options, resulting in negative effective extraction (χ).
% From this perspective, the relationship is a pure coordination Rope.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analytical observer sees both the genuine coordination functions (NORAD,
% economic integration) and the severe, asymmetric extraction and suppression.
% The combination of these features, along with the requirement for latent
% enforcement (US hegemony), is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE INDIVIDUAL CITIZEN (MOUNTAIN)
% For an individual Canadian citizen, the US-Canada power dynamic is not a
% negotiable system but a fixed, unchangeable feature of their geopolitical
% reality. It functions as a Mountain, a background condition with zero
% degrees of freedom at their level of agency.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_canada_geopolitical_asymmetry_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap between Canada (target) and the US (beneficiary).
    constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, snare, context(agent_power(organized), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    % The analytical observer must see the hybrid nature of the constraint.
    constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % A Tangled Rope requires a beneficiary, a victim, and active enforcement.
    narrative_ontology:constraint_beneficiary(us_canada_geopolitical_asymmetry, _),
    narrative_ontology:constraint_victim(us_canada_geopolitical_asymmetry, _),
    domain_priors:requires_active_enforcement(us_canada_geopolitical_asymmetry).

:- end_tests(us_canada_geopolitical_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This value represents the significant, structural opportunity cost imposed on Canada. It is not direct financial extraction but manifests as constrained foreign policy, forced defense expenditures, and asymmetric economic terms.
 *   - Suppression (0.85): This score is high because the constraint effectively forecloses entire categories of strategic alternatives for Canada (e.g., aligning with a rival power). The geopolitical cost of deviation is prohibitively high.
 *   - Theater (0.20): While there is diplomatic posturing, the core constraint is structural and non-performative, rooted in military and economic reality.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound and driven by the extreme power asymmetry.
 *   - The US (Beneficiary) perceives a Rope: a low-cost coordination mechanism that provides a secure border and strategic depth. The extractive elements are invisible or normalized as a natural feature of international relations.
 *   - Canada (Target) perceives a Snare: an inescapable relationship that severely limits its sovereignty. The coordination benefits, while real, do not compensate for the fundamental suppression of autonomy.
 *   - The Individual Citizen (Powerless) perceives a Mountain: the geopolitical reality is a fixed, unchangeable background condition of life.
 *   - The Analytical observer classifies it as a Tangled Rope, acknowledging both the coordination (Rope-like features) and the coercive extraction (Snare-like features).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality `d` is derived from the structural roles. Canada is declared a `constraint_victim` with `trapped` exit options, driving its `d` value towards 1.0 and amplifying effective extraction χ. The US is a `constraint_beneficiary` with `arbitrage` exit options, pushing its `d` value near 0.0 and making χ negative. This correctly models the flow of strategic benefit from north to south.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors. It does not dismiss the relationship as pure coercion (a simple Snare), because that would ignore genuine coordination like NORAD. It also avoids portraying it as a purely voluntary partnership (a simple Rope), which would ignore the immense, non-negotiable power imbalance. The Tangled Rope classification captures this essential duality of simultaneous cooperation and coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_us_canada_asymmetry,
    'Is the threat of US intervention a latent, structural feature of the power imbalance, or an actively contemplated policy tool?',
    'Access to classified US strategic planning documents (e.g., war plans, NSC directives).',
    'If purely structural, it confirms the Tangled Rope classification. If it is an active tool, the constraint functions more like a pure Snare, and the base extractiveness score should be higher.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_canada_geopolitical_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint is high-extraction (ε > 0.46), so temporal data is required.
% The data models a slight intensification of the asymmetry and associated
% political theater over the last decade (e.g., post-2016 "America First" era).
% Time scale: T=0 (2014), T=5 (2019), T=10 (2024).

% Theater ratio over time:
narrative_ontology:measurement(us_can_tr_t0, us_canada_geopolitical_asymmetry, theater_ratio, 0, 0.15).
narrative_ontology:measurement(us_can_tr_t5, us_canada_geopolitical_asymmetry, theater_ratio, 5, 0.18).
narrative_ontology:measurement(us_can_tr_t10, us_canada_geopolitical_asymmetry, theater_ratio, 10, 0.20).

% Extraction over time:
narrative_ontology:measurement(us_can_ex_t0, us_canada_geopolitical_asymmetry, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(us_can_ex_t5, us_canada_geopolitical_asymmetry, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(us_can_ex_t10, us_canada_geopolitical_asymmetry, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The constraint's coordination function (e.g., NORAD) exists
% within a framework of hegemonic power, making 'enforcement_mechanism' the most
% fitting type.
narrative_ontology:coordination_type(us_canada_geopolitical_asymmetry, enforcement_mechanism).

% Network relationships: US leverage over Canada in the geopolitical domain
% directly affects its ability to extract concessions in other domains, such as
% military burden-sharing within NATO.
narrative_ontology:affects_constraint(us_canada_geopolitical_asymmetry, nato_burden_sharing).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation chain,
% using the explicit `constraint_beneficiary` and `constraint_victim` declarations
% combined with the agents' respective `exit_options` (`arbitrage` vs `trapped`),
% accurately computes the directionality values needed to produce the correct
% perspectival classifications.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */