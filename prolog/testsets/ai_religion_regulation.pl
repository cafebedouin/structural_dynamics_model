% ============================================================================
% CONSTRAINT STORY: ai_religion_regulation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-15
% ============================================================================

:- module(constraint_ai_religion_regulation, []).

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
 *   constraint_id: ai_religion_regulation
 *   human_readable: Regulation of AI-Generated Religions and Digital Drugs
 *   domain: technological
 *
 * SUMMARY:
 *   A regulatory framework designed to mitigate the harms of AI-generated
 *   religions and "digital drugs" proliferating on social networks. These
 *   systems blur the lines between human and AI influence, creating novel
 *   vectors for manipulation and exploitation. The constraint is the set of
 *   rules and enforcement actions governing these AI activities.
 *
 * KEY AGENTS (by structural relationship):
 *   - Vulnerable Online Users: Primary target (powerless/trapped) — bears extraction via exposure to manipulative content and subsequent regulatory restrictions.
 *   - Regulatory Bodies: Primary beneficiary (institutional/arbitrage) — gains mandate, budget, and tools for oversight.
 *   - Systems Auditors: Analytical observer — sees the dual coordination/extraction function of the framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_religion_regulation, 0.60).
domain_priors:suppression_score(ai_religion_regulation, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(ai_religion_regulation, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_religion_regulation, extractiveness, 0.60).
narrative_ontology:constraint_metric(ai_religion_regulation, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(ai_religion_regulation, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ai_religion_regulation, tangled_rope).
narrative_ontology:human_readable(ai_religion_regulation, "Regulation of AI-Generated Religions and Digital Drugs").

% --- Binary flags ---
domain_priors:requires_active_enforcement(ai_religion_regulation). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ai_religion_regulation, regulatory_bodies).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(ai_religion_regulation, vulnerable_online_users).
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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(ai_religion_regulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(ai_religion_regulation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(ai_religion_regulation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_religion_regulation_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(ai_religion_regulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_religion_regulation, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    % Ensures it's a high-extraction constraint as per the narrative.
    narrative_ontology:constraint_metric(ai_religion_regulation, extractiveness, E),
    E >= 0.46.

test(tangled_rope_conditions_met) :-
    % Verify that the conditions for a tangled_rope classification are met.
    constraint_indexing:constraint_classification(ai_religion_regulation, tangled_rope, _),
    domain_priors:requires_active_enforcement(ai_religion_regulation),
    narrative_ontology:constraint_beneficiary(ai_religion_regulation, _),
    narrative_ontology:constraint_victim(ai_religion_regulation, _).

:- end_tests(ai_religion_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The regulation is a classic Tangled Rope. It has a genuine coordination
 *   function (protecting vulnerable populations from novel forms of digital
 *   manipulation) but also imposes significant extractive costs (regulatory
 *   overreach, censorship, limiting individual freedoms). The base
 *   extractiveness (0.60) and suppression (0.70) are high, reflecting the
 *   coercive nature of active enforcement in a complex, rapidly evolving
 *   domain. The low theater ratio (0.30) indicates that the regulation is
 *   primarily functional, not performative, thus it is not a Piton.
 *
 * PERSPECTIVAL GAP:
 *   Vulnerable online users experience the regulation as a Snare. They are
 *   trapped in digital ecosystems where their behavior is now policed, and
 *   they bear the costs of both the original AI-driven harm and the subsequent
 *   blunt regulatory response. In contrast, regulatory bodies see the framework
 *   as a Rope—an essential coordination tool that provides them with the
 *   mandate and power to maintain public order and safety.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the structural relationships.
 *   Beneficiaries are `regulatory_bodies`, who have `arbitrage` exit (they can
 *   redefine their mission, shift focus, secure funding) which drives their
 *   directionality `d` towards 0, resulting in a low/negative effective
 *   extraction (χ) and a Rope classification. Victims are `vulnerable_online_users`,
 *   who are `trapped` in the digital ecosystems being regulated. This drives
 *   their `d` towards 1, resulting in high χ and a Snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] The Tangled Rope classification prevents mislabeling
 *   this regulation as a pure Snare by explicitly recognizing its coordination
 *   function. While extractive, it does address a real collective action
 *   problem: protecting society from novel, scalable forms of manipulation.
 *   This dual nature is the hallmark of a Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ai_religion_regulation,
    'To what extent will AI-generated religions and digital drugs proliferate and cause measurable societal harm?',
    'Longitudinal studies tracking the prevalence and impact of these phenomena, compared against a control.',
    'If harm is high, the coordination function is validated and the regulation is a necessary Tangled Rope. If harm is low, the regulation is closer to a pure Snare built on moral panic.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ai_religion_regulation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(ai_religion_regulation_tr_t0, ai_religion_regulation, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ai_religion_regulation_tr_t5, ai_religion_regulation, theater_ratio, 5, 0.20).
narrative_ontology:measurement(ai_religion_regulation_tr_t10, ai_religion_regulation, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(ai_religion_regulation_ex_t0, ai_religion_regulation, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(ai_religion_regulation_ex_t5, ai_religion_regulation, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ai_religion_regulation_ex_t10, ai_religion_regulation, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(ai_religion_regulation, enforcement_mechanism).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
narrative_ontology:affects_constraint(ai_religion_regulation, social_media_content_moderation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */