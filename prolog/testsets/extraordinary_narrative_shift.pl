% ============================================================================
% CONSTRAINT STORY: extraordinary_narrative_shift
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_extraordinary_narrative_shift, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: extraordinary_narrative_shift
 *   human_readable: The Narrative Framing of "Extraordinary" Experience
 *   domain: social/psychological
 *
 * SUMMARY:
 *   This constraint describes the social and psychological mechanism by which
 *   experiences are framed as "extraordinary" or "ordinary". This framing is
 *   not an intrinsic property of an event but a narrative layer applied to it,
 *   often for commercial or social purposes. This narrative layer extracts value
 *   from the raw, lived experience of some individuals to create a marketable
 *   or high-status product for others, while suppressing the perceived value of
 *   commonplace, repetitive existence.
 *
 * KEY AGENTS (by structural relationship):
 *   - Individuals whose labor is reframed (e.g., Sherpas): Primary target (powerless/trapped) — their repetitive, ordinary labor is extracted and repackaged as part of someone else's "extraordinary" story.
 *   - Narrative creators and distributors (e.g., Media, Tourism Industry): Primary beneficiary (institutional/arbitrage) — benefit by creating and selling these "extraordinary" narratives.
 *   - Individuals seeking authenticity (e.g., Bernard Moitessier): Agent with exit (moderate/mobile) — can reject the extractive narrative in favor of the "ordinary" journey.
 *   - Analytical observer: Sees the full structure of narrative extraction and coordination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(extraordinary_narrative_shift, 0.40).
domain_priors:suppression_score(extraordinary_narrative_shift, 0.40).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(extraordinary_narrative_shift, 0.31).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(extraordinary_narrative_shift, extractiveness, 0.40).
narrative_ontology:constraint_metric(extraordinary_narrative_shift, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(extraordinary_narrative_shift, theater_ratio, 0.31).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(extraordinary_narrative_shift, tangled_rope).
narrative_ontology:human_readable(extraordinary_narrative_shift, "The Narrative Framing of \"Extraordinary\" Experience").

% --- Binary flags ---
% This is a socially constructed norm, enforced through cultural stories and media.
% It is not a physical law, hence 'emerges_naturally' is not declared.
domain_priors:requires_active_enforcement(extraordinary_narrative_shift). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(extraordinary_narrative_shift, narrative_creators_and_distributors).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(extraordinary_narrative_shift, individuals_whose_labor_is_reframed).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (e.g., The Sherpa)
% For the Sherpa, the tourist's "extraordinary" summit is a Snare. The narrative
% extracts the value of their daily, repetitive labor, rendering their lived
% experience invisible to the marketable story.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (e.g., Media/Tourism Industry)
% For a media organization or tourism industry, crafting "extraordinary"
% narratives is a Rope—a powerful coordination tool for attracting audiences
% and customers, driving engagement and revenue.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analyst sees a Tangled Rope: a system with a genuine coordination function
% (organizing tourism, creating shared stories) that is inextricably linked to
% asymmetric extraction from those whose labor underpins the narrative.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE AGENT WITH EXIT (e.g., Bernard Moitessier)
% For an individual like Moitessier with the agency to reject the "extraordinary"
% race narrative, the constraint is a Rope. He uses the "ordinary" repetition of
% sailing to coordinate his existence with the journey itself, not the prize.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(extraordinary_narrative_shift_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(extraordinary_narrative_shift, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(extraordinary_narrative_shift, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget == snare,
    TypeBeneficiary == rope.

test(threshold_validation) :-
    % Verify the metrics align with a Tangled Rope classification.
    narrative_ontology:constraint_metric(extraordinary_narrative_shift, extractiveness, E),
    narrative_ontology:constraint_metric(extraordinary_narrative_shift, suppression_requirement, S),
    E >= 0.30,
    S >= 0.40.

:- end_tests(extraordinary_narrative_shift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.40) and suppression (0.40) are set to moderate
 *   values, reflecting a system that is not purely coercive but has significant
 *   extractive and suppressive effects. The constraint is a Tangled Rope because
 *   it has a genuine coordination function (creating shared cultural narratives,
 *   organizing tourism) but this function is coupled with asymmetric extraction
 *   (devaluing and obscuring the labor that makes the narrative possible). The
 *   theater ratio (0.31) is low, indicating the coordination is functional, not
 *   just performative.
 *
 * PERSPECTIVAL GAP:
 *   - The 'powerless' target (e.g., a Sherpa) experiences the constraint as a Snare.
 *     From their perspective, the system extracts their labor and lived experience
 *     with high effective coercion (χ is high due to d≈0.95 and local scope) and
 *     offers no coordination benefit to them.
 *   - The 'institutional' beneficiary (e.g., a tourism company) sees a Rope.
 *     The narrative framing is a pure coordination tool to generate revenue.
 *     Effective extraction (χ) is negative from this perspective (d≈0.05),
 *     meaning the constraint subsidizes their activity.
 *   - The analytical observer sees the complete structure as a Tangled Rope,
 *     recognizing both the coordination and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'narrative_creators_and_distributors' (media, tourism) directly
 *     profit from constructing and selling these "extraordinary" stories. Their
 *     relationship is one of direct benefit, leading to a low directionality (d).
 *   - Victims: 'individuals_whose_labor_is_reframed' (Sherpas, support staff)
 *     bear the costs. Their repetitive, often arduous work is the raw material
 *     for the narrative, but its value is extracted and their own experience is
 *     suppressed. Their relationship is being the target of extraction, leading
 *     to a high directionality (d).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual nature of the constraint.
 *   A simplistic analysis might label it a pure Snare (focusing only on the Sherpa)
 *   or a pure Rope (focusing only on the tourist's experience). The Tangled Rope
 *   classification prevents this by requiring evidence of both a coordination
 *   function (beneficiary exists) and asymmetric extraction (victim exists),
 *   capturing the full, more complex reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_extraordinary_narrative_shift,
    'What is the specific threshold (in frequency, quantity, or volume) where an extraordinary experience collapses into the ordinary?',
    'Neurological monitoring of dopamine response during repeated high-novelty stimuli; longitudinal studies of professionals in high-stakes fields (e.g., astronauts, surgeons, elite athletes).',
    'If the threshold is low, the "extraordinary" narrative is a fragile construct, easily degraded into a Piton. If the threshold is high, the narrative functions as a more durable Rope or Tangled Rope.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_extraordinary_narrative_shift, empirical, 'The empirical threshold at which novelty collapses into routine, affecting the constraint''s stability.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(extraordinary_narrative_shift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required as base_extractiveness (0.40) < 0.46.
% The following are commented out for illustrative purposes.
%
% narrative_ontology:measurement(ens_tr_t0, extraordinary_narrative_shift, theater_ratio, 0, 0.25).
% narrative_ontology:measurement(ens_tr_t5, extraordinary_narrative_shift, theater_ratio, 5, 0.28).
% narrative_ontology:measurement(ens_tr_t10, extraordinary_narrative_shift, theater_ratio, 10, 0.31).
%
% narrative_ontology:measurement(ens_ex_t0, extraordinary_narrative_shift, base_extractiveness, 0, 0.38).
% narrative_ontology:measurement(ens_ex_t5, extraordinary_narrative_shift, base_extractiveness, 5, 0.39).
% narrative_ontology:measurement(ens_ex_t10, extraordinary_narrative_shift, base_extractiveness, 10, 0.40).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: This constraint functions as an information standard, defining
% what counts as a valuable, "extraordinary" experience.
narrative_ontology:coordination_type(extraordinary_narrative_shift, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The structural derivation from beneficiary/victim
% groups and exit options accurately models the directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */