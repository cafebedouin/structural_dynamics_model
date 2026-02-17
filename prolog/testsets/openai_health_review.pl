% ============================================================================
% CONSTRAINT STORY: openai_health_review
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_openai_health_review, []).

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
 *   constraint_id: openai_health_review
 *   human_readable: OpenAI ChatGPT Health Review Service
 *   domain: technological/economic
 *
 * SUMMARY:
 *   OpenAI has launched a service that uses a large language model to review
 *   users' health test results and provide personalized diet and lifestyle
 *   recommendations. Marketed as an accessible health tool, it operates
 *   outside traditional medical and regulatory frameworks, creating a new
 *   dynamic between users, the provider, and the established healthcare system.
 *
 * KEY AGENTS (by structural relationship):
 *   - Health Data Submitters: Primary target (powerless/trapped) — bears the risk of inaccurate advice and provides valuable training data.
 *   - OpenAI: Primary beneficiary (institutional/arbitrage) — gains market share, training data, and user dependency with minimal liability.
 *   - Medical Regulators (e.g., FDA): Institutional actor (institutional/constrained) — mandate is undermined by a service that bypasses established safety and efficacy protocols.
 *   - Analytical Observer: Sees the full structure of coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openai_health_review, 0.55).
domain_priors:suppression_score(openai_health_review, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(openai_health_review, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openai_health_review, extractiveness, 0.55).
narrative_ontology:constraint_metric(openai_health_review, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(openai_health_review, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(openai_health_review, tangled_rope).
narrative_ontology:human_readable(openai_health_review, "OpenAI ChatGPT Health Review Service").

% --- Binary flags ---
domain_priors:requires_active_enforcement(openai_health_review). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(openai_health_review, openai).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(openai_health_review, health_data_submitters).
narrative_ontology:constraint_victim(openai_health_review, medical_regulators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Users seeking health advice who bear the full risk of misinterpretation.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% Calculation: χ = 0.55 * 1.42 * 1.0 (national scope) = 0.781 (Snare, χ ≥ 0.66)
constraint_indexing:constraint_classification(openai_health_review, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% OpenAI, the service provider.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% Calculation: χ = 0.55 * -0.12 * 1.0 (national scope) = -0.066 (Rope, χ ≤ 0.35)
constraint_indexing:constraint_classification(openai_health_review, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context sees both coordination and extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15. Recognizes the structural flags for Tangled Rope.
% Calculation: χ = 0.55 * 1.15 * 1.2 (global scope) = 0.759. This high χ, combined
% with the presence of beneficiary (coordination) and victim (extraction) flags,
% confirms the Tangled Rope classification.
constraint_indexing:constraint_classification(openai_health_review, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 4: MEDICAL REGULATORS (SNARE)
% Institutional actors whose mandate is bypassed and authority is challenged.
% They are victims with constrained exit options (cannot ignore it, but slow to act).
% Victim + constrained exit -> high d -> high χ, classifying as a snare that traps their agency.
constraint_indexing:constraint_classification(openai_health_review, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openai_health_review_tests).

test(perspectival_gap_user_provider, [nondet]) :-
    % Verify the gap between the user (target) and provider (beneficiary).
    constraint_indexing:constraint_classification(openai_health_review, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(openai_health_review, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope, [nondet]) :-
    % The analytical observer must see the full tangled structure.
    constraint_indexing:constraint_classification(openai_health_review, tangled_rope, context(agent_power(analytical), _, _, _)).

test(regulator_view_is_snare, [nondet]) :-
    % Verify that the bypassed regulators perceive the constraint as a snare.
    constraint_indexing:constraint_classification(openai_health_review, snare, context(agent_power(institutional), _, exit_options(constrained), _)).

test(tangled_rope_structural_gates_met) :-
    % A constraint is only a Tangled Rope if it has all three structural properties.
    narrative_ontology:constraint_beneficiary(openai_health_review, _), % Has coordination function
    narrative_ontology:constraint_victim(openai_health_review, _),       % Has asymmetric extraction
    domain_priors:requires_active_enforcement(openai_health_review). % Requires enforcement

:- end_tests(openai_health_review_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High. The extraction is not monetary but structural: the transfer of medical risk to the user, the extraction of valuable health data for model training, and the creation of user dependency on a proprietary, unregulated system.
 *   - Suppression Score (0.70): High. The service suppresses the primary alternative (consulting a licensed medical professional) by being instant, free/cheap, and accessible. This is especially true for users with limited financial resources or access to healthcare.
 *   - The analytical classification is Tangled Rope because the system has a genuine (if risky) coordination function (matching user data to medical knowledge) while simultaneously featuring severe, asymmetric extraction. The presence of both `constraint_beneficiary` and `constraint_victim` alongside `requires_active_enforcement` meets the structural requirements for this classification.
 *
 * PERSPECTIVAL GAP:
 *   - The gap is stark. For a `powerless`/`trapped` user, the service is a Snare: it offers a solution but embeds hidden risks with no recourse, extracting value (data, dependence) in the process.
 *   - For OpenAI (`institutional`/`arbitrage`), it is a Rope: a coordination tool that organizes information to solve a user problem, generating immense value for the company with near-zero marginal cost and high strategic upside. They can exit or pivot at any time.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `openai` directly profits from user engagement, data acquisition, and market positioning in the high-value healthcare sector.
 *   - Victims: `health_data_submitters` bear all medical risks. `medical_regulators` are also victims, as their public safety mandate and regulatory authority are structurally undermined, forcing them into a reactive and constrained position.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The constraint creates a conflict between two institutional actors: OpenAI and the regulatory bodies. OpenAI, with `arbitrage` exit, operates freely. Regulators, with `constrained` exit, are trapped. They cannot easily ban the service without political backlash (appearing anti-innovation) nor can they ignore the public health risk. This asymmetry is why they perceive the constraint as a Snare on their institutional function.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two potential errors. It does not dismiss the service as a pure Snare, because it acknowledges the genuine (though unvalidated) coordination function it provides. It also does not accept the marketing claim of it being a pure Rope, because the indexical analysis from the powerless perspective reveals severe, non-consensual extraction of risk. The Tangled Rope classification captures this duality perfectly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_openai_health_review,
    'Is the aggregate medical advice provided by the service net beneficial or net harmful to public health?',
    'Long-term, large-scale, randomized controlled trials comparing health outcomes of users vs. non-users, and vs. traditional medical care.',
    'If net beneficial, the coordination function is stronger than modeled (ε might be lower). If net harmful, the extractive element is even more severe (ε is accurate or higher).',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(openai_health_review, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is required for this high-extraction (ε=0.55) constraint.
% The trajectory models an initial "growth" phase with lower extraction to build
% trust, followed by increasing extraction as the platform matures and monetizes.
% This pattern is typical of Extraction Accumulation drift.

% Theater ratio over time (starts high for marketing, then declines):
narrative_ontology:measurement(ohr_tr_t0, openai_health_review, theater_ratio, 0, 0.50).
narrative_ontology:measurement(ohr_tr_t5, openai_health_review, theater_ratio, 5, 0.40).
narrative_ontology:measurement(ohr_tr_t10, openai_health_review, theater_ratio, 10, 0.30).

% Extraction over time (starts lower, then increases):
narrative_ontology:measurement(ohr_ex_t0, openai_health_review, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ohr_ex_t5, openai_health_review, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ohr_ex_t10, openai_health_review, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The service coordinates unstructured user health data against
% a structured corpus of medical information.
narrative_ontology:coordination_type(openai_health_review, information_standard).

% Network relationship: This service structurally undermines the basis for
% professional medical licensing by creating a parallel, unregulated alternative.
narrative_ontology:affects_constraint(openai_health_review, medical_professional_licensing).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */