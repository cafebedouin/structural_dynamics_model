% ============================================================================
% CONSTRAINT STORY: openai_default_data_training
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_openai_default_data_training, []).

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
 *   constraint_id: openai_default_data_training
 *   human_readable: "Default Use of ChatGPT User Data for Model Training"
 *   domain: technological
 *
 * SUMMARY:
 *   This constraint describes the policy and technical implementation by which
 *   OpenAI uses conversations from non-API ChatGPT users to train its models
 *   by default. While users can opt-out, the burden is on them to do so,
 *   creating a structural extraction of valuable training data. A recent
 *   court ruling shielded this practice from federal DMCA claims but left it
 *   vulnerable to state-level privacy lawsuits, codifying its complex legal
 *   and ethical status.
 *
 * KEY AGENTS (by structural relationship):
 *   - ChatGPT Users: Primary target (powerless/trapped) — their data is extracted,
 *     representing a loss of privacy and intellectual property.
 *   - OpenAI: Primary beneficiary (institutional/arbitrage) — receives vast
 *     quantities of high-quality training data at zero marginal cost, a key
 *     competitive advantage.
 *   - AI Researchers / Society: Secondary beneficiaries — benefit from the
 *     accelerated capability improvements of models trained on this data.
 *   - Analytical Observer: Sees the full structure, recognizing both the
 *     coordination benefit and the asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openai_default_data_training, 0.52).
domain_priors:suppression_score(openai_default_data_training, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(openai_default_data_training, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openai_default_data_training, extractiveness, 0.52).
narrative_ontology:constraint_metric(openai_default_data_training, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(openai_default_data_training, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(openai_default_data_training, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(openai_default_data_training). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% N/A for this constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(openai_default_data_training, openai).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(openai_default_data_training, chatgpt_users).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET
%   Snare:        victim required; beneficiary optional -> MET

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

% PERSPECTIVE 1: THE CHATGPT USER (PRIMARY TARGET)
% For a user unaware of the opt-out, or for whom the privacy cost is high,
% the default data collection is purely extractive.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.52 * 1.42 * 1.2 (global scope) ≈ 0.88. This χ > 0.66 and suppression > 0.60
% classifies it as a Snare.
constraint_indexing:constraint_classification(openai_default_data_training, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OPENAI (PRIMARY BENEFICIARY)
% For OpenAI, this is a highly efficient coordination mechanism to gather the
% resources needed to improve its product.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.52 * -0.12 * 1.2 ≈ -0.07. This negative effective extraction makes it a
% pure Rope from their perspective.
constraint_indexing:constraint_classification(openai_default_data_training, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analyst sees both the coordination function (a better model for all)
% and the asymmetric extraction (uncompensated data from users).
% Engine derives canonical d for analytical ≈ 0.73 → f(d) ≈ 1.15.
% χ = 0.52 * 1.15 * 1.2 ≈ 0.72. This χ is in the [0.40, 0.90] range.
% With ε > 0.30, S > 0.40, and presence of coordination, extraction, and
% enforcement, it is a canonical Tangled Rope.
constraint_indexing:constraint_classification(openai_default_data_training, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openai_default_data_training_tests).

test(perspectival_gap_snare_vs_rope) :-
    constraint_indexing:constraint_classification(openai_default_data_training, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(openai_default_data_training, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Perspectival gap validated: Snare (powerless) vs. Rope (institutional).~n').

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(openai_default_data_training, tangled_rope, context(agent_power(analytical), _, _, _)),
    format('Analytical classification as Tangled Rope validated.~n').

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(openai_default_data_training, _),
    narrative_ontology:constraint_victim(openai_default_data_training, _),
    domain_priors:requires_active_enforcement(openai_default_data_training),
    format('Tangled Rope structural requirements (beneficiary, victim, enforcement) validated.~n').

:- end_tests(openai_default_data_training_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.52): High. The value of diverse, real-world user
 *     conversation data for training state-of-the-art LLMs is immense. This
 *     represents a massive in-kind subsidy from users to OpenAI, forming a
 *     core part of their R&D and competitive advantage.
 *   - Suppression (S=0.65): High. While alternative LLMs exist, ChatGPT has
 *     significant market and mindshare dominance. The default opt-in nature
 *     suppresses user agency, and the legal system has thus far suppressed
 *     broad, federal-level challenges to the practice, increasing its stability.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound and defines the controversy.
 *   - From the User's perspective (Snare): It's a non-consensual (by default)
 *     appropriation of their data and intellectual output, with potential privacy
 *     harms. The benefit (a slightly better model in the future) is diffuse
 *     and abstract compared to the concrete cost.
 *   - From OpenAI's perspective (Rope): It's a brilliant coordination mechanism.
 *     It aligns the act of using the product with the act of improving it,
 *     creating a powerful feedback loop. The extraction is seen as a necessary
 *     and fair trade for providing a free, powerful service.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations directly map to the flow of value.
 *   - `constraint_beneficiary(openai)`: OpenAI directly receives the data asset.
 *   - `constraint_victim(chatgpt_users)`: The users are the source of this asset
 *     and bear the privacy/IP risks.
 *   This structure, combined with the extreme power/exit asymmetry (institutional/
 *   arbitrage vs. powerless/trapped), is what drives the directionality `d` to
 *   its extremes, producing the Snare/Rope classification gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a classic case where a "pure extraction" (Snare) or "pure coordination"
 *   (Rope) label would be misleading. The Tangled Rope classification is essential
 *   because it acknowledges both truths simultaneously: the system *does* provide
 *   a genuine coordination benefit (a better model for society), but it achieves
 *   this via a mechanism of asymmetric, non-negotiated extraction. Ignoring the
 *   coordination function would miss why the system is so powerful and widely
 *   adopted; ignoring the extraction would miss the core ethical and privacy conflict.
 *   The powerless users' potential to organize (Dynamic Coalition extension) and
 *   shift their classification from Snare to something else is a key dynamic to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_openai_default_data_training,
    'Is mass user data a fundamental requirement for SOTA model improvement, or merely a cost-effective convenience?',
    'Analysis of model performance (e.g., HELM benchmarks) for models trained exclusively on synthetic/licensed data versus those trained on user data.',
    'If required (Mountain-like necessity), the coordination aspect is stronger. If a convenience (Snare-like opportunism), the extraction is more predatory.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(openai_default_data_training, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has intensified as OpenAI's market position has solidified
% and the value of training data has become more apparent. The theater ratio
% has also slightly increased as privacy policies become more scrutinized.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(openai_default_data_training_tr_t0, openai_default_data_training, theater_ratio, 0, 0.10).
narrative_ontology:measurement(openai_default_data_training_tr_t5, openai_default_data_training, theater_ratio, 5, 0.12).
narrative_ontology:measurement(openai_default_data_training_tr_t10, openai_default_data_training, theater_ratio, 10, 0.15).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(openai_default_data_training_ex_t0, openai_default_data_training, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(openai_default_data_training_ex_t5, openai_default_data_training, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(openai_default_data_training_ex_t10, openai_default_data_training, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The constraint allocates a resource (user data) to a
% collective pool for a shared purpose (model training).
narrative_ontology:coordination_type(openai_default_data_training, resource_allocation).

% Network relationships: This policy is in direct conversation with, and is
% shaped by, privacy regulations like the CCPA.
narrative_ontology:affects_constraint(openai_default_data_training, ccpa_privacy_rights).
narrative_ontology:affects_constraint(dmca_safe_harbor, openai_default_data_training).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard structural
% derivation from beneficiary/victim groups and exit options accurately
% models the power dynamics and produces the correct directionality values.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */