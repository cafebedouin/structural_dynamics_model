% ============================================================================
% CONSTRAINT STORY: sm_addictive_design
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_sm_addictive_design, []).

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
 *   constraint_id: sm_addictive_design
 *   human_readable: Social Media Addictive Design Features
 *   domain: technological
 *
 * SUMMARY:
 *   This constraint represents the set of design principles and algorithmic
 *   features common to modern social media platforms, which are optimized to
 *   maximize user engagement and time-on-platform. These features, while
 *   providing a coordination function (connecting users), are alleged to
 *   behave as an extractive mechanism by fostering behavioral addiction,
 *   particularly among younger users, leading to negative mental health
 *   outcomes. The lawsuit against Snap by school districts is a formal
 *   challenge to this mechanism.
 *
 * KEY AGENTS (by structural relationship):
 *   - Adolescent Users: Primary target (powerless/trapped) — bear the costs of addiction and mental health degradation.
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — benefit from maximized engagement and resulting ad revenue.
 *   - Legal System/Regulators: Secondary institutional actor (institutional/constrained) — attempt to impose costs on the extraction mechanism.
 *   - Analytical Observer: Analytical observer — sees the dual function of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sm_addictive_design, 0.68).
domain_priors:suppression_score(sm_addictive_design, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(sm_addictive_design, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sm_addictive_design, extractiveness, 0.68).
narrative_ontology:constraint_metric(sm_addictive_design, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(sm_addictive_design, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(sm_addictive_design, tangled_rope).
narrative_ontology:human_readable(sm_addictive_design, "Social Media Addictive Design Features").
narrative_ontology:topic_domain(sm_addictive_design, "technological").

% --- Binary flags ---
domain_priors:requires_active_enforcement(sm_addictive_design). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(sm_addictive_design, platform_operators).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(sm_addictive_design, adolescent_users).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (ADOLESCENT USER)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% This high effective extraction (χ) and high suppression classify the
% constraint as a Snare from their perspective.
constraint_indexing:constraint_classification(sm_addictive_design, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (PLATFORM OPERATOR)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% From this view, the "extractive" features are simply effective tools for
% user retention, making the platform a highly efficient coordination Rope.
constraint_indexing:constraint_classification(sm_addictive_design, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. Sees both the coordination function and the
% asymmetric extraction. With high ε, high suppression, and a beneficiary/victim
% pair, the system is correctly identified as a Tangled Rope.
constraint_indexing:constraint_classification(sm_addictive_design, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE (THE LEGAL SYSTEM / REGULATOR) ---
% The legal system is an institutional actor attempting to constrain another
% institutional actor (the platform). Its exit is constrained by precedent and
% law. The default derivation would incorrectly classify it as a beneficiary.
% We use a directionality override to model its adversarial stance, aligning it
% with the interests of the victims it seeks to represent.
constraint_indexing:constraint_classification(sm_addictive_design, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sm_addictive_design_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:classify(sm_addictive_design,
        context(agent_power(powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(global)),
        snare),
    constraint_indexing:classify(sm_addictive_design,
        context(agent_power(institutional), time_horizon(generational), exit_options(arbitrage), spatial_scope(global)),
        rope).

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:classify(sm_addictive_design,
        context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global)),
        tangled_rope).

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(sm_addictive_design, _),
    narrative_ontology:constraint_victim(sm_addictive_design, _),
    domain_priors:requires_active_enforcement(sm_addictive_design).

:- end_tests(sm_addictive_design_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.68): High. Represents the significant mental health cost and attention drain imposed on users, which is the direct source of the platform's monetary value. The extraction is not a side effect; it is the core business model.
 *   - Suppression (0.85): High. Network effects create a powerful lock-in. For an adolescent, leaving the platform can mean social isolation, making alternatives structurally inaccessible even if they technically exist. This high suppression score is critical for the Snare classification.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the user's Snare and the platform's Rope is profound. For the user, the experience is coercive and harmful, a trap they cannot easily escape. For the platform, it is a tool for connection that they built and monetize effectively; from their index, the extraction is a feature, not a bug. This gap is the source of the social and legal conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `platform_operators`. They directly benefit from the revenue generated by the attention extracted via addictive design.
 *   - Victim: `adolescent_users`. They bear the direct costs in the form of degraded mental health, lost time, and potential for addiction.
 *   These declarations correctly inform the engine, leading to a negative effective extraction (χ) for the beneficiary and a very high positive χ for the victim.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The legal system is modeled as an institutional actor with `constrained` exit. Its structural role is to act as a check on the platform on behalf of the victims. The default derivation chain (based on power atom) would incorrectly assign a low directionality (`d=0.0`) to this institutional actor, making it appear as a beneficiary. A `directionality_override` to `d=0.75` is used to accurately model its adversarial role, reflecting its attempt to increase costs for the platform's extractive behavior. This correctly classifies the constraint as a Tangled Rope from the legal system's perspective — it acknowledges the platform's legitimate coordination function but targets its harmful extractive component.
 *
 * MANDATROPHY ANALYSIS:
 *   This model avoids mislabeling the platform as a pure Snare (which would ignore its genuine coordination function) or a pure Rope (which would ignore its immense, asymmetric harm). The analytical classification of Tangled Rope is crucial, as it identifies a system with a legitimate purpose that has been corrupted by an overwhelming extractive incentive. This diagnosis points toward regulatory solutions that don't seek to ban the platform (destroying the Rope) but to curtail the addictive features (taming the Snare component). The Dynamic Coalition extension is relevant here, as lawsuits represent an organization of previously `powerless` victims into a powerful bloc.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_sm_addictive_design,
    'Is the addictive potential an intentional design choice for extraction, or an unavoidable byproduct of creating an engaging coordination tool?',
    'Internal platform documentation, whistleblower testimony, or A/B testing data that explicitly weighed engagement metrics against user well-being metrics.',
    'If intentional, the ε is understated and the system is closer to a pure Snare. If an unavoidable byproduct, the system is a classic Tangled Rope where the challenge is mitigating harm without destroying the coordination function.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(sm_addictive_design, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has intensified over time as platforms refined their
% engagement algorithms and monetization strategies.
% Base Extractiveness (ε) started lower and increased as optimization became more aggressive.
% Theater Ratio also increased as platforms built out trust & safety teams in response to public pressure.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(sm_addictive_design_tr_t0, sm_addictive_design, theater_ratio, 0, 0.10).
narrative_ontology:measurement(sm_addictive_design_tr_t5, sm_addictive_design, theater_ratio, 5, 0.25).
narrative_ontology:measurement(sm_addictive_design_tr_t10, sm_addictive_design, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(sm_addictive_design_ex_t0, sm_addictive_design, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sm_addictive_design_ex_t5, sm_addictive_design, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(sm_addictive_design_ex_t10, sm_addictive_design, base_extractiveness, 10, 0.68).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The platform provides a standardized protocol for
% members of a social graph to exchange information.
narrative_ontology:coordination_type(sm_addictive_design, information_standard).

% Network relationship: Addictive design is a key mechanism within the broader
% economic model of surveillance capitalism.
narrative_ontology:affects_constraint(sm_addictive_design, surveillance_capitalism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% An override is necessary for the inter-institutional perspective of the legal system.
% The default derivation for an 'institutional' power atom that is not an explicitly
% declared beneficiary or victim would be d=0.0 (canonical), incorrectly implying it
% benefits. This override models its role as an external check, structurally
% aligned with the victims. d=0.75 positions it as a powerful, organized actor
% working against the extraction, similar to an 'analytical' or 'organized' victim.
constraint_indexing:directionality_override(sm_addictive_design, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */