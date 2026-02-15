% ============================================================================
% CONSTRAINT STORY: copyright_protection
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_copyright_protection, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: copyright_protection
 *   human_readable: Copyright Protection Framework
 *   domain: economic/legal/technological
 *
 * SUMMARY:
 *   Copyright is a legal framework granting creators exclusive rights to their
 *   original works for a limited time. It creates an artificial scarcity of
 *   expression to incentivize creation, separating protected "expression" from
 *   unprotected "idea". This creates a fundamental tension between creator
 *   livelihoods, corporate control over culture, public access, and new forms
 *   of technological innovation (e.g., AI training).
 *
 * KEY AGENTS (by structural relationship):
 *   - Public Consumers & Derivative Creators: Primary target (powerless/trapped) — bears extraction of usage rights.
 *   - Large Media Corporations: Primary beneficiary (institutional/arbitrage) — benefits from monopoly rights and enforcement.
 *   - Independent Artists: Secondary beneficiary (moderate/mobile) — uses the system for coordination and income.
 *   - AI Developers: Secondary target (institutional/constrained) — experiences the system as a barrier to data access.
 *   - Analytical Observer: Sees the full coordination/extraction structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_protection, 0.40).
domain_priors:suppression_score(copyright_protection, 0.50).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(copyright_protection, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_protection, extractiveness, 0.40).
narrative_ontology:constraint_metric(copyright_protection, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(copyright_protection, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(copyright_protection, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(copyright_protection). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(copyright_protection, copyright_holders).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(copyright_protection, public_domain_advocates).
narrative_ontology:constraint_victim(copyright_protection, ai_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PUBLIC CONSUMER (MOUNTAIN)
% Agent who bears extraction of usage rights. Experiences copyright as an
% immutable fact of the digital landscape, too complex to navigate.
constraint_indexing:constraint_classification(copyright_protection, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE MEDIA CORPORATION (ROPE)
% Primary beneficiary who leverages the system for profit. For them, it is a
% pure coordination mechanism to structure markets and enforce monopoly rights.
constraint_indexing:constraint_classification(copyright_protection, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE INDEPENDENT ARTIST (ROPE)
% A secondary beneficiary who uses the system to coordinate income and control
% their work, though with less power than large institutions.
constraint_indexing:constraint_classification(copyright_protection, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: THE AI DEVELOPER (SNARE)
% An institutional actor, but one targeted by the constraint. The need for
% massive datasets makes individual licensing impossible, creating a legal
% bottleneck that traps and strangles large-scale innovation.
constraint_indexing:constraint_classification(copyright_protection, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The default analytical context, which sees both the valid coordination
% function for creators and the asymmetric extraction from the public and
% innovators. This matches the constraint_claim.
constraint_indexing:constraint_classification(copyright_protection, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_protection_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between different agents.
    constraint_indexing:constraint_classification(copyright_protection, TypeConsumer, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(copyright_protection, TypeCorp, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(copyright_protection, TypeAI, context(agent_power(institutional), _, exit_options(constrained), _)),
    TypeConsumer \= TypeCorp,
    TypeCorp \= TypeAI.

test(analytical_claim_consistency) :-
    narrative_ontology:constraint_claim(copyright_protection, ClaimedType),
    constraint_indexing:constraint_classification(copyright_protection, AnalyticalType, context(agent_power(analytical), _, _, _)),
    ClaimedType == AnalyticalType.

:- end_tests(copyright_protection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.40): Represents the value extracted from the public commons (freedom to use, share, adapt) and transferred to rights holders. It's significant but not total, as fair use provides some relief.
 *   - Suppression (0.50): High due to the chilling effects of automated enforcement (e.g., YouTube's Content ID) and the high cost of legal challenges, which suppresses even legitimate uses.
 *   - Theater (0.10): The system is largely functional, not performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For a consumer, it's an incomprehensible Mountain. For a media corporation, it's a perfect Rope for organizing their business model. For an AI developer, it's a Snare that halts their work. For an artist, it's a necessary Rope to build a career. This divergence highlights the constraint's hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'copyright_holders' captures both large corporations and individual creators who benefit from the state-granted monopoly.
 *   - Victims: 'public_domain_advocates' and 'ai_developers' represent groups whose activities are directly curtailed by the expansion and enforcement of copyright.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The story shows two institutional actors with opposite experiences. The media corporation (beneficiary, arbitrage exit) sees a Rope. The AI developer (victim, constrained exit) sees a Snare. This is a classic inter-institutional conflict mediated by a single legal constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope correctly identifies that copyright has a genuine coordination function (allowing creators to earn a living) that cannot be dismissed, while also acknowledging the significant, asymmetric extraction it imposes on the public and on technological innovation. It is neither pure coordination (Rope) nor pure extraction (Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_copyright_protection,
    "Will training AI on copyrighted material be legally classified as 'Transformative Fair Use', or will it require new licensing models?",
    "Supreme Court rulings on AI training datasets (e.g., NYT v. OpenAI), and the development of new, industry-standard AI licensing frameworks.",
    "If Yes (Fair Use): The 'Snare' for AI labs vanishes, shifting the constraint towards Rope. If No: The 'Snare' aspect intensifies, potentially becoming a Mountain for AI development.",
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_copyright_protection, empirical, "Legal precedent for AI training under Fair Use doctrine.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(copyright_protection, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is < 0.46, so temporal data is not strictly required.
% This section is included for structural completeness.
%
% narrative_ontology:measurement(copyright_protection_tr_t0, copyright_protection, theater_ratio, 0, 0.05).
% narrative_ontology:measurement(copyright_protection_tr_t5, copyright_protection, theater_ratio, 5, 0.08).
% narrative_ontology:measurement(copyright_protection_tr_t10, copyright_protection, theater_ratio, 10, 0.10).
%
% narrative_ontology:measurement(copyright_protection_ex_t0, copyright_protection, base_extractiveness, 0, 0.35).
% narrative_ontology:measurement(copyright_protection_ex_t5, copyright_protection, base_extractiveness, 5, 0.38).
% narrative_ontology:measurement(copyright_protection_ex_t10, copyright_protection, base_extractiveness, 10, 0.40).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(copyright_protection, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed; the structural derivation from beneficiary/victim
% status and exit options accurately models the directionality for each agent.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */