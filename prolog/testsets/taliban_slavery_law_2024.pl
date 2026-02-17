% ============================================================================
% CONSTRAINT STORY: taliban_slavery_law_2024
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_taliban_slavery_law_2024, []).

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
 *   constraint_id: taliban_slavery_law_2024
 *   human_readable: Taliban Criminal Code Re-legalizing Slavery
 *   domain: political/legal
 *
 * SUMMARY:
 *   Following their return to power, the Taliban regime in Afghanistan has
 *   reinstated a version of their criminal code that effectively re-legalizes
 *   slavery and slave-like practices. The law primarily targets women,
 *   ethnic and religious minorities (such as Hazaras), and political
 *   opponents, creating a formal legal mechanism for their total subjugation
 *   and exploitation.
 *
 * KEY AGENTS (by structural relationship):
 *   - Afghan women and minorities: Primary target (powerless/trapped) — bear total extraction of autonomy and labor.
 *   - Taliban regime and fighters: Primary beneficiary (institutional/arbitrage) — benefit from the extracted labor, control, and ideological reinforcement.
 *   - International human rights organizations: Analytical observer — identifies the structure as a system of coercive extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taliban_slavery_law_2024, 0.90).
domain_priors:suppression_score(taliban_slavery_law_2024, 0.95).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(taliban_slavery_law_2024, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taliban_slavery_law_2024, extractiveness, 0.90).
narrative_ontology:constraint_metric(taliban_slavery_law_2024, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(taliban_slavery_law_2024, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(taliban_slavery_law_2024, snare).
narrative_ontology:human_readable(taliban_slavery_law_2024, "Taliban Criminal Code Re-legalizing Slavery").

% --- Binary flags ---
domain_priors:requires_active_enforcement(taliban_slavery_law_2024).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(taliban_slavery_law_2024, taliban_regime_and_fighters).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(taliban_slavery_law_2024, afghan_women_and_minorities).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%   χ = 0.90 * f(0.95) * σ(national) ≈ 0.90 * 1.42 * 1.0 ≈ 1.28 >> 0.66 (Snare)
constraint_indexing:constraint_classification(taliban_slavery_law_2024, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
%   χ = 0.90 * f(0.05) * σ(national) ≈ 0.90 * -0.12 * 1.0 ≈ -0.11 (Rope)
constraint_indexing:constraint_classification(taliban_slavery_law_2024, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% Default analytical context.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
%   χ = 0.90 * f(0.72) * σ(global) ≈ 0.90 * 1.15 * 1.2 ≈ 1.24 >> 0.66 (Snare)
constraint_indexing:constraint_classification(taliban_slavery_law_2024, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taliban_slavery_law_2024_tests).

test(perspectival_gap_snare_vs_rope) :-
    % Verify the core perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(taliban_slavery_law_2024, snare,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(taliban_slavery_law_2024, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_snare) :-
    constraint_indexing:constraint_classification(taliban_slavery_law_2024, snare,
        context(agent_power(analytical), _, _, _)).

test(snare_threshold_validation) :-
    % Verify the constraint meets the numerical criteria for a Snare.
    domain_priors:base_extractiveness(taliban_slavery_law_2024, E), E >= 0.46,
    domain_priors:suppression_score(taliban_slavery_law_2024, S), S >= 0.60.

:- end_tests(taliban_slavery_law_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.90): The constraint allows for the total
 *     appropriation of an individual's labor, autonomy, and physical being.
 *     Slavery is a near-total form of extraction, justifying a very high ε.
 *   - Suppression Score (S=0.95): The Taliban regime actively and violently
 *     suppresses all alternatives, including the former Afghan constitution,
 *     international human rights conventions, and any form of organized
 *     dissent. Exit is effectively impossible for the target population.
 *   - Theater Ratio (TR=0.10): This is not a performative law. It is a
 *     functional component of the regime's ideology and system of control,
 *     intended for active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The gap between Snare and Rope is profound and defines the conflict.
 *   - For the victims (powerless/trapped), the law is a Snare: a mechanism
 *     of pure, coercive extraction from which there is no escape. The high
 *     base extraction is amplified by their lack of power, resulting in a
 *     very high effective extraction (χ).
 *   - For the beneficiaries (institutional/arbitrage), the law is a Rope: a
 *     coordination mechanism that reinforces their social and political order,
 *     provides economic benefits (free labor), and solidifies their power.
 *     From their position, the system is not extractive but generative.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous.
 *   - Beneficiaries: `taliban_regime_and_fighters` directly benefit through
 *     the acquisition of property, labor, and social status conferred by the
 *     law. This maps them to low directionality (d), yielding a negative χ.
 *   - Victims: `afghan_women_and_minorities` are the designated targets of
 *     the law, bearing the full cost of its extractive potential. This maps
 *     them to high directionality (d), yielding a high χ.
 *   The model correctly derives this from the explicit beneficiary/victim
 *   declarations combined with their respective exit options (arbitrage vs. trapped).
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   This constraint is a clear example of what the Deferential Realism
 *   framework is designed to prevent: the mischaracterization of pure
 *   extraction as coordination. An analysis that ignores directionality
 *   or perspectival difference might try to frame this as a "cultural norm"
 *   or an "internal organizing principle" (a Rope). The high suppression
 *   score, high base extractiveness, and the stark victim/beneficiary
 *   dichotomy force the classification of Snare from any non-beneficiary
 *   perspective, preventing the normalization of a violent, extractive system.
 *   The framework correctly identifies this as a Snare, resolving the potential
 *   mandatrophy of labeling it otherwise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_taliban_slavery_law_2024,
    'Is the slavery provision a universally enforced component of the legal code, or a selectively applied tool for political terror against specific groups?',
    'Systematic, on-the-ground reporting of legal cases, tracking both accusations and enforcement patterns across different regions and ethnic groups.',
    'If universal, it represents a stable, albeit brutal, institutional structure. If selective, it functions more as an arbitrary terror mechanism, which could imply even higher suppression but potentially lower system-wide base extraction.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(taliban_slavery_law_2024, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the shift from a legally-prohibited practice (high
% theater, low extraction) to a legally-sanctioned one (low theater, high
% extraction) following the regime change.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(tsl_tr_t0, taliban_slavery_law_2024, theater_ratio, 0, 0.80).
narrative_ontology:measurement(tsl_tr_t5, taliban_slavery_law_2024, theater_ratio, 5, 0.30).
narrative_ontology:measurement(tsl_tr_t10, taliban_slavery_law_2024, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(tsl_ex_t0, taliban_slavery_law_2024, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(tsl_ex_t5, taliban_slavery_law_2024, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(tsl_ex_t10, taliban_slavery_law_2024, base_extractiveness, 10, 0.90).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: This law functions as a brutal enforcement mechanism
% for a specific social and political hierarchy.
narrative_ontology:coordination_type(taliban_slavery_law_2024, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary. The standard derivation from beneficiary/victim
% declarations and exit options correctly captures the structural dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */