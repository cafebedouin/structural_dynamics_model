% ============================================================================
% CONSTRAINT STORY: iran_hijab_law
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_iran_hijab_law, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: iran_hijab_law
 *   human_readable: Mandatory Hijab Law in Iran
 *   domain: political/social/religious
 *
 * SUMMARY:
 *   This constraint represents the mandatory hijab law in the Islamic Republic
 *   of Iran, enforced by the state's "morality police" (Gasht-e Ershad).
 *   The law mandates specific dress codes for women in public, serving as a
 *   core pillar of the theocratic regime's ideological control. Its violent
 *   enforcement, culminating in events like the 2022 death of Mahsa Amini
 *   in custody, has made it a focal point for widespread civil unrest and
 *   protests under the slogan "Woman, Life, Freedom."
 *
 * KEY AGENTS (by structural relationship):
 *   - Iranian women & dissident citizens: Primary target (powerless/trapped) — bear the extraction of autonomy, physical harm, and death.
 *   - The Iranian clerical regime & IRGC: Primary beneficiary (institutional/arbitrage) — uses the law to consolidate power and enforce ideology.
 *   - Organized protestors: Secondary target (organized/constrained) - actively resist the constraint, altering its perceived extractiveness.
 *   - Analytical observer: External analyst — sees the full structure of coercion and claimed coordination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iran_hijab_law, 0.75).
domain_priors:suppression_score(iran_hijab_law, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(iran_hijab_law, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iran_hijab_law, extractiveness, 0.75).
narrative_ontology:constraint_metric(iran_hijab_law, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(iran_hijab_law, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(iran_hijab_law, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(iran_hijab_law). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(iran_hijab_law, iranian_clerical_regime).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(iran_hijab_law, iranian_women_and_dissidents).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present)
%   Snare:        victim required; beneficiary optional (victim is present)

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
% An Iranian woman subject to the law. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%   χ = 0.75 * 1.42 * 1.0 = 1.065. Meets Snare criteria (χ >= 0.66, ε >= 0.46, supp >= 0.60).
constraint_indexing:constraint_classification(iran_hijab_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The clerical regime. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
%   χ = 0.75 * -0.12 * 1.0 = -0.09. The constraint appears as a beneficial coordination tool.
constraint_indexing:constraint_classification(iran_hijab_law, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% The observer sees both the regime's claimed coordination function and the
% severe asymmetric extraction. All three gates for Tangled Rope are met:
% has_coordination_function, has_asymmetric_extraction, requires_active_enforcement.
% χ = 0.75 * 1.15 * 1.2 (global scope) = 1.035.
constraint_indexing:constraint_classification(iran_hijab_law, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ORGANIZED RESISTANCE (TANGLED ROPE)
% Protestors acting as a collective. Their organization provides a degree of
% power and constrained exit (they can evade, but not leave the system).
% Engine derives a moderate d: victim + constrained exit -> d ~ 0.65 -> f(d) ~ 1.0.
% χ = 0.75 * 1.0 * 1.0 = 0.75. This is above the snare threshold, but the
% recognition of a coordinating opponent (the regime) leads to a Tangled Rope view.
constraint_indexing:constraint_classification(iran_hijab_law, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iran_hijab_law_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(iran_hijab_law, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iran_hijab_law, rope, context(agent_power(institutional), _, _, _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(iran_hijab_law, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_met) :-
    narrative_ontology:constraint_beneficiary(iran_hijab_law, _), % provides coordination function
    narrative_ontology:constraint_victim(iran_hijab_law, _), % provides asymmetric extraction
    domain_priors:requires_active_enforcement(iran_hijab_law).

test(high_extraction_and_suppression_metrics) :-
    narrative_ontology:constraint_metric(iran_hijab_law, extractiveness, E),
    narrative_ontology:constraint_metric(iran_hijab_law, suppression_requirement, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(iran_hijab_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): Extremely high. The constraint extracts fundamental human autonomy, freedom of expression, and bodily integrity. The cost of non-compliance can be arrest, violence, or death.
 *   - Suppression Score (S=0.80): Very high. The state uses its full coercive apparatus (morality police, courts, IRGC) to suppress any alternative. There is no legal or safe way to opt out.
 *   - Theater Ratio (T=0.20): Low. While the enforcement is a form of political theater to project power, the consequences are severe and functional from the regime's perspective. It is not an empty ritual.
 *
 * PERSPECTIVAL GAP:
 *   The gap is maximal. The regime (Beneficiary) perceives the law as a foundational 'Rope' for maintaining ideological purity and social order, essential for its vision of society. For them, χ is negative; the law provides structure.
 *   Conversely, a woman targeted by the law (Victim) experiences it as a pure 'Snare'. It offers no coordination benefit to her, only a constant threat of violence and a deprivation of freedom. For her, χ is extremely high.
 *   The Analytical observer sees both sides: the regime's use of the law as a coercive "coordination" mechanism and the immense, asymmetric cost imposed on the population. This dual nature is the definition of a 'Tangled Rope'.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `iranian_clerical_regime`. The law is a tool to enforce ideological conformity, which is a primary source of the regime's legitimacy and power. It benefits directly from the constraint's existence.
 *   - Victim: `iranian_women_and_dissidents`. They bear 100% of the cost through loss of freedom, risk of violence, and social penalties. The directionality is unambiguously extractive from their perspective.
 *   These declarations correctly feed the d-derivation chain, producing the massive gap in perceived effective extraction (χ) between the two groups.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two key errors. First, it doesn't mislabel the constraint as a pure Snare from a systemic perspective, because that would ignore the functional role it plays in the regime's power structure (its "coordination" aspect). Second, it avoids the regime's own framing of it as a pure Rope, which would erase the immense human cost and coercion. The 'Tangled Rope' classification correctly identifies it as a system of coercive control with a beneficiary group that frames it as a coordination good. [RESOLVED MANDATROPHY]
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_iran_hijab_law,
    'Is the mandatory hijab a critical pillar for regime stability, or a legacy policy that has become a net liability by fueling opposition?',
    'Analysis of internal regime communications, polling data (if available), and the long-term impact of the "Woman, Life, Freedom" movement on political stability.',
    'If critical pillar -> Regime will escalate violence to maintain it (Snare intensifies). If liability -> Regime may tactically retreat or substitute with other controls (Piton-ization or replacement).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(iran_hijab_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.75 > 0.46), requiring temporal data.
% The timeline models the period from the post-Iran-Iraq war consolidation (~1990s)
% to the Mahsa Amini protests (~2022), showing intensification of extraction.

% Theater ratio over time:
narrative_ontology:measurement(iran_hijab_law_tr_t0, iran_hijab_law, theater_ratio, 0, 0.10).
narrative_ontology:measurement(iran_hijab_law_tr_t5, iran_hijab_law, theater_ratio, 5, 0.15).
narrative_ontology:measurement(iran_hijab_law_tr_t10, iran_hijab_law, theater_ratio, 10, 0.20).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(iran_hijab_law_ex_t0, iran_hijab_law, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(iran_hijab_law_ex_t5, iran_hijab_law, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(iran_hijab_law_ex_t10, iran_hijab_law, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% From the regime's perspective, this is a tool to enforce ideological norms.
narrative_ontology:coordination_type(iran_hijab_law, enforcement_mechanism).

% Network relationships (structural influence edges)
% The hijab law is structurally coupled with broader systems of political
% and social repression in Iran. A failure in one domain risks the others.
narrative_ontology:affects_constraint(iran_hijab_law, iran_political_suppression).
narrative_ontology:affects_constraint(iran_hijab_law, iran_media_censorship).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */