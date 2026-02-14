% ============================================================================
% CONSTRAINT STORY: ice_raids_minnesota_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_ice_raids_minnesota_2026, []).

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
 *   constraint_id: ice_raids_minnesota_2026
 *   human_readable: "Large-Scale Pre-Announced ICE Raids as a Deportation Mechanism"
 *   domain: political/social
 *
 * SUMMARY:
 *   This constraint models the policy of using large-scale, often publicly
 *   announced, immigration enforcement raids by U.S. Immigration and Customs
 *   Enforcement (ICE). The policy serves to detain and deport undocumented
 *   immigrants while also functioning as a political signaling mechanism.
 *   The high-profile nature of the raids creates a strong coercive effect on
 *   immigrant communities, suppresses alternatives, and benefits political actors
 *   and private industries involved in detention.
 *
 * KEY AGENTS (by structural relationship):
 *   - Undocumented Immigrant Communities: Primary target (powerless/trapped) — bears total extraction of freedom, economic potential, and community ties.
 *   - Immigration Enforcement Complex: Primary beneficiary (institutional/arbitrage) — includes federal agencies (ICE/DHS), private detention contractors, and political actors who gain budget, profit, or political capital.
 *   - Sanctuary City Governments: Secondary actor (institutional/constrained) — experiences the policy as a disruptive external force, bearing costs of community disruption but unable to nullify the federal mandate.
 *   - Analytical Observer: Analytical observer — sees the full structure of extraction, coercion, and political theater.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ice_raids_minnesota_2026, 0.85).
domain_priors:suppression_score(ice_raids_minnesota_2026, 0.90).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(ice_raids_minnesota_2026, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ice_raids_minnesota_2026, extractiveness, 0.85).
narrative_ontology:constraint_metric(ice_raids_minnesota_2026, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(ice_raids_minnesota_2026, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ice_raids_minnesota_2026, snare).

% --- Binary flags ---
domain_priors:requires_active_enforcement(ice_raids_minnesota_2026).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ice_raids_minnesota_2026, immigration_enforcement_complex).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(ice_raids_minnesota_2026, undocumented_immigrant_communities).
narrative_ontology:constraint_victim(ice_raids_minnesota_2026, sanctuary_city_governments).

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
constraint_indexing:constraint_classification(ice_raids_minnesota_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(ice_raids_minnesota_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% Default analytical context (civilizational/analytical/global).
% The observer sees the high base extraction and suppression.
constraint_indexing:constraint_classification(ice_raids_minnesota_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% Perspective 4: Sanctuary City Government (Snare)
% This institutional actor is a victim, not a beneficiary. Its exit is constrained
% by federal law. The engine derives a higher d than for the beneficiary.
% victim membership + constrained exit → higher d → positive χ
constraint_indexing:constraint_classification(ice_raids_minnesota_2026, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ice_raids_minnesota_2026_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(ice_raids_minnesota_2026, snare, context(agent_power(powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(ice_raids_minnesota_2026, rope, context(agent_power(institutional), _, arbitrage, _)).

test(inter_institutional_gap) :-
    % Verify that two institutional actors with different exit options see the constraint differently.
    constraint_indexing:classify(ice_raids_minnesota_2026, context(agent_power(institutional), time_horizon(generational), exit_options(arbitrage), spatial_scope(national)), rope, Chi_Beneficiary, _),
    constraint_indexing:classify(ice_raids_minnesota_2026, context(agent_power(institutional), time_horizon(generational), exit_options(constrained), spatial_scope(national)), snare, Chi_Victim, _),
    Chi_Beneficiary < 0,
    Chi_Victim > 0.66.

test(snare_threshold_validation) :-
    domain_priors:base_extractiveness(ice_raids_minnesota_2026, E),
    domain_priors:suppression_score(ice_raids_minnesota_2026, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(ice_raids_minnesota_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.85): Extremely high. The constraint removes an individual's freedom, economic future, and community ties, which constitutes near-total extraction from their perspective.
 *   - Suppression Score (0.90): Extremely high. For an undocumented person targeted in a raid, there are virtually no legal alternatives or avenues of escape. The system is designed to foreclose options.
 *   - Theater Ratio (0.40): Significant but not dominant. The raids are performative and designed for media consumption to signal a political stance, but they also result in real deportations, so they are not purely theatrical (which would imply a Piton).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the undocumented immigrant (powerless/trapped), the constraint is a Snare—a coercive trap with devastating consequences. For the institutional beneficiary (the enforcement complex), it is a Rope—a useful, low-cost tool for achieving policy, political, and financial goals. This disagreement is not a matter of opinion but a direct consequence of their differing structural positions, which the directionality 'd' captures.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `immigration_enforcement_complex`. This group benefits directly through increased budgets (ICE/DHS), profits (private prisons), and political capital (politicians). Their `arbitrage` exit option and beneficiary status derive a low 'd', resulting in a negative effective extraction (χ).
 *   - Victim: `undocumented_immigrant_communities`. They bear the full cost. Their `trapped` status and victim declaration derive a high 'd', maximizing effective extraction.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model includes a perspective for a 'Sanctuary City' government. Unlike the federal enforcement agency, this institutional actor is also a victim of the policy. Its exit is 'constrained'—it can resist but not override federal action. This structural difference, captured by its victim status and exit options, results in the engine classifying the constraint as a Snare from its perspective, just as for the primary target. This demonstrates how two institutional actors can have fundamentally different experiences of the same national policy.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY]
 *   This classification correctly identifies the policy as a Snare, not a Tangled Rope. While a defender might claim a coordination function ("enforcing laws"), the overwhelmingly high and asymmetric extraction (ε=0.85) and suppression of alternatives (suppression=0.90) reveal that any coordinative aspect is incidental to the primary function of extraction and coercion. The analytical perspective correctly resolves to Snare, preventing the mislabeling of pure extraction as a flawed but necessary coordination mechanism.
 *   The "Dynamic Coalition" extension is relevant here: while individual targets are 'powerless', as a group they exceed the critical mass threshold, granting them the potential to act as an 'organized' bloc through community defense networks, which could slightly alter the classification by lowering the effective power differential.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ice_raids_2026,
    'Is the primary driver of large-scale ICE raids the enforcement of immigration law for national security, or is it a performative act of political signaling that primarily benefits private contractors and specific political actors?',
    'A quantitative analysis correlating raid announcements with political polling cycles, media coverage value, and the stock performance/revenue of major private detention contractors.',
    'If driven by security, the constraint might be better modeled as a harsh Tangled Rope. If driven by political/economic incentives, it is correctly modeled as a pure Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ice_raids_minnesota_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This policy intensified over the preceding decade, showing a clear pattern of
% extraction accumulation and increased theatricality.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(ice_raids_tr_t0, ice_raids_minnesota_2026, theater_ratio, 0, 0.20).
narrative_ontology:measurement(ice_raids_tr_t5, ice_raids_minnesota_2026, theater_ratio, 5, 0.35).
narrative_ontology:measurement(ice_raids_tr_t10, ice_raids_minnesota_2026, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(ice_raids_ex_t0, ice_raids_minnesota_2026, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(ice_raids_ex_t5, ice_raids_minnesota_2026, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(ice_raids_ex_t10, ice_raids_minnesota_2026, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The *claimed* function is law enforcement.
narrative_ontology:coordination_type(ice_raids_minnesota_2026, enforcement_mechanism).

% Network relationships (structural influence edges)
% This policy directly impacts local economies and social cohesion.
narrative_ontology:affects_constraint(ice_raids_minnesota_2026, local_labor_market_stability).
narrative_ontology:affects_constraint(ice_raids_minnesota_2026, public_trust_in_law_enforcement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% directionality for all key agents, including the inter-institutional dynamic
% between federal enforcers and local governments.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */