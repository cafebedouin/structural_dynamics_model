% ============================================================================
% CONSTRAINT STORY: nsl_hk
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-25
% ============================================================================

:- module(constraint_nsl_hk, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nsl_hk
 *   human_readable: Hong Kong National Security Law (NSL)
 *   domain: political/legal
 *
 * SUMMARY:
 *   The National Security Law (NSL) was imposed on Hong Kong by Beijing in 2020.
 *   It criminalizes secession, subversion, terrorism, and collusion with
 *   foreign forces, with broad and vaguely defined terms. The law has been used
 *   to arrest and imprison pro-democracy advocates, journalists, and politicians,
 *   effectively dismantling political opposition and silencing dissent.
 *
 * KEY AGENTS (by structural relationship):
 *   - Jimmy Lai & HK Dissidents: Primary target (powerless/trapped) — bears the full extractive force of the law through imprisonment, asset seizure, and suppression of rights.
 *   - Beijing-aligned Authorities: Primary beneficiary (institutional/arbitrage) — uses the law to consolidate political control, eliminate opposition, and enforce ideological alignment with the mainland.
 *   - Global Businesses in HK: Secondary actor (powerful/constrained) — navigates the new legal landscape, facing increased political risk but also a more controlled (less protest-prone) operating environment.
 *   - International Observers: Analytical observer — views the law as a tool of authoritarian consolidation, documenting its impact on civil liberties.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_hk, 0.85). % Extracts political freedom, assets, and civil liberties.
domain_priors:suppression_score(nsl_hk, 0.95).   % Explicitly designed to eliminate political alternatives. Structural property (raw, unscaled).
domain_priors:theater_ratio(nsl_hk, 0.20).       % Primarily functional enforcement, not performative. Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_hk, extractiveness, 0.85).
narrative_ontology:constraint_metric(nsl_hk, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(nsl_hk, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(nsl_hk, snare).
narrative_ontology:human_readable(nsl_hk, "Hong Kong National Security Law (NSL)").

% --- Binary flags ---
domain_priors:requires_active_enforcement(nsl_hk). % Enforced by dedicated police units and designated judges.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(nsl_hk, beijing_aligned_authorities).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(nsl_hk, hong_kong_dissidents).
narrative_ontology:constraint_victim(nsl_hk, global_businesses_in_hk).

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
constraint_indexing:constraint_classification(nsl_hk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(nsl_hk, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(nsl_hk, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% Global businesses are powerful but constrained by the law. They are victims
% of increased political risk and legal ambiguity.
% Engine derives d from victim membership + constrained exit.
constraint_indexing:constraint_classification(nsl_hk, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_hk_tests).

test(perspectival_gap_is_snare_vs_rope) :-
    % Verify the core perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(nsl_hk, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nsl_hk, rope, context(agent_power(institutional), _, _, _)).

test(analytical_view_is_snare) :-
    constraint_indexing:constraint_classification(nsl_hk, snare, context(agent_power(analytical), _, _, _)).

test(snare_thresholds_met) :-
    % Verify the base metrics meet the canonical Snare definition.
    domain_priors:base_extractiveness(nsl_hk, E), E >= 0.46,
    domain_priors:suppression_score(nsl_hk, S), S >= 0.60.

:- end_tests(nsl_hk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.85): The law directly extracts fundamental civil
 *     liberties (speech, assembly), economic assets (e.g., Jimmy Lai's), and
 *     political autonomy from its targets. The cost imposed is extremely high.
 *   - Suppression Score (0.95): The NSL's primary function is to eliminate
 *     political alternatives to the Beijing-backed government. It criminalizes
 *     dissent and has been used to dissolve opposition parties and media outlets.
 *     Suppression is a raw structural property of the law; it is not scaled.
 *
 * PERSPECTIVAL GAP:
 *   The gap is maximal. For the Beijing-aligned authorities (institutional/arbitrage),
 *   the NSL is a 'Rope'—a powerful tool for enforcing political order and
 *   coordinating state power, yielding a negative effective extraction (χ).
 *   For Jimmy Lai and other dissidents (powerless/trapped), it is a 'Snare'—a
 *   mechanism of pure extraction with no coordinating benefit, designed to entrap
 *   and neutralize them, yielding a very high χ.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. The constraint is designed and enforced
 *   by one group (`beijing_aligned_authorities`) to extract rights and resources
 *   from another (`hong_kong_dissidents`). The beneficiary/victim declarations
 *   reflect this clear structural asymmetry. The engine correctly derives a
 *   very low directionality `d` for the beneficiary (leading to a Rope classification)
 *   and a very high `d` for the victim (leading to a Snare classification).
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   The high suppression score (0.95) and the clear victim/beneficiary structure
 *   prevent this constraint from being mislabeled as a form of coordination
 *   (like a Tangled Rope). While proponents claim it coordinates "social stability,"
 *   its mechanism is overwhelmingly coercive suppression rather than providing
 *   a shared good. The analytical perspective correctly identifies it as a Snare,
 *   piercing the veil of coordination rhetoric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_nsl_hk,
    'Will the application of the NSL broaden to encompass purely economic or commercial disputes, or will it remain focused on political targets?',
    'Observation of prosecution patterns over the next 5-10 years, particularly cases involving foreign firms without clear political dimensions.',
    'If it broadens, it becomes a Snare for a much larger group (global_businesses_in_hk); if it remains political, its primary target group remains stable.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(nsl_hk, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the law's intensification from its introduction in 2020.
% The initial phase had more theater ("restoring stability") and the full
% extractive potential was not yet realized. It quickly ramped up.
% Base extractiveness is high (>0.46), so temporal data is required.

% Theater ratio over time (declined as overt enforcement replaced rhetoric):
narrative_ontology:measurement(nsl_hk_tr_t0, nsl_hk, theater_ratio, 0, 0.40).
narrative_ontology:measurement(nsl_hk_tr_t5, nsl_hk, theater_ratio, 5, 0.20).
narrative_ontology:measurement(nsl_hk_tr_t10, nsl_hk, theater_ratio, 10, 0.20).

% Extraction over time (increased as arrests and prosecutions became common):
narrative_ontology:measurement(nsl_hk_ex_t0, nsl_hk, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(nsl_hk_ex_t5, nsl_hk, base_extractiveness, 5, 0.85).
narrative_ontology:measurement(nsl_hk_ex_t10, nsl_hk, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The NSL is a legal framework used for state control.
narrative_ontology:coordination_type(nsl_hk, enforcement_mechanism).

% Network relationships (structural influence edges)
% The NSL is a tool within a broader geopolitical strategy.
narrative_ontology:affects_constraint(nsl_hk, china_sovereignty_consolidation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% power dynamics and perspectival gaps.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */